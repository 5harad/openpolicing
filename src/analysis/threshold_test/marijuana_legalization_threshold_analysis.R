# rm(list=ls())

code_path = "~/openpolicing/"
setwd(code_path)

# First, source widely used utility scripts. 
# more specific analysis scripts are sourced only when they are used. 
# for additional explanation of script functions, see README in relevant directory. 
source('src/util/libraries.R') # libraries used throughout the analysis
source('src/util/constants_and_paths.R') # defines constants and paths. YOU WILL NEED TO EDIT THIS TO RUN THE CODE YOURSELF. 
source("src/processing/aggregation.R") # computes and loads aggregate data (working with stop-level data takes too long)
source("src/processing/processing_driver.R") # helper code for processing raw state data
source("src/analysis/stratify_by_covariates.R") # runs regressions and makes county scatterplots
source("src/util/util_functions.R") # utility functions. 
source('src/util/theme_for_plots.R')

library(rstan)

# This script produces time series plots 5a, 5b, 5c in the paper,
# representing the effect of marijuana legalization in Colorado and Washington.
# We have separate functions to aggregate all the marijuana data + do the analysis 
# since aggregating the data takes a while but analysis is very fast on aggregate data. 

# small helper method to get legalization date for a state. 
# the two legalization dates for CO + WA are essentially the same (2012-12-10 vs 2012-12-09). 
# for control states, which of course have no legalization date, we return Washington's by default. 
get_legalization_date_for_state = function(state){return(ifelse(state == 'CO', COLORADO_LEGALIZATION_DATE, WASHINGTON_LEGALIZATION_DATE))}

# Helper function to group the data by quarter so we can plot time series. 
aggregate_marijuana_data_threshold <- function(df){
  message(sprintf("Generating aggregate marijuana data (grouped by date and race) for %s", df$state[1]))
  
  # First annotate with excluded search types. 
  # search types to exclude because they have nothing to do with drug searches, so would not expect to change. 
  excluded_search_types = c('Incident to Arrest', 'Inventory', 'Protective Frisk', 
                            'Stop and Frisk', 'Warrant', 'Other',  'Impound',
                            'Reasonable Suspicion', 'Consent Search - Reasonable Suspicion',# we exclude reasonable suspicion searches because can only be conducted for officer safety
                            'Exigent Circumstances', 'Parole/Probation', 
                            'Pat Down/Frisk', 'Vehicle Inventory', 'Search Warrant')
  excluded_search_types = tolower(excluded_search_types)  # avoid capitalization issues.
  
  # small helper function to exclude a search if any of the excluded search types are in the search. 
  # if search type is NA, we do not exclude. 
  search_is_excluded = function(x, excluded_search_types){return(grepl(paste0(excluded_search_types, collapse = '|'), tolower(x)))}
  
  df = df %>% 
    mutate(search_eligible = !search_is_excluded(search_type, excluded_search_types), 
           eligible_search_conducted = search_eligible & search_conducted)
  
  # print these out to make sure the proper search types are excluded / included. 
  
  all_search_types_included = df %>% filter(search_eligible, search_conducted) %>% 
    group_by(search_type) %>% 
    summarise(included_search_type = n()) %>% 
    arrange(-included_search_type) %>% 
    head(50) %>% 
    as.data.frame()
  all_search_types_excluded = df %>% 
    filter(!search_eligible, search_conducted) %>% 
    group_by(search_type) %>% 
    summarise(excluded_search_type = n()) %>% 
    arrange(-excluded_search_type) %>% 
    head(50) %>% 
    as.data.frame()
  message("Search types included")
  print(all_search_types_included)
  message("Search types excluded")
  print(all_search_types_excluded) 
  
  df = df %>%
    mutate(legal = stop_date >= get_legalization_date_for_state(state)) %>%
    group_by(state, county_name, driver_race, legal) %>%
    summarize(num_stops = n(),
              num_searches = sum(eligible_search_conducted, na.rm=T),
              num_hits = sum(eligible_search_conducted & contraband_found, na.rm=T)) %>% 
    as.data.frame()
  return(df)
}


run_mcmc_threshold <- function(obs, full_model_path, full_output_path, iter = 5000, warmup = NULL, chains = 5, adapt_delta = 0.9, max_treedepth = 12, 
                     sample_from_prior = FALSE, verbose = FALSE) {
  # if there's no warmup specification, use half the total sampling period by default. 
  if (is.null(warmup)) {
    if (sample_from_prior) {
      warmup = 0
    } else {
      warmup = ceiling(iter/2) 
    }
  }
  
  # check model identifiability
  r <- length(unique(obs$driver_race))
  d <- length(unique(obs$location_variable))
  if (! (r >= 3 & d >= 5)) {
    stop('Not enough departments to constrain estimates')
  }
  
  # Package data for Stan
  stan_data = with(obs, list(
    N = nrow(obs),
    D = length(unique(location_variable)),
    R = length(unique(driver_race)),
    d = as.integer(location_variable),
    r = as.integer(driver_race),
    legal = as.integer(legal),
    n = num_stops,
    s = num_searches,
    h = num_hits
  ))
  
  # set up parameter initialization
  initializer <- function(num_obs, num_races, num_locations) {	
    
    # force immediate evaluation of arguments
    force(num_obs); force(num_races); force(num_locations);
    
    function() {
      list(sigma_t        = runif(num_races, 0.05, 0.25),
           t_r            = runif(num_races, -3, -1),  
           t_i_raw        = runif(num_obs, -0.75, 0.75),
           phi_r          = runif(num_races, -3.75, -3.25),
           lambda_r       = runif(num_races, 1.5, 2.5), 
           phi_d_raw      = runif(num_locations-1, -0.25, 0.25),
           lambda_d_raw   = runif(num_locations-1, -0.25, 0.25),
           t_legal_r      = runif(num_races, -2, 2),
           phi_legal_r      = runif(num_races, -2, 2),
           lambda_legal_r      = runif(num_races, -2, 2)
      )
    }
  }
  
  # fit the model
  init_fn <- initializer(stan_data$N, stan_data$R, stan_data$D)
  
  fit <- rstan::stan(full_model_path, data=stan_data, iter=iter, init=init_fn, chains=chains, cores=chains, refresh = 50, warmup = warmup, 
                     control = list(adapt_delta = adapt_delta, max_treedepth = max_treedepth, adapt_engaged = !sample_from_prior, 
                                    adapt_init_buffer = 75, adapt_term_buffer = 50, adapt_window = 25), 
                     verbose = verbose, diagnostic_file = paste0(full_output_path, '_diag.txt'))
  post = rstan::extract(fit)
  save(file=full_output_path, obs, post, fit)
  list(obs=obs, post=post, fit=fit)
}

run_threshold_test_marijuana <- function(state_prefix, min_stops=1000, adapt_delta=0.9, max_treedepth=12){
  
  # Check that inputs are valid
  location_type='county_name'
  stopifnot(toupper(state_prefix) %in% c('CO', 'WA'))
  
  # Generate model and output paths. 
  full_model_path = sprintf('%s/model_%s.stan', threshold_test_code_folder, 'marijuana')
  full_output_path = sprintf('%s/%s_%s.RData', threshold_test_output_folder,  state_prefix, 'marijuana')
  stopifnot(file.exists(sprintf(threshold_test_output_folder)))
  print(sprintf("Running model for %s and saving results to %s", toupper(state_prefix), full_output_path))
  
  # load state data
  df <- read_state(state_prefix, perform_data_checks = F) %>%
    aggregate_marijuana_data_threshold()
  
  # create a new column that uses the location variable so it has a standard name 
  df$location_variable = df[,location_type]
  
  # threshold counties with at least "min_stops" or a maximum number of 100 counties and more than 0 searches. 
  locations <- df %>% 
    filter(! is.na(location_variable)) %>%
    group_by(location_variable) %>% 
    summarise(num_stops = sum(num_stops),
              num_searches = sum(num_searches)) %>%
    filter((num_stops >= min_stops) & (num_searches > 0)) %>%
    arrange(desc(num_stops)) %>%
    top_n(100, num_stops)
  
  # create data frame with aggregate search / hit rates for each county-race pair
  # filter out stops with 0 searches as hit rate will be NA which Stan cannot handle
  stops <- df %>%
    filter(location_variable %in% locations$location_variable) %>%
    mutate(location_variable = factor(location_variable, levels=locations$location_variable)) %>%
    filter(num_searches != 0) %>%
    as.data.frame()
  
  print(paste0('Running threshold test on ', toupper(state_prefix), ': ', length(unique(stops$location_variable)), ' locations, ', length(unique(stops$driver_race)), ' race groups, ', min_stops, ' min nr stops.'))
  
  output = run_mcmc_threshold(stops, full_model_path, full_output_path, iter=5000, chains=5, adapt_delta=adapt_delta, max_treedepth=max_treedepth)
  message("Successfully completed threshold test.")
}

threshold_cis = function(obs, post,
                         groups = 'driver_race',
                         weights = NULL,
                         probs = c(0.025,0.5,0.975)) {
  if (is.null(weights)) {
    weights = obs %>% group_by(location_variable) %>%
      mutate(w = sum(num_stops)) %>%
      with(w)
  }
  
  obs = obs %>% mutate(idx = 1:nrow(.))
  
  t = t(post$t_i)
  
  
  obs %>% group_by_(.dots = groups) %>% do(
    as.data.frame(t(quantile(colSums(weights[.$idx] * t[.$idx,])/sum(weights[.$idx]), probs = probs)))
  ) %>% as.data.frame()
}


threshold_difference_cis = function(obs, post,
                                    groups = 'driver_race',
                                    base_group = 'White',
                                    weights = NULL,
                                    probs = c(0.025,0.5,0.975)) {
  if (is.null(weights)) {
    weights = obs %>% group_by(location_variable) %>%
      mutate(w = sum(num_stops)) %>%
      with(w)
  }
  
  join_vars = setdiff(names(obs)[sapply(obs, function(x) !is.numeric(x))], groups)
  matches = sapply(1:nrow(obs), function(i) all(obs[i,groups] == base_group))
  
  obs = obs %>% mutate(idx = 1:nrow(.))
  idx = obs %>% left_join(obs[matches,], by = join_vars) %>%
    with(idx.y)
  
  t_diff = t(post$t_i - post$t_i[,idx])
  
  
  obs[!is.na(idx),] %>% group_by_(.dots = groups) %>% do(
    as.data.frame(t(quantile(colSums(weights[.$idx] * t_diff[.$idx,])/sum(weights[.$idx]), probs = probs)))
  ) %>% as.data.frame()
}

marijuana_threshold_table = function(filename) {
  load(paste0(threshold_test_output_folder, 'CO_marijuana.RData'), verbose = T)
  co = threshold_cis(obs, post,
                     groups = c('legal', 'driver_race'),
                     weights = obs %>% group_by(location_variable, legal) %>%
                       mutate(w=sum(num_stops)) %>%
                       with(w),
                     probs = 0.5) %>%
    mutate(state='CO')
  load(paste0(threshold_test_output_folder, 'WA_marijuana.RData'), verbose = T)
  wa = threshold_cis(obs, post,
                     groups = c('legal', 'driver_race'),
                     weights = obs %>% group_by(location_variable, legal) %>%
                       mutate(w=sum(num_stops)) %>%
                       with(w),
                     probs = 0.5) %>%
    mutate(state='WA')
  
  df = bind_rows(wa,co) %>%
    arrange(driver_race, state) %>%
    setNames(c('legal', 'driver_race', 'value', 'state'))
  
  tab_tex = do.call(function(...) sprintf(paste("\\begin{tabular}{@{}lllll@{}}",
          "& \\multicolumn{2}{l}{Colorado} & \\multicolumn{2}{l}{Washington} \\\\ ",
          "& Pre          & Post          & Pre           & Post           \\\\ \\midrule",
          "White    & %.0f\\%%           & %.0f\\%%            & %.0f\\%%            & %.0f\\%%             \\\\",
          "Black    & %.0f\\%%           & %.0f\\%%            & %.0f\\%%            & %.0f\\%%             \\\\",
          "Hispanic & %.0f\\%%           & %.0f\\%%            & %.0f\\%%            & %.0f\\%%             \\\\",
          "\\end{tabular}"
          , sep = '\n'), ...),
           as.list(100*df$value))
  
  f = file(filename)
  writeLines(tab_tex, f)
  close(f)
  
}
