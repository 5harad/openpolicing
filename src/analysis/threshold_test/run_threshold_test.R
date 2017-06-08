library(rstan) # we have to explicitly load this because otherwise we get C++ errors.

#we run the same mcmc function for all models and just change the initialization. 
run_mcmc <- function(obs, full_model_path, full_output_path, iter = 5000, warmup = NULL, chains = 5, adapt_delta = 0.9, max_treedepth = 12, 
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
           lambda_d_raw   = runif(num_locations-1, -0.25, 0.25)
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

run_threshold_test <- function(state_prefix, location_type='county_name', min_stops=1000, adapt_delta=0.9, max_treedepth=12){
  
  # Check that inputs are valid
  stopifnot(location_type %in% c('county_name', 'location_raw', 'district'))
  stopifnot(toupper(state_prefix) %in% GOOD_THRESHOLD_TEST_DATA)

  # Generate model and output paths. 
  full_model_path = sprintf('%s/model_%s.stan', threshold_test_code_folder, THRESHOLD_TEST_MODEL_NAME)
  full_output_path = sprintf('%s/%s_%s.RData', threshold_test_output_folder,  state_prefix, THRESHOLD_TEST_MODEL_NAME)
  stopifnot(file.exists(sprintf(threshold_test_output_folder)))
  print(sprintf("Running model for %s and saving results to %s", toupper(state_prefix), full_output_path))
  
  # load state data
  df <- read_state(state_code = toupper(state_prefix), perform_data_checks=FALSE) %>%
    filter(!is.na(search_conducted), !is.na(contraband_found))
  
  # create a new column that uses the location variable so it has a standard name 
  df$location_variable = df[,location_type]
  
  # threshold counties with at least "min_stops" or a maximum number of 100 counties and more than 0 searches. 
  locations <- df %>% 
    filter(! is.na(location_variable)) %>%
    group_by(location_variable) %>% 
    summarise(num_stops = n(), num_searches = sum(search_conducted)) %>%
    filter((num_stops >= min_stops) & (num_searches > 0)) %>%
    arrange(desc(num_stops)) %>%
    top_n(100, num_stops)
  
  # create data frame with aggregate search / hit rates for each county-race pair
  # filter out stops with 0 searches as hit rate will be NA which Stan cannot handle
  stops <- df %>%
    filter(location_variable %in% locations$location_variable) %>%
    mutate(location_variable = factor(location_variable, levels=locations$location_variable)) %>%
    group_by(location_variable, driver_race) %>%
    summarise(num_stops    = n(), 
              num_searches = sum(search_conducted),
              num_hits     = sum(contraband_found),
              search_rate  = num_searches/num_stops,
              hit_rate     = num_hits/num_searches) %>%
    filter(num_searches != 0) %>%
    as.data.frame()
  
  print(paste0('Running threshold test on ', toupper(state_prefix), ': ', length(unique(stops$location_variable)), ' locations, ', length(unique(stops$driver_race)), ' race groups, ', min_stops, ' min nr stops.'))
  
  output = run_mcmc(stops, full_model_path, full_output_path, iter=5000, chains=5, adapt_delta=adapt_delta, max_treedepth=max_treedepth)
  message("Successfully completed threshold test.")
}

run_threshold_test_for_all_states = function(){
  run_threshold_test(state_prefix='co', location_type='county_name')
  run_threshold_test(state_prefix='ct', location_type='county_name', adapt_delta=0.95)
  run_threshold_test(state_prefix='il', location_type='location_raw') # department
  run_threshold_test(state_prefix='nc', location_type='district', adapt_delta=0.95) # two letter district codes. 
  run_threshold_test(state_prefix ='ri', location_type='location_raw', adapt_delta=0.95) # zone codes. 
  run_threshold_test(state_prefix='sc', location_type='county_name', adapt_delta=0.95)
  run_threshold_test(state_prefix='tx', location_type='county_name', adapt_delta=0.95)
  run_threshold_test(state_prefix='wa', location_type='county_name')
  run_threshold_test(state_prefix='wi', location_type='county_name') 
}
