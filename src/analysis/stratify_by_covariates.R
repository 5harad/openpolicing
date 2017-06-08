
# This file contains functions for doing regression or scatterplot analysis for each of
# several binary variables. We include stop rate in this framework, as it
# uses mostly the same code.
# * analysis_county() - plots for stratification by county 
# * analysis_regression() - regression controlling for all factors + estimating effect of race

# Small helper function to map variables to English names. 
get_english_label_for_binary_variable = function(binary_variable){
  stopifnot(binary_variable %in% ALL_BINARY_VARIABLES)
  if(binary_variable == 'is_stopped'){return("stop rate")}
  if(binary_variable == 'search_conducted'){return("search rate")}
  if(binary_variable == 'contraband_found'){return("hit rate")}
  if(binary_variable == 'consent_search_conducted'){return("fraction of stops\nresulting in consent searches")}
  if(binary_variable == 'is_arrested'){return("arrest rate")}
  if(binary_variable == 'cited_speeding_only'){return("cited, speeding only")}
}

acceptable_regressions <- c('race', 'race_and_county', 'race_and_county_and_time', 'race_and_county_and_demographics', 'race_and_county_and_time_and_demographics')

# small helper method that encodes the variables used in each type of regression.  
get_regression_variables <- function(regression_variables_to_use){
  stopifnot(regression_variables_to_use %in% acceptable_regressions)
  time_variables = c('stop_year', 'stop_quarter', 'weekday', 'stop_hour_categorical')
  if(regression_variables_to_use == 'race'){
    stratification = c('driver_race')
  }
  if(regression_variables_to_use == 'race_and_county'){
    stratification = c('driver_race', 'county_fips')
  }
  if(regression_variables_to_use == 'race_and_county_and_time'){
    stratification = c('driver_race', 'county_fips', time_variables)
  }
  if(regression_variables_to_use == 'race_and_county_and_demographics'){
    stratification = c('driver_race', 'county_fips', 'driver_age_categorical', 'driver_gender')
  }
  if(regression_variables_to_use == 'race_and_county_and_time_and_demographics'){
    stratification = c('driver_race', 'county_fips', 'driver_age_categorical', 'driver_gender', time_variables)
  }
  return(stratification)
}

# create_analysis_df() creates the dataframe on which we do analysis. It
# loops over the input states,  loading aggregate data for each state for
# which we have the variables in stratification (a vector). It returns a
# dataframe where each row corresponds to one combination of the variables
# in stratification in one state. Additionally, the row will have a pos_count
# field (the number of stops with positive examples of the variable) and
# a neg_count field (the number of stops with negative examples of the variable).
# So, if the variable is 'search_conducted' and stratification is
# c("driver_age_categorical", "driver_gender", "driver_race"), 
# the returned dataframe will have a row for each (age, gender, race) combination
# in each state with a count of the number of stops that resulted in searches
# and the number of stops that did not. 
# states: which states to use. 
# variable: which variable to analyze (eg, search_conducted)
# analysis_to_perform: either county or regression
# races_to_use: which races to include in the analysis. 
# regression_variables_to_use: which variables to include in regression (ignored if doing county analysis)
create_analysis_df <- function(states, variable, analysis_to_perform, 
                               races_to_use=c("Black", "Hispanic", "White"), 
                               regression_variables_to_use = 'race_and_county_and_time_and_demographics'){
  
  # Check whether the variable being analyzed and the analysis being performed are legitimate ones. 
  stopifnot(variable %in% ALL_BINARY_VARIABLES)
  stopifnot(analysis_to_perform %in% c('county', 'regression'))
  stopifnot(regression_variables_to_use %in% acceptable_regressions)
  
  # Gather a list of the variables necessary for the analysis. 
  # We use these variables to load in the appropriate aggregate dataframe. 
  if(analysis_to_perform == 'county'){
    stratification <- c('county_fips', 'driver_race')
  }else if(analysis_to_perform == 'regression'){
    stratification = get_regression_variables(regression_variables_to_use)
  }
  
  necessary_variables <- stratification
  if(variable == 'is_stopped'){
    necessary_variables <- c(necessary_variables, 'stop_year')  # Need stop_year to map to census data since populations change over time.
  }
  
  # Avoid accidental duplicates
  necessary_variables <- unique(necessary_variables)
  
  # Now we bind all the states for which we have data together, loading in one aggregate file at a time. 
  full_df <- NULL
  for(state in states){
    # Read the individual aggregated data files
    d = load_aggregate_data(state, necessary_variables)
    if(is.null(d)){
      next;
    }
    
    # Make sure race data is correct. 
    d <- filter(d, driver_race %in% races_to_use)
    stopifnot(length(unique(d$driver_race)) == length(races_to_use))
    
    # Skip states that do not have reliable data for the field we're interested in. 
    if((variable == 'is_arrested') & (!(state %in% GOOD_ARREST_DATA))){message(sprintf("Excluding %s because lacking reliable arrest data.", state)); next;}
    if(grepl('search_conducted', variable) & (!(state %in% GOOD_SEARCH_CONDUCTED_DATA))){message(sprintf("Excluding %s because lacking reliable search conducted data.", state)); next;}
    if(grepl('contraband_found', variable) & (!(state %in% GOOD_COUNTY_LEVEL_CONTRABAND_DATA))){message(sprintf("Excluding %s because lacking reliable county-level contraband data.", state)); next;}
    if((variable == 'consent_search_conducted') & (!(state %in% GOOD_CONSENT_DATA))){message(sprintf("Excluding %s because lacking reliable consent search data.", state)); next;}
    if((variable == 'cited_speeding_only') & (!state %in% GOOD_SPEEDING_CITATION_DATA)){message(sprintf("Excluding %s because lacking reliable speeding citation data.", state)); next;}
    # if doing regressions on age or time, exclude states missing too much data for these variables. 
    if((analysis_to_perform == 'regression') & (grepl('time', regression_variables_to_use)) & (state %in% BAD_TIME_OR_DATE_DATA)){message(sprintf("Excluding %s from %s regression because lacking reliable time or date data.", state, regression_variables_to_use)); next;}
    if((analysis_to_perform == 'regression') & (grepl('demographics', regression_variables_to_use)) & (state %in% BAD_AGE_OR_GENDER_DATA)){message(sprintf("Excluding %s from %s regression because lacking reliable age or gender data.", state, regression_variables_to_use)); next;}

    # Add the state's data to the full data frame.
    message(sprintf("Adding data for %s.", state))
    d$state <- state
    full_df <- rbind(full_df, d)
  }
  
  if(is.null(full_df)){
    message("No data in dataframe; returning NULL.")
    return(NULL)
  }
  if(nrow(full_df) == 0){
    message("No data in dataframe; returning NULL.")
    return(NULL)
  }
  
  # Create the pos_count and total_count columns (see comment at top for explanation). 
  if(variable == 'search_conducted'){
    full_df$pos_count <- full_df$n_searches
    full_df$total_count <- full_df$n_stops_with_search_data
  }else if(variable == 'is_arrested'){
    full_df$pos_count <- full_df$n_arrests
    full_df$total_count <- full_df$n_stops_with_arrest_data
  } else if(variable == 'contraband_found'){
    full_df$pos_count <- full_df$n_hits
    full_df$total_count <- full_df$n_searches_with_contraband_data
  }else if(variable == 'cited_speeding_only'){
    full_df$pos_count = full_df$n_citations_for_speeding
    full_df$total_count = full_df$n_citations_for_speeding + full_df$n_warnings_for_speeding
  }else if(variable == 'consent_search_conducted')
  {
    full_df$pos_count <- full_df$n_consent_searches
    full_df$total_count <- full_df$n_stops_with_consent_search_data
  }else if(variable == 'is_stopped'){
    # For the stop rate analysis, the denominator is census counts,
    # so we have to connect to Census data. This is a bit involved. 
    
    # First we want to filter out years in states with only partial data. 
    # These years will throw off our estimates of people stopped per year
    # and keep us from matching consistently to Census data. 
    incomplete_years = get_incomplete_years(unique(full_df$state))
    full_df = filter(full_df, !(paste(state, stop_year) %in% incomplete_years$state_year))
    if(nrow(incomplete_years) > 0){
      message("Filtering out years with partial data.")
      print(incomplete_years)
    }

    # read in the Census data for driving age population. 
    dcC <- read_csv(paste0(processed_intercensal_data_path), progress=F) %>%
      filter(race %in% races_to_use, year > 2010) %>%
      mutate(
        county_fips=fips,
        stop_year=year,
        state_code=substr(fips, 1, 2),
        driver_race=race,
        driver_age_categorical=age,
        driver_gender=gender
      ) %>%
      select(county_fips, stop_year, state_code, driver_race, driver_age_categorical, driver_gender, count)
    
    # Group by district for states without county data. This is just Illinois and North Carolina. 
    dcC <- rbind(
      # exclude Illinois and North Carolina, as we'll add them as districts
      dcC %>% filter(!(state_code %in% c('17','37'))), 
      dcC %>% filter(  state_code %in% c('17','37')) %>%
        inner_join(
          read_csv(paste0(code_path, "/resources/dictionaries/districts.csv"), col_types='cccc') %>%
            select(county_fips, district),
          by="county_fips"
        ) %>%
        # relabel county_fips using the district codes, and compute count for each district. 
        mutate(county_fips=district) %>%
        group_by(county_fips, stop_year, state_code, driver_race, driver_age_categorical, driver_gender) %>%
        summarize(count=sum(count)) %>%
        ungroup()
    ) %>%
      as.data.frame()
    # Filter Census data down to counties which are actually in the dataset. 
    dcC = filter(dcC, paste(county_fips, stop_year) %in% unique(paste(full_df$county_fips, full_df$stop_year)))
    
    # Make sure everything is the same type for the joins
    for(c in stratification){
      if(c %in% colnames(dcC)){dcC[,c] <- as.character(dcC[,c])}
      if(c %in% colnames(full_df)){full_df[,c] <- as.character(full_df[,c])}
    }
    # Join census data to stop data, adding stop year so we are sure to match on time period
    # if we're doing county analysis, or a regression just on race and county, we have to add up the age + gender groups in the census data
    # and join using time, race, and county.
    if((analysis_to_perform == 'county') | 
       (analysis_to_perform == 'regression' & regression_variables_to_use == 'race_and_county')){
      dcC = dcC %>% group_by(county_fips, driver_race, stop_year) %>% summarise(count = sum(count))
      
      # join census data frame to stop DF. Join order is important because if you flip the order you cut out counties with no stops. 
      # we have already filtered the Census data down to only include counties in the stops dataset. 
      counties_to_states = full_df %>% select(county_fips, state) %>% filter(!is.na(state)) %>% unique() %>% as.data.frame()
      full_df = dcC %>% 
        left_join(full_df, by = c('county_fips', 'driver_race', 'stop_year')) %>% ungroup()
      # the join creates a couple NAs we have to fix (from counties which record no stops). 
      full_df$n_stops[is.na(full_df$n_stops)] = 0
      full_df$state = ifelse(is.na(full_df$state), plyr::mapvalues(full_df$county_fips, counties_to_states$county_fips, counties_to_states$state), full_df$state)
      
      # if we are doing the county analysis, we only want race and county as covariates, so average over years. 
      if(analysis_to_perform == 'county'){
        full_df = full_df %>% 
          group_by(county_fips, driver_race) %>% 
          summarise(state = first(state), 
                    mean_population = mean(count, na.rm = TRUE), # average population in county over years.
                    n_stops = sum(n_stops, na.rm = TRUE),#, # total stops made in county over years. 
                    n_years = length(unique(stop_year)), # total number of years. Each year is a full year (because we filter out partial years to keep things clean)
                    stops_per_year = n_stops / n_years, # stops per year in county
                    pos_count = round(stops_per_year), 
                    total_count = round(mean_population)) %>%
          filter(!is.na(mean_population))
        stopifnot((sum(is.na(full_df$n_stops)) == 0) & (sum(is.na(full_df$state)) == 0))
      }else{ 
        # if we're not averaging across stop years, just a simple renaming of variables suffices. 
        full_df = full_df %>% 
          mutate(pos_count = n_stops, 
                 total_count = count)
        stratification = c(stratification, 'stop_year') # add stop year to the variables we need to hang on to in the final dataframe (since it's needed for regression). 
      }
    }
    
    # if we're doing a regression that includes demographics, we join to Census data using demographic variables as well. 
    if((analysis_to_perform == 'regression' & regression_variables_to_use == 'race_and_county_and_demographics')){
      counties_to_states = full_df %>% select(county_fips, state) %>% filter(!is.na(state)) %>% unique()
      # join creates a couple NAs we have to fix (again, from substratifications with no stops). 
      full_df = dcC  %>% 
        left_join(full_df, by = c('county_fips', 'driver_race', 'driver_age_categorical', 'driver_gender', 'stop_year'))
      full_df$n_stops[is.na(full_df$n_stops)] = 0
      full_df$state = ifelse(is.na(full_df$state), plyr::mapvalues(full_df$county_fips, counties_to_states$county_fips, counties_to_states$state), full_df$state)
      full_df = full_df %>% 
        mutate(pos_count = n_stops, 
               total_count = count)
      stratification = c(stratification, 'stop_year')
    }
  }
  
  # Now we just do some post-processing / sanity-checking to be careful. 
  # Make sure that no variables are NA's. 
  for(var in stratification){ 
    full_df = full_df[!is.na(full_df[,var]),]
  }
  # Remove states with no positive examples
  states_without_data <- full_df %>%
    group_by(state) %>%
    summarize(pos_count=sum(pos_count, na.rm = TRUE)) %>%
    filter(pos_count==0)
  full_df <- filter(full_df, !(state %in% states_without_data$state))
  if(nrow(states_without_data) > 0){
    message(
      sprintf('filtering out states with no total pos_count = 0 (probably indicating no data): %s', 
              paste(states_without_data$state, collapse=',')))
  }
  
  
  # if we have data down to the year, filter out state - years with no data 
  # (this is a problem in some states with inconsistent data). 
  if('stop_year' %in% colnames(full_df)){
    state_years_without_data <- full_df %>%
      group_by(state, stop_year) %>%
      summarize(pos_count=sum(pos_count, na.rm = TRUE)) %>%
      ungroup() %>%
      filter(pos_count==0) %>%
      mutate(state_year = paste(state, stop_year))
    if(nrow(state_years_without_data) > 0){
      full_df <- filter(full_df, !(paste(state, stop_year) %in% state_years_without_data$state_year))
      message(
        sprintf('filtering out %i state-years with total pos_count = 0 (probably indicating no data): %s', 
                nrow(state_years_without_data),
                paste(state_years_without_data$state_year, collapse=',')))
    }
  }
  
  # Make sure all states have more than one county (this would indicate bad county information)
  if('county_fips' %in% stratification){
    states_with_one_county <- full_df %>% 
      filter(!is.na(county_fips)) %>%
      group_by(state) %>% 
      summarize(n_counties=length(unique(county_fips))) %>%
      filter(n_counties <= 1)
    stopifnot(nrow(states_with_one_county) == 0)
  }
  
  # Compute pos_fraction. Filter out cells with 0 or nan total count to avoid nans. 
  stopifnot(is.na(full_df$total_count) == 0)
  full_df$pos_fraction <- full_df$pos_count / full_df$total_count
  full_df <- filter(full_df, total_count > 0)
  
  # Subset and print results
  full_df <- full_df[, c(stratification, 'state', 'pos_count', 'pos_fraction', 'total_count')]
  message(sprintf("Variable is %s.\nStratification variables are %s.\n", variable, paste(stratification, collapse=',')))
  message(sprintf("Total number of rows in full dataframe used for analysis: %i\nTotal count %2.1f\nTotal states %i\n%s\nPositive proportion %2.3f\n", 
                  nrow(full_df), 
                  sum(full_df$total_count), 
                  length(unique(full_df$state)),
                  paste(sort(unique(full_df$state)), collapse = ','),
                  sum(full_df$pos_count) / (sum(full_df$total_count))))
  
  return(full_df)
}


# Analysis #1 - Stratify by county. Creates a plot.
analysis_county <- function(states, variable, mx=NULL){
  # Given a list of states, create a scatterplot of rates broken down by race + location. 
  # variable specifies the rate to plot. So if variable == 'search_conducted', will plot search rate. 

  # First we have to create a dataframe where each row is a (county, race) pair. 
  # Columns should be driver_race, county_fips, pos_count, pos_fraction, total_count.
  # Eg, if we're looking at stop rate, pos_count = n_stops, and total_count = population. 
  # or, if we're looking at hit rate,  pos_count = n_hits, and total_count = n_searches. 
  df <- create_analysis_df(states, variable, analysis_to_perform = 'county')
  stop_rate <- variable == 'is_stopped'
  use_percent <- !stop_rate # if you want to plot a stop RATE as opposed to a percent.
  min_total_count <- ifelse(stop_rate, 1000, 500) # threshold on county size so we don't plot super noisy estimates. Use larger threshold for stop rate.
  axis_label <- get_english_label_for_binary_variable(variable) # English label for axis. 
  
  # Sanity checks to see whether the data is ok
  stopifnot((!is.null(df)) & (nrow(df) > 0))
  for(col in c('driver_race', 'county_fips', 'pos_count', 'total_count')){
    stopifnot(col %in% colnames(df))
    stopifnot(sum(is.na(df[,col])) == 0)
  }  
  stopifnot(sum(duplicated(df[,c('driver_race','county_fips')])) == 0) # make sure there are no duplicate county-race pairs. 
  
  # First do some cleanup of names for nice display
  df$driver_race <- factor(paste(df$driver_race, "drivers"), levels=c("White drivers", "Black drivers", "Hispanic drivers"))
  
  # Filter out counties with unreliable estimates
  counties_with_enough_data <- df %>%
    group_by(county_fips) %>%
    summarize(total_count=sum(total_count)) %>%
    filter(total_count >= min_total_count)
  df <- filter(df, county_fips %in% counties_with_enough_data$county_fips)
  
  # Save the data underlying the figure
  data_for_figure = df %>% select(driver_race, county_fips, pos_count, total_count)
  
  # Get aggregate statistics, display on top as a sanity check / in case we need numbers in paper. 
  total_count <- sum(df$total_count)
  pos_count <- sum(df$pos_count, na.rm = TRUE)
  n_states <- length(unique(df$state))
  n_counties <- length(unique(df$county_fips))
  state_string <- paste(sort(unique(df$state)), collapse = ',')
  
  # Make 'White' the reference column
  df <- df %>%
    filter(driver_race == 'White drivers') %>% 
    right_join(df %>% filter(driver_race != 'White drivers'), by='county_fips')
  stopifnot((length(unique(df$driver_race.x)) == 1) & df$driver_race.x[1] == 'White drivers')

  # Compute fraction of counties for which white drivers have higher values. 
  white_higher_than_black = mean(df[df$driver_race.y == 'Black drivers',]$pos_fraction.x > df[df$driver_race.y == 'Black drivers',]$pos_fraction.y)
  white_higher_than_hispanic = mean(df[df$driver_race.y == 'Hispanic drivers',]$pos_fraction.x > df[df$driver_race.y == 'Hispanic drivers',]$pos_fraction.y)
  
  # Add small margins and set maximum. 
  if(is.null(mx)){
    mx_search <- max(quantile(df$pos_fraction.x, .99), quantile(df$pos_fraction.y, .99))
    mx <- ceiling(mx_search/0.05)*0.05 
  }
  
  title_string = sprintf("States: %i; counties: %i; count: %s; pos count: %s;\nwhite higher than black for %2.0f%%, Hispanic for %2.0f%%\n(count represents denominator, not stops)\nStates: %s", 
                         n_states, 
                         n_counties,
                         big_round(total_count),
                         big_round(pos_count),
                         white_higher_than_black * 100, 
                         white_higher_than_hispanic * 100,
                         state_string)
  message(title_string)
  
  # Construct file name and set plot parameters. 
  # zoom in a bit for some variables to avoid outlier counties.
  fn = NULL
  breaks = waiver()
  if(length(states) > 1){  # save plot if national aggregate.
    if(variable=='is_stopped'){fn = 'fig2_stop_rate_county';  data_for_figure = data_for_figure %>% rename(location = county_fips, n_stops = pos_count, county_population = total_count); mx = min(mx, 0.25); breaks = c(0.0, .1, .2)}
    else if(variable=='search_conducted') {fn = 'fig3a_search_rate_county'; data_for_figure = data_for_figure %>% rename(location = county_fips, n_searches = pos_count, n_stops = total_count); mx = min(mx, 0.07); breaks = c(0, .025, .05)}
    else if(variable=='is_arrested'){fn = 'fig3b_arrest_rate_county'; data_for_figure = data_for_figure %>% rename(location = county_fips, n_arrests = pos_count, n_stops = total_count); mx = min(mx, .07); breaks = c(0, 0.025, 0.05)}
    else if(variable=='contraband_found'){fn = 'fig4a_hit_rate_county'; data_for_figure = data_for_figure %>% rename(location = county_fips, n_hits = pos_count, n_searches = total_count);}
  }
  
  # Construct and save the plot. 
  if(variable == 'is_stopped'){ # if plotting stop rates, size points by positive count (number of stops)
    df$col_to_size_points_by = df$pos_count.y
  }else{ # otherwise, size points by denominator
    df$col_to_size_points_by = df$total_count.y
  }
  data.frame(x=df$pos_fraction.x, y=df$pos_fraction.y, n=df$col_to_size_points_by, minority=df$driver_race.y) %>%
    scatter_circles(fn=fn, var=axis_label, lim=c(0, mx), use_percent=use_percent, breaks=breaks) -> p
  if(!is.null(fn)){
    write_csv(data_for_figure, sprintf('%s/%s.csv', data_for_figures_folder, fn))
  }
  print(p)
}

# Analysis #2 - Regression
# Either a binomial or a negative binomial regression of pos_event ~ total_count + covariates. 
analysis_regression <- function(states, 
                                variable, 
                                regression_variables_to_use, 
                                stop_rate_regression_type = 'negative_binomial'){
  # define some parameters to control the maximum number of counties used in the regression (used to keep things tractable and avoid using overly small counties)
  # this filters out a very small proportion of stops and does not affect analysis. 
  max_counties <- 1000 # ifelse(variable == 'is_stopped', 1000, 750)
  min_count_in_county <- ifelse(variable == 'is_stopped', 1000, 100)
  stopifnot(stop_rate_regression_type %in% c('negative_binomial', 'poisson_sandwich', 'quasipoisson'))
  
  # If doing stop rate regression, include stop year as covariate. 
  include_stop_year = variable == 'is_stopped'
  
  # create dataframe for analysis. 
  df <- create_analysis_df(states, 
                           variable, 
                           analysis_to_perform = 'regression', 
                           regression_variables_to_use = regression_variables_to_use)
  label <- get_english_label_for_binary_variable(variable)
  
  # If we run regressions on a single state and it lacks the data we need, dataframe will be empty, so return NULL.
  if(is.null(df)){message("No data in dataframe; not performing regression."); stopifnot(length(states) == 1); return(NULL)} 
  if(nrow(df) == 0){message("No data in dataframe; not performing regression."); stopifnot(length(states) == 1); return(NULL)} 
  
  # get the variables to use in the regression. 
  covs_to_use = get_regression_variables(regression_variables_to_use) 
  if(include_stop_year){message("Controlling for stop year."); covs_to_use = unique(c(covs_to_use, 'stop_year'))}
  
  # Sanity checks to see whether the data is ok
  stopifnot((!is.null(df)) & (nrow(df) > 0))
  for(col in c(covs_to_use, 'pos_count', 'total_count')){
    stopifnot(col %in% colnames(df))
    stopifnot(sum(is.na(df[,col])) == 0)
  }
  stopifnot(sum(duplicated(df[,c('state', covs_to_use)])) == 0)
  if('driver_gender' %in% colnames(df)){stopifnot(sum(!(df$driver_gender %in% c('M', 'F'))) == 0)}
  stopifnot(sum(!(df$driver_race %in% c('Hispanic', 'White', 'Black'))) == 0)
  
  # Throw out cells with no data
  df <- filter(df, total_count > 0)
  
  message(sprintf(
    "After filtering out cells with no values, %i counties and %i cells to run regression on.\n", 
    length(unique(df$county_fips)), 
    nrow(df)))
  
  # If we are looking at county, 
  # filter for top n counties to avoid very small counties. We have to do this slightly differently for stop rate 
  # to avoid duplication across years. 
  if(regression_variables_to_use == 'race'){ # county is not included as a covariate. 
    status_string = sprintf(
      "%i states, %i cells to run regression on.\nPositive count: %i; total count %2.1f.", 
      length(unique(df$state)), nrow(df), sum(df$pos_count), sum(df$total_count))
  }
  else{ # county is included as a covariate, so have to do filtering. 
    if(variable != 'is_stopped'){
      counties_with_most_data <- df %>% 
        filter(!is.na(county_fips)) %>%
        group_by(county_fips) %>% 
        summarize(n=sum(total_count)) %>% 
        filter(n > min_count_in_county) %>%
        arrange(-n)
    }else{ # if analyzing stop rate, we have to be sure to remove duplicates to accurately compute county counts. 
      columns_to_groupby = colnames(df)[colnames(df) %in% c('driver_age_categorical', 'driver_race', 'county_fips', 'driver_gender')]
      df$duplicates = duplicated(df[,columns_to_groupby])
      
      counties_with_most_data = df %>% 
        filter(!is.na(county_fips), !duplicates) %>%
        group_by(county_fips) %>% 
        summarize(n=sum(total_count, na.rm = TRUE))
      message(sprintf("Before filtering counties, total n is %i.", sum(counties_with_most_data$n)))
      counties_with_most_data = counties_with_most_data %>%
        filter(n > min_count_in_county, !is.na(n)) %>%
        arrange(-n)
    }
    count_prior_to_filtering = sum(counties_with_most_data$n)
    counties_with_most_data <- counties_with_most_data %>% top_n(max_counties, n)
    
    message(sprintf("After filtering counties, total n is %i.", sum(counties_with_most_data$n)))
    
    df <- df %>% filter(county_fips %in% counties_with_most_data$county_fips, !is.na(total_count))
    df$total_count = as.numeric(df$total_count)
    status_string = sprintf(
      "After filtering for top %i counties, %i counties, %i states, %i cells to run regression on.\nPositive count: %i; total count %2.1f. Count prior to filtering %i.", 
      max_counties, length(unique(df$county_fips)), length(unique(df$state)), nrow(df), sum(df$pos_count), sum(df$total_count), count_prior_to_filtering)
  }
  message(status_string)
  
  # Set white as the reference
  df$driver_race <- factor(df$driver_race, levels=c("White", "Black", "Hispanic"))
  
  df <- as.data.frame(df)
  for(c in covs_to_use){df[,c] <- as.factor(df[,c])}
  
  if(variable != 'is_stopped'){
    # Run the actual model. For every variable except stop rate, it's a binomial. 
    formula_string = paste('cbind(pos_count, total_count - pos_count) ~ ', paste(sprintf('C(%s)', covs_to_use), collapse = ' + '))
    message(sprintf("Running binomial regression with formula %s.", formula_string))
    model <- glm(as.formula(formula_string), data=df, family=binomial)
    model_summary = summary(model)
  }
  else{#If stop rate, use a negative binomial count model (to deal with overdispersion). Also include other specifications to check robustness. 
    message(sprintf("Running stop rate regression with regression type %s.", stop_rate_regression_type))
    formula_string = paste('pos_count ~ ', paste(sprintf('C(%s)', covs_to_use), collapse = ' + '), '+ offset(log(total_count))')
    if(stop_rate_regression_type == 'negative_binomial'){
      model <- MASS::glm.nb(as.formula(formula_string), data=df)
      model_summary <- summary(model)
    }
    if(stop_rate_regression_type == 'quasipoisson'){
      model <- glm(as.formula(formula_string), quasipoisson, data = df)
      model_summary <- summary(model)
    }
    if(stop_rate_regression_type == 'poisson_sandwich'){
      model <- glm(as.formula(formula_string), poisson, data = df)
      hcSE <- vcovHC(model, type="HC0")
      model_summary = coeftest(model, vcov=hcSE)
    }
    
  }
  
  # If we are only running regressions on a single state, just return the model coefficients (don't want to save a million regressions or print out a ton of output)
  if(length(states) == 1){return(summary(model)$coefficients)}
  
  # Extract and print out the model summary. This won't work for the Poisson sandwich model so we skip it. 
  if(!(stop_rate_regression_type == 'poisson_sandwich')){
    summary_without_county <- model_summary$coefficients[!grepl('fips', rownames(model_summary$coefficients)),]
    caption <- sprintf("%s regression. For brevity, counties are not included.", label)
    print(xtable::xtable(summary_without_county, caption=caption))
  }
  
  # Save output to file
  if(stop_rate_regression_type == 'negative_binomial'){ # default. 
    fn <- sprintf(paste0(regression_results_folder, 'national_aggregate_%s_stratified_by_%s_regression.txt'), variable, regression_variables_to_use)
  }else{ # include name of regression if not default. 
    fn <- sprintf(paste0(regression_results_folder, 'national_aggregate_%s_stratified_by_%s_%s_regression.txt'), variable, regression_variables_to_use, stop_rate_regression_type)
  }
  print(fn)
  # save just the model frame and coefficients (full model gets huge -- multiple gigabytes -- for some specifications)
  # we need to save these things in order to compute rates for the hypothetical driver (Table 2).
  model_frame = model.frame(model)
  model_coefficients = model$coefficients
  save(model_frame, model_coefficients, file = gsub('.txt', '.RData', fn))
  write(sprintf('Formula used:\n%s', formula_string), file = fn)
  write(sprintf("States used: %s.", paste(unique(df$state), collapse = ',')), file = fn, append = TRUE)
  write(sprintf(status_string), file = fn, append = TRUE)
  write(capture.output(model_summary), file = fn, append = TRUE)
  message(sprintf("Saved regression output to %s.\n", fn))
}
