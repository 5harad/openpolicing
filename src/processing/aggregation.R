# This file contains functions to produce aggregate dataframes from the raw data. 
# eg, compute the number of stops, searches, hits, and arrests broken down by age, race, and gender. 
# We save a number of aggregate dataframes using these functions because they are much faster 
# to do analysis on. 

add_citation_or_warning = function(d){
  # add columns to denote whether the stop resulted in 
  # a) a citation or b) in a warning / the trooper taking no action
  # (not all stops will result in one of these outcomes). 
  # NA if stop outcome is not known. 
  # this method is only reliable for the states included in our speeding citation analysis
  # since stop_outcome codes are heterogenous across states and it is hard to define citations consistently. 
  if(!('stop_outcome' %in% colnames(d))){
    d$cited = NA
    d$warned  = NA
  }
  else{
    d$cited = tolower(d$stop_outcome) %in% c('citation') 
    d$warned = tolower(d$stop_outcome) %in% c('warning', 'written warning', 'verbal warning', 'no action')
    d$cited[is.na(d$stop_outcome)] = NA # set to NA if stop outcome is unknown -- not actually necessary for our speeding analysis, but just in case others use it. 
    d$warned[is.na(d$stop_outcome)] = NA
  }
  return(d)
}

add_is_speeding = function(d){
  # this annotates as speeding only if the stop is JUST for speeding (no additional charges recorded)
  # NA if violation reason is not known 
  if(!('violation' %in% colnames(d))){
    d$is_speeding = NA
  }
  else{ 
    d$is_speeding = tolower(d$violation) == 'speeding'
  }
  return(d)
}

add_is_consent_search = function(d){
  # annotates as a consent search if a search was conducted and if consent is one of the reasons given for the search (hence we use grepl not ==)
  # NA if whether a search was conducted is unknown or if search type is unknown. False if there was no search. 
  # explicitly set VT to NA because it has weird search types which don't quite correspond to a consent search. 
  if((!('search_type' %in% colnames(d))) | (d$state[1] == 'VT')){
    d$consent_search_conducted = NA
  }
  else{
    d$consent_search_conducted = grepl('consent', tolower(d$search_type))
    d$consent_search_conducted[is.na(d$search_conducted) | is.na(d$search_type)] = NA
    d$consent_search_conducted[d$search_conducted == FALSE] = FALSE
  }
  return(d)
}

# Groups a dataframe by an arbitrary collection of columns and saves
# a number of useful statistics for each group (eg, n_stops, n_searches, n_hits, n_arrests, search_rate, hit_rate, arrest_rate). 

compute_aggregate_statistics = function(d, cols_to_group_by)
{
  state = d$state[1]
  stopifnot(all(cols_to_group_by %in% colnames(d)))
  
  # Compute number of days in the data. 
  min_date = min(d$stop_date, na.rm = TRUE)
  max_date = max(d$stop_date, na.rm = TRUE) 
  n_days = as.numeric(as.Date(max_date) - as.Date(min_date))
  
  # Exception for districts in NC, IL, RI: here we use districts, not counties, as the grouping variable. 
  # We do this because no county-level information is available. 
  if(state %in% c("IL","NC","RI") & "county_fips" %in% cols_to_group_by) {
    d$county_fips <- d$district
  }
  
  # Now do group_by with arbitrary columns and compute aggregate statistics. 
  # For an example of how this works, see first answer here: 
  # http://stackoverflow.com/questions/21208801/group-by-multiple-columns-in-dplyr-using-string-vector-input
  dots <- lapply(cols_to_group_by, as.symbol)
  grouped_df = group_by_(d, .dots = dots) %>%
    summarise(n_stops = n(), 
              n_searches = sum(search_conducted, na.rm = TRUE), 
              n_consent_searches = sum(consent_search_conducted, na.rm = TRUE),
              n_hits = sum(contraband_found, na.rm = TRUE), 
              n_arrests = sum(is_arrested, na.rm = TRUE),
              n_citations_for_speeding = sum(cited & is_speeding, na.rm = TRUE),
              n_warnings_for_speeding = sum(warned & is_speeding, na.rm = TRUE),
              # now we define counts of stops for which we actually have reliable data. 
              # we do this because some states are missing certain data fields for some stops and we don't want to use those stops to compute rates. 
              # eg, if a stop has a NA for whether someone was arrested, we should not use that stop in computing arrest rate. 
              # this is a minor point for our analysis because we remove states from the analyses if they have lots of missing data. 
              n_stops_with_search_data = sum(!is.na(search_conducted)),
              n_stops_with_consent_search_data = sum(!is.na(consent_search_conducted)),
              n_searches_with_contraband_data = sum(search_conducted & !is.na(contraband_found), na.rm = TRUE),
              n_stops_with_arrest_data = sum(!is.na(is_arrested), na.rm = TRUE),
              search_rate = n_searches / n_stops_with_search_data, 
              hit_rate = n_hits / n_searches_with_contraband_data, 
              arrest_rate = n_arrests / n_stops_with_arrest_data,
              state = first(state)) %>% 
    ungroup() 
  grouped_df$n_days = n_days
  grouped_df = as.data.frame(grouped_df)
  return(grouped_df)
}

# Returns the filepath for aggregated data for a given column grouping and state. 
get_aggregate_data_filename = function(state, 
                                       cols_to_group_by){
  filename = sprintf('%s/%s/%s_aggregated_by_%s.csv', 
                     aggregate_data_folder,
                     state, 
                     state, 
                     paste0(sort(cols_to_group_by), collapse = '_AND_'))
  return(filename)
}

# Loads aggregate data. 
load_aggregate_data = function(state, cols_to_group_by){
  fn <- get_aggregate_data_filename(state, cols_to_group_by) 
  if(file.exists(fn)){
    # suppress warnings because not all aggregate files have all these columns and so it throws distracting warnings. 
    d <- suppressWarnings(read_csv(fn, progress=F, col_types = cols(driver_gender = col_character(), 
                                                   driver_race = col_character(), 
                                                   driver_age_categorical = col_character(), 
                                                   county_fips = col_character(), 
                                                   stop_purpose = col_character(), 
                                                   stop_quarter = col_character(), 
                                                   weekday = col_character(), 
                                                   stop_hour_categorical = col_character(), 
                                                   n_stops = col_double(), 
                                                   n_searches = col_double(), 
                                                   n_consent_searches = col_double(),
                                                   n_hits = col_double(), 
                                                   n_arrests = col_double(),
                                                   n_searches_with_contraband_data = col_double(), 
                                                   n_warnings_for_speeding = col_double(), 
                                                   n_citations_for_speeding = col_double(),
                                                   n_stops_with_search_data = col_double(),
                                                   n_searches_with_contraband_data = col_double(),
                                                   n_stops_with_arrest_data = col_double(),
                                                   n_days = col_double(), 
                                                   search_rate = col_double(), 
                                                   hit_rate = col_double(), 
                                                   state = col_character()))) %>% as.data.frame()
    
  } else {
    message(sprintf("No aggregate data found for %s.", state))
    return(NULL)
  }
  return(d)
}

# Save all aggregate dataframes for a given set of states. 
# all_column_groupings is a list of vectors: eg, list(c('driver_race'), c('driver_race', 'county_fips'))
# states_to_use is a list of states to use. 
save_aggregate_dfs_for_state = function(all_column_groupings, state)
{
  stopifnot(is.list(all_column_groupings))
  stopifnot(nchar(state) == 2)
  
  aggregate_folder = sprintf('%s/%s/', aggregate_data_folder, state)
  if(!file.exists(aggregate_folder)){
    dir.create(aggregate_folder)
  }
  d <- read_state(state)
  stopifnot(!is.null(d))
  
  # Annotate with various necessary columns 
  d$driver_age_categorical = as.factor(bucket_age(d$driver_age)) # add bucketed age. 
  d = add_time_variables(d) 
  d = add_citation_or_warning(d)
  d = add_is_consent_search(d)
  d = add_is_speeding(d)
  
  # Loop over groupings, computing one aggregate dataframe for each grouping. 
  n_groupings_completed = 1
  for(cols_to_group_by in all_column_groupings){
    aggregate_filename = get_aggregate_data_filename(state, cols_to_group_by)
    message(sprintf("Computing aggregate statistics for %s (grouping %i/%i); columns to group by are\n", 
                    state, 
                    n_groupings_completed, 
                    length(all_column_groupings)))
    print(cols_to_group_by)
    df = compute_aggregate_statistics(d, cols_to_group_by)
    if(nrow(df) > 0){
      write_csv(df, aggregate_filename)
      message(sprintf("Successfully wrote file for %s.", aggregate_filename))
    }
    n_groupings_completed = n_groupings_completed + 1
  }
  
}

# small helper method to parallelize writing aggregate DFs. Uses 15 cores by default. 
# If you are running locally with only one core, just pass in 1. 
# Parallelizes over states. 
parallel_save_aggregate_dfs_for_states = function(all_column_groupings, states_to_use, n_cores = 15){
  mclapply(states_to_use, function(state, all_column_groupings) save_aggregate_dfs_for_state(state = state, 
                                                                                             all_column_groupings = all_column_groupings), 
           all_column_groupings = all_column_groupings, mc.cores = n_cores)
  
}

# This creates all the aggregate files we need for a given set of states to 
# quickly run the regressions and national-level disparate impact analyses in the paper. 
# We also create some groupings which are useful to have for fast analysis. 
create_all_aggregate_files = function(states_to_use, n_cores = 15){
  # First create a list of all the column sets we want to group by.

  # various useful groupings to save for quick analysis. 
  all_column_groupings = list(c('driver_race'), 
                              c('driver_race', 'violation'), 
                              c('driver_race', 'stop_outcome'),
                              c('driver_race', 'search_type'), 
                              c('driver_race', 'stop_date'))
  
  # now create groupings for each set of regressions. 
  for(columns_for_analysis in list(get_regression_variables('race'),
                                   get_regression_variables('race_and_county'),
                                   get_regression_variables('race_and_county_and_time'),
                                   get_regression_variables('race_and_county_and_demographics'),
                                   get_regression_variables('race_and_county_and_time_and_demographics'))){
    all_column_groupings[[length(all_column_groupings) + 1]] = columns_for_analysis
    # for stop rate regressions, we have to add in stop year to join with census analysis 
    all_column_groupings[[length(all_column_groupings) + 1]] = unique(c('stop_year', columns_for_analysis)) 
  }
  
  # alphabetize each grouping + remove duplicates to avoid doing extra work. 
  for(i in 1:length(all_column_groupings)){all_column_groupings[[i]] = sort(all_column_groupings[[i]])}
  all_column_groupings = all_column_groupings[!duplicated(all_column_groupings)]
  
  #Now loop over all states. Use parallelization to use cores more efficiently. 
  parallel_save_aggregate_dfs_for_states(states_to_use = states_to_use, 
                                         all_column_groupings = all_column_groupings,
                                         n_cores = n_cores)
}
