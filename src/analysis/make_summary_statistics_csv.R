# This script produces a single CSV which reports statistics broken down by race and county. 

# combine tables for each rate analysis. 
combined_df = NULL
for(var in ALL_BINARY_VARIABLES){
  # use create_analysis_df in stratify_by_covariates.R to create the dataframe of rates. 
  # this is the same function we use to make the rate plots + run regressions. 
  df = create_analysis_df(states = FINAL_STATE_LIST, variable = var, analysis_to_perform = 'county') %>%
    select(county_fips, state, driver_race, pos_fraction)
  
  message(sprintf("%s: %i counties, %s states", 
                  var, 
                  length(unique(df$county_fips)), 
                  paste0(unique(df$state), collapse = ',')))
  if(var == 'is_stopped'){
    df = df %>% rename(stop_rate = pos_fraction)
  }else if(var == 'search_conducted'){
    df = df %>% rename(search_rate = pos_fraction)
  }else if(var == 'is_arrested'){
    df = df %>% rename(arrest_rate = pos_fraction) 
  }else if(var == 'consent_search_conducted'){
    df = df %>% rename(consent_search_rate = pos_fraction)
  }else if(var == 'cited_speeding_only'){
    df = df %>% rename(citation_rate_speeding_stops = pos_fraction)
  }else if(var == 'contraband_found'){
    df = df %>% rename(hit_rate = pos_fraction)
  }
  stopifnot(sum(duplicated(df %>% select(county_fips, state, driver_race))) == 0) # make sure there's only one row per race-location pair. 
  if(is.null(combined_df)){
    combined_df = df
  }else{
    combined_df = full_join(combined_df, df, by = c('county_fips', 'state', 'driver_race'))
  }
}
combined_df$county_fips = toupper(as.character(combined_df$county_fips))

# now add in threshold data. 
threshold_df = read_csv(paste0(data_for_figures_folder, 'fig4b_threshold_test.csv')) %>% 
  mutate(driver_race = gsub(' drivers', '', driver_race)) %>% 
  mutate(location_variable = toupper(location_variable))
county_mapping = read_csv('resources/county_index.csv') %>% 
  select(fips, state, county_name) %>% 
  rename(location_variable = county_name, 
         county_fips = fips) %>% 
  mutate(location_variable = toupper(location_variable))
threshold_df = left_join(threshold_df, county_mapping, by = c('location_variable', 'state')) %>%  # map county names to FIPS if possible. 
  mutate(county_fips = ifelse(is.na(county_fips), location_variable, county_fips)) %>%
  select(county_fips, state, driver_race, inferred_threshold)
combined_df = full_join(combined_df, threshold_df, by = c('driver_race', 'county_fips', 'state')) %>% as.data.frame()


# Add in stop counts (ie, total number of stops in each county divided by time period reported)
stop_count_df = NULL
message("Computing stop counts per year")
for(state in unique(combined_df$state)){
  # we use the number of unique days in the reporting period: this is to deal with gaps in reporting
  # which actually only happens in AZ when a two-week period is missing (in two years), but this seems most precise. 
  # this computation will not work for aggregate states, because dates aren't granular; first we make sure we're not using those 
  stopifnot(!(state %in% c('MO', 'NE', 'MD'))) 
  # count number of days in stop data. 
  date_d = load_aggregate_data(state, cols_to_group_by = c('driver_race', 'stop_date'))
  n_days_in_state_data = length(unique(date_d$stop_date))
  message(sprintf('%s has %i unique days over %i years', state, n_days_in_state_data, length(unique(year(date_d$stop_date)))))
  # get total stop counts by county. 
  d = load_aggregate_data(state, cols_to_group_by = c('driver_race', 'county_fips')) %>%
    mutate(stops_per_year = as.integer(round(n_stops / (n_days_in_state_data / 365)))) %>%
    select(state, county_fips, driver_race, stops_per_year)
  stop_count_df = rbind(stop_count_df, d)
}
# join stop counts to main dataframe. 
stop_count_df$county_fips = toupper(stop_count_df$county_fips)
combined_df = left_join(combined_df, stop_count_df, by = c('state', 'county_fips', 'driver_race'))
# a few counties have no stops at all for minorities, so don't appear in stop count dataframe; fill these in with 0s. 
combined_df$stops_per_year[is.na(combined_df$stops_per_year)] = 0 

# rename and select columns. 
combined_df = combined_df %>% 
  rename(location = county_fips) %>%
  arrange(location, state, driver_race) %>%
  select(location, state, driver_race, stops_per_year, 
         stop_rate, search_rate, consent_search_rate, 
         arrest_rate, citation_rate_speeding_stops, hit_rate, inferred_threshold)

# map locations to standardized human-readable values. For every state except RI, IL, and NC these will be county names; for those states we had to use alternate location groupings. 
combined_df$location = suppressMessages(plyr::mapvalues(as.character(combined_df$location), as.character(county_mapping$county_fips), as.character(county_mapping$location_variable)))

# reformat. 
for(col in c('stop_rate', 'search_rate', 'consent_search_rate', 'arrest_rate', 'hit_rate', 'citation_rate_speeding_stops', 'inferred_threshold')){
  combined_df[,col] = round(combined_df[,col], 3)
}
combined_df$stops_per_year = as.integer(combined_df$stops_per_year)

# final basic sanity checks. 
stopifnot(sum(is.na(combined_df$location)) == 0)
stopifnot(sum(is.na(combined_df$state)) == 0)
stopifnot(sum(is.na(combined_df$driver_race)) == 0)
stopifnot(sum(is.na(combined_df$stops_per_year)) == 0)

# sort by state
combined_df = combined_df %>% arrange(state)

# write output file. 
output_fn = paste0(data_for_figures_folder, 'combined_data.csv')
write_csv(combined_df, output_fn)
message(sprintf("Successfully wrote summary statistics table to %s.", output_fn))
