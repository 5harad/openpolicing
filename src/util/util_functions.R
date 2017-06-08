# This script contains a variety of helper functions used across the repository

# Map functions to not have to load the full package
map <- plyr::mapvalues

# computes which states have years with only partial stop records; we filter these out in computing stop rate
# to ensure that the regressions are consistent and that we can match consistently to Census data. 
# we define incomplete years to be years with 5000 or more stops but at least 7 days with no stop data -- implausible by chance
# likely indicates missing data. 
get_incomplete_years = function(state_list){
  df = NULL
  for(state in state_list){
    d = load_aggregate_data(state, c('driver_race', 'stop_date'))
    d$year = year(d$stop_date)
    unique_dates_by_year = d %>% 
      group_by(year) %>% 
      summarise(n_stops = sum(n_stops), 
                n_unique_dates = length(unique(stop_date)), 
                min_date = min(stop_date), 
                max_date = max(stop_date),
                state = first(state)) %>% 
      ungroup()
    
    df = rbind(df, unique_dates_by_year)
  }
  incomplete_years = filter(df, n_unique_dates < 365 - 7, n_stops > 5000) %>%
    as.data.frame() %>%
    mutate(state_year = paste(state, year))
  # this function will not work for aggregate states because date is not sufficiently granular, so filter out any spurious results. 
  incomplete_years = filter(incomplete_years, !(state %in% c('MO', 'NE', 'MD')))
  return(incomplete_years)
}

#function to check for duplicates in data. pass in a set of columns
find_duplicates = function(d, cols){
  complete_cases = complete.cases(d[, cols])
  duplicates = duplicated(d[,cols])
  cat(sprintf("Using columns to find duplicates\n"))
  print(cols)
  cat(sprintf("Columns are not nan for proportion %2.3f of cases.\n", mean(complete_cases)))
  
  d$is_duplicate = (duplicates & complete_cases)
  cat(sprintf("Duplicates account for proportion %2.3f of stops.\n", 
              mean(d$is_duplicate)))
  cat(sprintf("Duplicates account for proportion %2.3f of searches.\n", 
              sum(d$search_conducted[d$is_duplicate]) / sum(d$search_conducted)))
  
  
  cat(sprintf("Search rate without duplicates: %2.5f; with, %2.5f\n", 
              mean(d$search_conducted[!d$is_duplicate], na.rm = TRUE),
              mean(d$search_conducted, na.rm = TRUE)))
  
  cat(sprintf("Duplicates account for proportion %2.3f of hits.\n", 
              sum(d$contraband_found[d$is_duplicate]) / sum(d$contraband_found)))
  cat(sprintf("Hit rate without duplicates: %2.5f; with, %2.5f\n", 
              mean(d$contraband_found[(!d$is_duplicate) & d$search_conducted], na.rm = TRUE),
              mean(d$contraband_found[d$search_conducted], na.rm = TRUE)))
}

data_check <- function(d){
  #do various sanity checks on d.
  #This is a fast method that just does bare-bones sanity checking
  
  if((sum(is.na(d$search_conducted)) != 0) & (sum(is.na(d$search_conducted)) != nrow(d))){
    message(sprintf('Data alert: search_conducted contains %2.3f%% nans.', 100*mean(is.na(d$search_conducted))))
  }
  
  if((sum(is.na(d$contraband_found)) != 0) & (sum(is.na(d$contraband_found)) != nrow(d))){
    message(sprintf('Data alert: contraband_found contains %2.3f%% nans.', 100*mean(is.na(d$contraband_found))))
  }
  
  if((sum(is.na(d$is_arrested)) != 0) & (sum(is.na(d$is_arrested)) != nrow(d))){
    message(sprintf('Data alert: is_arrested contains %2.3f%% nans.', 100*mean(is.na(d$is_arrested))))
  }
  
  if(sum(is.na(d$stop_time) != 0)){
    message(sprintf('Data alert: stop_time contains %2.3f%% nans.', 100*mean(is.na(d$stop_time))))
  }
  if(sum(!(d$driver_gender %in% c("M", "F", NA))) > 0){
    message(sprintf('Data alert: driver_gender contains %2.3f%% values not in M,F; setting to NA', 
                    100 * mean(!(d$driver_gender %in% c("M", "F", NA)))))
    d$driver_gender[!(d$driver_gender %in% c("M", "F"))] = NA
  }
  
  message("Races in dataset are (with counts)")
  print(d %>% group_by(driver_race) %>% summarise(n = n()) %>% ungroup() %>% arrange(-n) %>% mutate(percentage = round(100 * n / nrow(d), 1)))
  
  if(sum(d$contraband_found, na.rm = T) == 0){message("Data alert: no cases where contraband found = TRUE")}
  if(sum(d$search_conducted, na.rm = T) == 0){message("Data alert: no cases where search conducted = TRUE")}
  
  contraband_with_no_search = (!is.na(d$contraband_found)) & (!is.na(d$search_conducted)) & (d$contraband_found == TRUE) & (d$search_conducted == FALSE)
  stopifnot(sum(contraband_with_no_search) == 0)
  return(d)
}

# This function downloads the data for a specific state locally.
# Type:
#   'original_raw' is the raw data as provided in the original format
#   'original_csv' is the raw data converted to csv format
#   'cleaned' is the cleaned-up data, standardized across states
download_state <- function(state, type='cleaned'){
  
  if (!type %in% c('original_raw','original_csv','cleaned')) {
    print("[ERROR] Type should be one of: 'original_raw','original_csv','cleaned'")
    stop()
  }
  
  # Construct local path
  local_path = sprintf('%s/%s/', data_folder_local, state)
  if(!file.exists(local_path)) {
    system(sprintf("mkdir %s", local_path))
  }
  
  # Pull from remote location
  # TODO: change this process to pull from server where we host
  if (type %in% c('original_raw','original_csv')) {
    remote_path = sprintf('%s/%s/%s_%s.tar.gz', data_folder_remote, state, state, type)
  } else {
    remote_path = sprintf('%s/%s/%s_cleaned.gz', data_folder_remote, state, state)
  }
  cmd = sprintf('scp %s %s', remote_path, local_path)
  print(cmd)
  system(cmd)
}


# Reads in a state from the CSV in standard format. 
read_state <- function(state_code, 
                       filters = TRUE,
                       min_year = 2011,
                       max_year = 2015,
                       perform_data_checks = TRUE){
  # Reads in data in standardized way. Mostly intended for analysis in paper, but can be adapted. 
  # state_code should be a two letter state code.
  # filters: whether to filter for races and years and set race factor levels to white, black, Hispanic. Only works for states used in paper analysis. 
  # If filters is set to TRUE, we filter for stops between min_year and max_year, inclusive. 
  # perform_data_checks: does some basic sanity checks on the data and print out data alerts. Fast to run, probably a good idea, TRUE by default. 
  
  if (filters & (!(state_code %in% FINAL_STATE_LIST))){
    stop(sprintf("Error: filtering by race and setting race factor levels is only guaranteed to work for states on FINAL_STATE_LIST (ie, states under analysis); %s is not. Please set filters to FALSE", state_code))
  }
  path <- sprintf("%s/%s/%s-clean.csv.gz", data_folder_local, state_code, state_code)
  
  message(sprintf("Reading data from %s.", path))
  d <- read_csv(path, 
                progress = FALSE, 
                col_types = cols(id = col_character(), 
                                 state = col_character(), 
                                 stop_date = col_date(), 
                                 stop_time = col_character(), 
                                 location_raw = col_character(), 
                                 county_name = col_character(), 
                                 county_fips = col_character(), 
                                 fine_grained_location = col_character(), 
                                 police_department = col_character(), 
                                 driver_gender = col_character(),
                                 driver_age_raw = col_character(), 
                                 driver_age = col_double(), 
                                 driver_race_raw = col_character(), 
                                 driver_race = col_character(), 
                                 violation_raw = col_character(), 
                                 violation = col_character(), 
                                 search_conducted = col_logical(), 
                                 search_type_raw = col_character(), 
                                 search_type = col_character(), 
                                 contraband_found = col_logical(), 
                                 stop_outcome = col_character(), 
                                 is_arrested = col_logical())) %>% as.data.frame()
  
  message(sprintf("Read in individual-level data for state code %s, %i rows.", state_code, nrow(d)))
  
  if(filters) {
    #filter for driver race and year.
    race_levels = c('White', 'Black', 'Hispanic')
    d = filter(d,
               year(stop_date) >= min_year, 
               year(stop_date) <= max_year, 
               driver_race %in% race_levels)
    stopifnot(sort(unique(d$driver_race)) == sort(race_levels))
    # Set factor levels for driver_race. Ensure race counts stay unchanged when you do this. 
    race_counts = table(d$driver_race)
    d <- d %>% mutate(driver_race = factor(driver_race, levels = race_levels))
    new_race_counts = table(d$driver_race)
    stopifnot(new_race_counts['White'] == race_counts['White']) # make sure still unchanged. 
    stopifnot(new_race_counts['Black'] == race_counts['Black']) 
    stopifnot(new_race_counts['Hispanic'] == race_counts['Hispanic']) 
  }
  
  if(nrow(d) == 0){
    message("No rows remaining after filtering; returning NULL.")
    return(NULL)
  }
  
  #check the data. 
  if(perform_data_checks){
    d = data_check(d)
  }
  return(d)
}

##
## Formatting
##

# Contruct a Date-like month variable
get_month <- function(ds) {
  ds %>%
    substr(1, 7) %>%
    paste0('-01') %>%
    as.Date()
}

# Prints a probability as a percentage
pct <- function(x, digits=1) {
  paste0(round(x*100, digits), '%')
}

# Prints a big number as .k, .M, .B
big_round <- function(x, digits=1) {
  if(!is.numeric(x)) { return(NA) }
  if(x<10**3 ) { return(x) }
  if(x<10**6 ) { return(paste0(round(x/10**3, digits), 'k')) }
  if(x<10**9 ) { return(paste0(round(x/10**6, digits), 'M')) }
  if(x<10**12) { return(paste0(round(x/10**9, digits), 'B')) }
  # too big
  return(x)
}

# Returns string w/o leading or trailing whitespace
trim <- function (x) {
  gsub("^\\s+|\\s+$", "", x)
}
