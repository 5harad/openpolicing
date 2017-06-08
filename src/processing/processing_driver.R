# This script contains helper functions for the state processing code.


##
## Main processing function
##

process_state <- function(state) {
  fn <- sprintf("%s/src/processing/states/%s.R", code_path, state)
  print(sprintf("Processing %s", state))
  print(sprintf("[%s] running %s", state, fn))
  source(fn)
}


##
## I/O Functions
##

# Write out a state's cleaned up data and move back to the main path
write_cleaned_state <- function(data, extra_cols=c()) {

  state <- data$state[1]
  fn_out <- sprintf("%s/%s/%s-clean.csv", data_folder_local, state, state)
  print(sprintf("[%s] writing out the cleaned data to %s", state, fn_out))
  
  # Do various preprocessing / standardizations which should apply across all states. 
  data = filter(data, state_patrol == TRUE) # filter for state patrol stops only. 
  stopifnot(sum(data$state_patrol == TRUE) == nrow(data))
  data$id = make_row_id(data) # redo IDs so they are consecutive for state patrol stops. 
  data = arrange(data, id) # sort by id. 
  stopifnot(sort(data$id) == data$id)
  stopifnot(sum(duplicated(data$id)) == 0)
  
  # set contraband found to FALSE if no search 
  # since we define contraband_found to be contraband found as the result of a search
  if(sum(data$contraband_found[data$search_conducted == FALSE], na.rm = T) > 0){
    message(sprintf("[WARNING]: in %s, %i hits with no searches; setting these to FALSE.", 
            state,
            sum(data$contraband_found[data$search_conducted == FALSE], na.rm = T)))
  }
  data$contraband_found[data$search_conducted == FALSE] = FALSE 
  
  # set search_type to NA if no search. 
  if(sum(!is.na(data$search_type[data$search_conducted == FALSE])) > 0){
    message(sprintf("[WARNING]: in %s, some search types are not NA even when search is not conducted; setting these to NA.", state))
    print(table(data$search_type[data$search_conducted == FALSE], useNA = 'ifany'))
  }
  data$search_type[data$search_conducted == FALSE] = NA 
  
  # Construct the columns to extract
  cols <- c(
    'id',
    'state',
    'stop_date',
    'stop_time',
    'location_raw',
    'county_name',
    'county_fips',
    'fine_grained_location',
    'police_department',
    'driver_gender',
    'driver_age_raw',
    'driver_age',
    'driver_race_raw',
    'driver_race',
    'violation_raw',
    'violation',
    'search_conducted',
    'search_type_raw',
    'search_type',
    'contraband_found',
    'stop_outcome',
    'is_arrested'
  )
  
  # add in extra columns
  cols = c(cols, extra_cols)
  
  # Extract the columns
  data <- data[, cols]
  
  # Write data to file
  write_csv(data, fn_out, na='')

  # Compress the data
  print(sprintf("[%s] compressing the cleaned output file", state))
  cmd <- sprintf("gzip -9 --keep --force %s", fn_out)
  system(cmd)
}

# Returns a dataframe with various useful time-of-day and time-of-year
# variables which we group_by and use in regressions.
add_time_variables <- function(d){
  #extract stop year.
  d$stop_year = as.factor(as.character(year(d$stop_date)))
  d$stop_year[as.character(d$stop_year) > as.character(year(today()))] = NA
  
  #extract month and year. Cast this to date for convenience in plotting.
  d$month_and_year = as.Date(paste0(substr(d$stop_date, 1, 7), '-01')) # stop month and year are given by first 7 digits. 

  # compute stop quarter (Q1 - Q4). 
  d$stop_quarter = quarters(d$stop_date)  
  d$stop_quarter[is.na(d$stop_date)] = NA
  d$stop_quarter = as.factor(d$stop_quarter)
  
  # extract weekday. 
  d$weekday = as.factor(weekdays(d$stop_date))
  
  # bin hour into 8 3-hour bins to create stop_hour_categorical. 
  d$stop_hour = as.numeric(substr(d$stop_time, 1, 2))
  d$stop_hour_categorical = paste0('hour_category_', floor(d$stop_hour / 3) * 3)
  d$stop_hour_categorical[is.na(d$stop_hour)] = NA
  d$stop_hour_categorical = as.factor(d$stop_hour_categorical)
  
  return(d)
}


##
## Helper functions
##

# Determine when state data was last updated. 
print_state_creation_dates = function(){
  states_to_use = list.files(data_folder_local)
  states_to_use = states_to_use[(nchar(states_to_use) == 2)]
  file_info = file.info(sprintf('%s/%s/%s-clean.csv', 
                          data_folder_local,
                          states_to_use, 
                          states_to_use))
  file_info$state = states_to_use
  file_info = file_info %>% select(state, mtime, uname) %>% arrange(mtime) %>% mutate(mtime = as.character(mtime))
  print(file_info)
  return(file_info)
}


# Change between state data path and main path
# If state=NA, then change back to main path
change_path <- function(state) {
  if(is.na(state)){
    setwd(code_path)
  } else {
    path <- sprintf("%s/%s/%s_original_csv", data_folder_local, state, state)
    setwd(path)
  }
}

# Construct a unique identifier for each row. The format is STATE-YEAR-ID.
# Stop are assigned an identifier by chronological order.
make_row_id <- function(df) {
  df %>%
    # ungroup to be safe because some of the state dataframes have grouping in preprocessing.
    ungroup() %>%
    mutate(
      # add row number
      i = 1:nrow(df),
      # add year column
      year=year(stop_date)
    ) %>%
    # sort by stop time
    group_by(year) %>%
    arrange(stop_date, stop_time) %>%
    mutate(
      # renew row numbers by year
      id=row_number(),
      # pad with zeros
      id=str_pad(id, nchar(max(id)), pad='0')
    ) %>%
    # construct id
    mutate(ids=paste(state, year, id, sep='-')) %>%
    # sort by original row number
    arrange(i) %>%
    # return the ids
    .$ids
}

# Creates a date field
make_date <- function(ds, format=NA, min_ds=as.Date('1995-01-01')) {
  last_ds  = today()
  if(!is.na(format)) {
    ds = as.Date(ds, format)
  } else {
    ds = as.Date(ds)
  }
  # unfortunately ifelse loses the Date type, so we have to reset
  structure(ifelse(ds < min_ds | ds > last_ds, NA, ds), class='Date')
}

# Converts a birthdate to age. Can be one of three types:
# 'age' - the driver_age_raw already contains age
# 'birthyear' - the driver_age_raw field contains the year of birth, so subtract this from the stop date.
# 'birthdate' - the driver_age_raw field contains the date of birth, so compute the interval between this and the stop date. 
# if birthdate, can optionally pass in a format. 
get_age <- function(df, type='age', format=NA) {
  # convert to int for age
  stopifnot(type %in% c('age', 'birthyear', 'birthdate'))
  if(type=='age') {
    age = as.numeric(df$driver_age_raw)
  }
  # subtract for just birth year
  if(type=='birthyear') {
    age = year(df$stop_date) - df$driver_age_raw
  }
  # compute actual interval for birthdate
  if(type=='birthdate') {
    if(!is.na(format)) {
      birthdate = as.Date(df$driver_age_raw, format)
    } else {
      birthdate = as.Date(df$driver_age_raw)
    }
    age = as.numeric(interval(birthdate, df$stop_date) %/% years(1))
  }
  ifelse(age > 14 & age < 100, age, NA)
}

# Buckets age into age groups
bucket_age <- Vectorize(function(age) {
  x <- ifelse(age >= 16 & age < 20, '16-19',
       ifelse(age >= 20 & age < 30, '20-29',
       ifelse(age >= 30 & age < 40, '30-39',
       ifelse(age >= 40 & age < 50, '40-49',
       ifelse(age >= 50, '50+', NA)))))
  return(x)
})

# Creates a new (empty) column with given name as given location. 
# This is only used for Illinois. 
add_new_col <- function(df, col_name, position) {
  ncols = dim(df)[2]
  # Make sure position is in range. 
  stopifnot((position > 1) & (position <= ncols)) 
  df$NEW = NA
  df = df[, c(1:(position-1), ncols + 1, position:ncols)]
  colnames(df)[position] <- col_name
  return(df)
}


##
## Mapping functions
##

# Multimap is like plyr::map, but maps multiple values per entry (so "A,B" becomes "A_mapped,B_mapped")
# id: the id column of a dataframe. 
# col: the column of values to be mapped. 
# keys: the values to map FROM. 
# vals: the values to map TO. 
# sep: the separator between values. 
multimap <- function(id, col, keys, vals, sep=','){
   # construct temporary data.frame
  df_expand <- data.frame(
      id=id,
      col=col,
      stringsAsFactors=FALSE
    ) %>%
    # separate rows by separator
    separate_rows(col, sep=sep) %>%
    # map 'NA' strings to NA values
    mutate(col = ifelse(col == 'NA', NA, col))
  # map the individual rows
  df_expand$mapped <- plyr::mapvalues(df_expand$col, keys, vals)
  # put it back together
  df_expand <- df_expand %>%
    distinct(id, mapped) %>%
    # sort by key
    arrange(id, mapped) %>%
    group_by(id) %>%
    # only keep NA if it's the only one
    filter(n() == 1 | !is.na(mapped)) %>%
    summarize(mapped=paste(mapped, collapse=','))
  # clean up blank values or "NA" strings. 
  df_expand$mapped[(nchar(df_expand$mapped) == 0) | (df_expand$mapped == "NA")] <- NA
  # make sure mapped values are in the right order (this deals with case where id is not in sorted order).
  stopifnot(sort(df_expand$id) == sort(id))
  mapped <- df_expand$mapped[match(id, df_expand$id)] 
  return(mapped)
}

# Convert raw location strings to normalized county strings and FIPS id
normalize_county <- function(d_in){
  # read in CSV that maps county names to FIPS. 
  counties_index = read_csv(paste0(code_path, "/resources/county_index.csv"), col_types='cccccc')  # national index of counties
  # read in CSV that maps raw location values to county names (created specifically for this project).
  map_county = read_csv(paste0(code_path, "/resources/dictionaries/county.csv"), col_types='ccc')  # county mapping
  this_state <- d_in$state[1]
  # construct temporary data.frame
  df <- data.frame(
    location_raw=d_in$location_raw,
    fips=NA,
    stringsAsFactors=FALSE
  ) %>%
  # first pass at cleaning county name 
  mutate(
    # capitalize original value
    county_name = str_to_title(location_raw),
    # append 'County' if not there already
    # don't do this for super-short names because they are districts or other codes.
    county_name = ifelse(nchar(county_name) > 2 & !(grepl('County', county_name)), 
                         paste(county_name, 'County'), 
                         county_name)
  )
  # join with manual mapping to map and normalize spelling
  map_countyF <- map_county %>% filter(state==this_state)
  new_c = map_countyF$county_name[match(df$location_raw, map_countyF$location_raw)]
  df = df %>% mutate(county_name=coalesce(new_c, county_name))
  # join with national mapping to get FIPS code
  counties_indexF <- counties_index %>% filter(state==this_state)
  df$fips <- counties_indexF$fips[match(str_to_title(df$county_name), str_to_title(counties_indexF$county_name))]
  df$county_name[is.na(df$fips)] <- NA 
  return(df)
}


# Convert the 'col' vector, which contains raw violation strings, to normalized values.
# If 'clean' is FALSE, it will map the codes to human-interpretable values provided by a dictionary, or the state's legal code.
# If 'clean' is set to TRUE, it will map the codes to our standardized categorization of violation reasons. 
# Unmapped (but non-NA) codes will be mapped to 'Other (non-mapped)'
normalize_violation <- function(d_in, col, clean=FALSE) {
  this_state = d_in$state[1]
  # construct temporary data.frame
  df <- data.frame(
    id=d_in$id,
    violation=NA,
    violation_raw=col,
    stringsAsFactors=FALSE
  )
  # join with manual mapping to normalize spelling
  # first filter for violations specific to the state. 
  map_violation = read_csv(paste0(code_path, "/resources/dictionaries/violations.csv"), col_types='cccc')  # violation mapping
  map_violationF <- map_violation %>% filter(state==this_state)
  if(clean) {
    # get the values in the _clean column
    df$violation <- map_violationF$violation_clean[match(df$violation_raw, map_violationF$violation_raw)]
  } else {
    # get the mapped values
    df$violation <- map_violationF$violation[match(df$violation_raw, map_violationF$violation_raw)]
  }
  df %>%
    mutate(
      # Replace non-NA value with 'Other'
      violation = ifelse(is.na(violation) & (!is.na(violation_raw) & (violation_raw != 'NA')), 'Other (non-mapped)', violation)
    ) %>%
    # return the value
    .$violation
}


# Convert raw violation strings with multiple values to normalized values
# similar to multimap, above. 
normalize_violation_multiple <- function(df, col, clean=FALSE, sep=',') {
  # construct temporary data.frame
  df_expand <- data.frame(
      id=df$id,
      state=df$state,
      violation_raw=col,
      stringsAsFactors=FALSE
    ) %>%
    # separate rows by separator
    separate_rows(violation_raw, sep=sep)
  # map the individual rows
  df_expand$violation <- normalize_violation(df_expand, df_expand$violation_raw, clean)
  # put it back together
  df_expand = df_expand %>%
    distinct(id, violation) %>%
    # sort by violation
    arrange(id, violation) %>%
    group_by(id) %>%
    summarize(violation=paste(na.omit(violation), collapse=',')) %>% 
    ungroup()
  # clean up NA strings. 
  df_expand$violation[(df_expand$violation == 'NA') | nchar(df_expand$violation) == 0] = NA
  # make sure mapped values are in the right order (this deals with case where id is not in sorted order).
  stopifnot(sort(df_expand$id) == sort(df$id))
  violation = df_expand$violation[match(df$id, df_expand$id)] 
  return(violation)
}


# Returns the unique value if there's a unique value, "" otherwise. 
get_unique_value_if_exists <- function(x){
  ifelse(length(unique(na.omit(x))) == 1, first(x), '')
}
