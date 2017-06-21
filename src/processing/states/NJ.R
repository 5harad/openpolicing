# Processing code for New Jersey

# Set-up
this_state <- 'NJ'
change_path(this_state)

# read in CAD files
column_names <- c('CAD_INCIDENT', 'UNIT', 'DATE_TIME', 'TOWNSHIP', 'LOCATION',
                  'BADGE', 'ACTION', 'STATUTE', 'INVOLVEMENT', 'GENDER', 'RACE',
                  'VEH_STATE', 'VEH_MAKE', 'VEH_MODEL', 'VEH_COLOR')
column_names <- c(paste0('X', 1:15), column_names) # first 15 columns are just the column names. 

d <- list.files('.', pattern='cad*') %>%
  lapply(function(x){
    message(sprintf("Reading file %s", x));
    # records after 2011 sometimes have 32 columns rather than 30, but the last two columns don't contain useful information
    read_csv(x, col_types=paste0(rep('c', 30), collapse=''), col_names=column_names, na=c("", "NA", "N/A")) %>%
    # only take the columns with data
    select(16:30)
  }) %>%
  bind_rows() %>%
  distinct() %>% # first remove rows which are identical duplicates of other rows.
  filter(INVOLVEMENT %in% c('DRIVER', 'OCCUPANT')) # only take rows corresponding to a driver or passenger (small number of rows with pedestrians etc)

message(sprintf("Prior to group by, %i rows", nrow(d)))
for(c in colnames(d)){
  message(sprintf("%s is NA proportion %2.5f of the time",c, mean(is.na(d[,c]))))
}

d = d %>%
  # Then remove duplicates as rows refers to individual people and violations
  group_by(CAD_INCIDENT) %>%
  summarize(
    # get gender and race of the driver
    GENDER  = get_unique_value_if_exists(GENDER[INVOLVEMENT %in% c('DRIVER')]), 
    RACE    = get_unique_value_if_exists(RACE[INVOLVEMENT %in% c('DRIVER')]),
    # for other fields, take the unique value if one exists; otherwise, set to NA
    UNIT = get_unique_value_if_exists(UNIT),
    DATE_TIME = get_unique_value_if_exists(DATE_TIME),
    TOWNSHIP = get_unique_value_if_exists(TOWNSHIP),
    LOCATION = get_unique_value_if_exists(LOCATION),
    BADGE = get_unique_value_if_exists(BADGE),
    VEH_STATE = get_unique_value_if_exists(VEH_STATE),
    VEH_MAKE = get_unique_value_if_exists(VEH_MAKE),
    VEH_MODEL = get_unique_value_if_exists(VEH_MODEL),
    VEH_COLOR = get_unique_value_if_exists(VEH_COLOR),
    ACTION  = paste(unique(ACTION ), collapse=','), # concatenate action and violation, consistent with other states.
    STATUTE = paste(sort(unique(na.omit(STATUTE))), collapse='|') # make sure statutes are in sorted order. 
  ) %>%
  ungroup() %>% 
  # separate date time
  separate(DATE_TIME, c("date", "time"), "  ")
d$STATUTE[nchar(d$STATUTE) == 0] = NA # clean up blank statute names. 



message(sprintf("After group by, %i rows", nrow(d)))
for(c in colnames(d)){
  message(sprintf("%s is NA proportion %2.5f of the time", c, mean(is.na(d[,c]))))
}

# Dictionaries
race_keys <- c('AI - AMERICAN INDIAN', 'AS - ASIAN INDIAN', 'B - BLACK', 'H - HISPANIC', 'NP - NOT PROVIDED',
               'OA - OTHER ASIAN', 'UA - UNATTENDED', 'UO - UNABLE TO OBSERVE', 'W - WHITE', '')
race_vals <- c('Other', 'Asian', 'Black', 'Hispanic', NA, 'Asian', NA, NA, 'White', NA)

# Rename and extract columns
print(sprintf("[%s] extracting columns", this_state))
d$state                 <- this_state
d$stop_date             <- make_date(d$date, format='%m/%d/%Y')
d$stop_time             <- strftime(strptime(d$time, "%I:%M:%S%p"), format='%H:%M')
d$id                    <- make_row_id(d)
d$location_raw          <- as.character(d$TOWNSHIP)
counties_clean          <- normalize_county(d)
d$county_name           <- counties_clean$county_name
d$county_fips           <- counties_clean$fips
d$fine_grained_location <- d$LOCATION
d$state_patrol          <- TRUE
d$police_department     <- d$UNIT
d$driver_gender         <- ifelse(substr(d$GENDER, 1, 1) %in% c('F','M'), substr(d$GENDER, 1, 1), NA)
d$driver_age_raw        <- NA
d$driver_age            <- NA
d$driver_race_raw       <- d$RACE
d$driver_race           <- map(d$driver_race_raw, race_keys, race_vals)
d$violation_raw         <- d$STATUTE
d$violation             <- normalize_violation_multiple(d, d$STATUTE, clean=TRUE, sep = '\\|')
d$search_conducted      <- NA
d$search_type_raw       <- NA
d$search_type           <- NA
d$contraband_found      <- NA
d$stop_outcome          <- ifelse(grepl('SUMMONS' , d$ACTION), 'Summons',
                           ifelse(grepl('REPORT'  , d$ACTION), 'Report',
                           ifelse(grepl('BOARDING', d$ACTION), 'Boarding',
                           ifelse(grepl('WARNING' , d$ACTION), 'Warning', NA))))
d$is_arrested           <- NA

# Extra fields
d$officer_id            <- d$BADGE
d$out_of_state          <- d$VEH_STATE == 'NJ'
d$vehicle_make          <- d$VEH_MAKE
d$vehicle_model         <- d$VEH_MODEL
d$vehicle_color         <- d$VEH_COLOR

# Close-up
write_cleaned_state(d, extra_cols=c('officer_id','out_of_state','vehicle_make','vehicle_model','vehicle_color'))
change_path(NA)
