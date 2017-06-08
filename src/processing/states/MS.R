# Processing code for Mississippi

# Set-up
this_state <- 'MS'
change_path(this_state)

# Read in and combine data
print(sprintf("[%s] reading in the data", this_state))
d <- read_csv('citpentx_sheet1.csv') %>%
  # agency field sometimes misses leading zeros
  mutate(agency=str_pad(agency, 4, pad='0'))

# Value dictionaries
race_keys = c("B","I","O","W","Y")
race_vals = c("Black","American Indian","Other","White","Asian/Pacific Island")
race_vals_clean = c("Black","Other","Other","White","Asian")
map_agencies <- read_csv('agencies.csv') %>%
  mutate(code = as.character(substr(`Agency code`, 1, 4)))

# Rename and extract columns
print(sprintf("[%s] extracting columns", this_state))
d$state                 <- this_state
d$stop_date             <- make_date(d$tikdate)
d$stop_time             <- NA  # not included
d$id                    <- make_row_id(d)
# Counties are two-digits, but the location depends on what kind of location
# code is used (police department, sherrif office, highway patrol).
d$location_raw            <- ifelse(substr(d$agency, 1, 2) %in% c('00','90'),
	                         substr(d$agency, 3, 4), substr(d$agency, 1, 2))
counties_clean          <- normalize_county(d)
d$county_name           <- counties_clean$county_name
d$county_fips           <- counties_clean$fips
d$fine_grained_location <- NA  # not included
d$state_patrol          <- substr(d$agency, 1, 2) == '90'
d$police_department     <- ifelse(substr(d$agency, 1, 2) == '90', 'Mississippi Highway Patrol',
	                       ifelse(substr(d$agency, 1, 2) == '00', paste(d$county_name, 'Sherrif Office'),
	                       map(d$agency, map_agencies$code, map_agencies$`Agency Name`)))
d$driver_gender         <- ifelse(d$sex %in% c('M','F'), d$sex, NA)
d$driver_age_raw        <- d$dob
d$driver_age            <- get_age(d, type='birthdate')
d$driver_race_raw       <- map(d$race, race_keys, race_vals)
d$driver_race           <- map(d$race, race_keys, race_vals_clean)
d$violation_raw         <- normalize_violation(d, d$acd, clean=FALSE)
d$violation             <- normalize_violation(d, d$violation_raw, clean=TRUE)
d$search_conducted      <- NA  # not included
d$search_type_raw       <- NA  # not included
d$search_type           <- NA  # not included
d$contraband_found      <- NA  # not included
d$stop_outcome          <- NA  # not included
d$is_arrested           <- NA  # not included

# Extra fields
d$officer_id           <- d$badge

# Close-up
write_cleaned_state(d, extra_cols=c('officer_id'))
change_path(NA)
