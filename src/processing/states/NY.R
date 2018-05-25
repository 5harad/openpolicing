# Processing code for New York

# Set-up
this_state <- 'NY'
change_path(this_state)

# Read in and combine data
print(sprintf("[%s] reading in the data", this_state))
d <- lapply(list.files('.'), function(fn) read_csv(fn) ) %>%
  bind_rows() %>%
  separate(VIOLATION_DATE, into=c("date","x1"), sep=' ') %>%
  separate(VIO_TIME, into=c("x2","time"), sep=' ')

race_keys <- c("1 - BLACK","2 - WHITE","3 - HISPANIC","4 - ASIAN - EAST INDIAN","5 - NATIVE AMERICAN","6 - OTHER")
race_vals <- c("Black", "White", "Hispanic", "Asian", "Other","Other")


# Rename and extract columns
print(sprintf("[%s] extracting columns", this_state))
d$state                 <- this_state
d$stop_date             <- make_date(d$date, format='%m/%d/%y')
d$stop_time             <- substr(d$time, 1, 5)
d$id                    <- make_row_id(d)
d$location_raw          <- d$COUNTY
counties_clean          <- normalize_county(d)
d$county_name           <- counties_clean$county_name
d$county_fips           <- counties_clean$fips
d$fine_grained_location <- d$VIO_STREET
d$state_patrol          <- TRUE
d$police_department     <- NA  # not included
d$driver_gender         <- d$GENDER
d$driver_age_raw        <- d$AGE
d$driver_age            <- get_age(d, type='age')
d$driver_race_raw       <- d$RACE
d$driver_race           <- map(d$driver_race_raw, race_keys, race_vals)
d$violation_raw         <- normalize_violation(d, d$DCJS_CODE, clean=FALSE)
d$violation             <- normalize_violation(d, d$DCJS_CODE, clean=TRUE)
d$search_conducted      <- NA  # not included
d$search_type_raw       <- NA  # not included
d$search_type           <- NA  # not included
d$contraband_found      <- NA  # not included
d$stop_outcome          <- NA  # not included
d$is_arrested           <- NA  # not included

# Extra fields
d$out_of_state          <- d$REG_STATE != 'NY'
d$vehicle_type          <- paste(str_to_title(d$MAKE), str_to_title(d$MODEL), str_to_title(d$VEH_COLOR), d$YEAR)
# also included: speed, posted speed

# Close-up
write_cleaned_state(d)
change_path(NA)
