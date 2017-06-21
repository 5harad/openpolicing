# Processing code for Vermont

# Set-up
this_state <- 'VT'
change_path(this_state)

# Read in and combine data
print(sprintf("[%s] reading in the data", this_state))
d <- read_csv("vsp_traffic_stops_20160218_public_sheet1.csv") %>%
  # split date column
  separate(`Stop Date`, c("date", "time"), sep="  ")

# Value dictionaries

# Race - inspect with : data.frame(from=race_keys, to=race_vals)
race_keys <- c("A", "B", "H", "I", "U", "W", "X", "F", "M", "N")
race_vals <- c("Asian", "Black", "Hispanic", "Other", NA, "White", NA, NA, NA, NA)
search_type_keys <- c("NS","SPC","SRS","SW", "_PSS")
search_type_vals <- c(NA, "Consent Search - Probable Cause", "Consent Search - Reasonable Suspicion", "Warrant", NA)
outcome_keys <- c("A","AW","N","T","V","W")
outcome_vals <- c("Arrest for Violation","Warrant Arrest",NA, "Citation","Verbal Warning","Written Warning")

# Rename and extract columns
print(sprintf("[%s] extracting columns", this_state))

d$state                 <- this_state
d$stop_date             <- make_date(d$date, "%m/%d/%Y")
d$stop_time             <- strftime(strptime(d$time, "%I:%M:%S %p"), format='%H:%M') # convert time to 24H
d$id                    <- make_row_id(d)
d$location_raw          <- d$`Stop City`
counties_clean          <- normalize_county(d)
d$county_name           <- counties_clean$county_name
d$county_fips           <- counties_clean$fips
d$fine_grained_location <- d$`Stop Address`
d$state_patrol          <- TRUE
d$police_department     <- d$`Agency Name`
d$driver_gender         <- ifelse(d$`Driver Gender` %in% c('M','F'), d$`Driver Gender`, NA)
d$driver_age_raw        <- d$`Driver Age`
d$driver_age            <- get_age(d)
d$driver_race_raw       <- d$`Driver Race Description`
d$driver_race           <- map(d$`Driver Race`, race_keys, race_vals)  # extract race with dictionary approach
d$violation_raw         <- d$`Stop Reason Description`
d$violation             <- normalize_violation(d, d$`Stop Reason Description`, clean=TRUE)
d$search_conducted      <- d$`Stop Search` %in% c("SPC","SRS", "SW", "(Winooski) Passgr Srch")
d$search_type_raw       <- d$`Stop Search Description`
d$search_type           <- map(d$`Stop Search`, search_type_keys, search_type_vals)
d$contraband_found      <- d$`Stop Contraband Description` == 'Contraband/Evidence Found'
d$contraband_found[d$search_conducted == FALSE] = FALSE#there are a bunch of nans in contraband_found; we set those where no search was conducted to FALSE. 
d$stop_outcome          <- map(d$`Stop Outcome`, outcome_keys, outcome_vals)
d$is_arrested           <- d$`Stop Outcome Description` %in% c('Arrest for Violation', 'Arrest on Warrant')
# Extra columns
d$officer_id            <- d$`Officer ID`  # they are sometimes negative?

# Close-up
write_cleaned_state(d, extra_cols=c('officer_id'))
change_path(NA)
