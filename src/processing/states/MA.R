# Processing code for Massachusetts

# Set-up
this_state <- 'MA'
change_path(this_state)

# Read in and combine data
print(sprintf("[%s] reading in the data", this_state))
d <- read_csv("FOI_Req.csv")

# Value dictionaries
race_keys   <- c("A","American Indian or Alaskan Native", "Asian or Pacific Islander",
	             "Middle Eastern or East Indian (South Asian)",
	             "None - for no operator present citations only", "")
race_vals   <- c("Asian", "Other", "Asian", "Other", NA, NA)
gender_keys <- c("Female", "Male", "None", "White")
gender_vals <- c("F", "M", NA, NA)
search_type_keys   <- c(NA, "Incident to Arrest", "Probable Cause", "Consent Search",  "Terry Frisk")  
search_type_vals <- c(NA, "Incident to Arrest", "Probable Cause", "Consent", "Protective Frisk")  


# Rename and extract columns
print(sprintf("[%s] extracting columns", this_state))
d$state                 <- this_state
d$stop_date             <- make_date(d$DateIssue)
d$stop_time             <- NA  # not included
d$id                    <- make_row_id(d)
d$location_raw            <- d$CITY_TOWN_NAME
counties_clean          <- normalize_county(d)
d$county_name           <- counties_clean$county_name
d$county_fips           <- counties_clean$fips
d$fine_grained_location <- paste('Route', d$Route)
d$fine_grained_location[is.na(d$Route)] = NA
d$state_patrol          <- TRUE
d$police_department     <- NA  # not included
d$driver_gender         <- map(trim(d$Sex), gender_keys, gender_vals)  # map gender with dict
d$driver_age_raw        <- 1900 + as.numeric(d$DOBYr)
d$driver_age            <- get_age(d, type='birthyear')
d$driver_race_raw       <- trim(d$Race)
d$driver_race           <- map(d$driver_race_raw, race_keys, race_vals)  # map race with dict
d$violation_raw         <- NA  # booleans
d$violation             <- ifelse(d$Speed == 1,
                             ifelse(d$SeatBelt == 1, 'Speeding,Seat belt', 'Speeding'),
                             ifelse(d$SeatBelt == 1, 'Seat belt', NA))
# SearchYN and SearchDescr are inconsistent, we take the most progressive interpretation
d$search_conducted      <- (d$SearchYN == 'Yes') | (!is.na(d$SearchDescr))
d$search_conducted[is.na(d$search_conducted)] <- FALSE
d$search_type_raw       <- d$SearchDescr
d$search_type           <- map(d$search_type_raw, search_type_keys, search_type_vals)
d$contraband_found      <- (d$RsltSrchDrg + d$RsltSrchAlc + d$RsltSrchWpn + d$RsltSrchMny + d$RsltSrchOth) > 0
d$contraband_found[d$search_conducted == FALSE] = FALSE
d$stop_outcome          <- d$ResultDescr  
d$is_arrested           <- d$ResultDescr == 'Arrest'

# Extra fields
d$out_of_state          <- d$State != 'MA'

# Close-up
write_cleaned_state(d, extra_cols=c('out_of_state'))
change_path(NA)
