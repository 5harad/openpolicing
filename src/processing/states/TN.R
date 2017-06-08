# Processing code for Tennessee

# Set-up
this_state <- 'TN'
change_path(this_state)

# Read in and combine data
print(sprintf("[%s] reading in the data", this_state))
d <- read_csv("CitationFileSep0216.csv", na=c('','NA','NULL'), col_types = cols(AGY_CDE = col_character()))

# Value dictionaries
race_keys <- c("","A","B","C","H","I","J","O","R","W")
race_vals <- c(NA,"Asian","Black","Asian","Hispanic","Native American","Asian","Other","Unknown","White")
race_vals_clean <- c(NA, "Asian","Black","Asian","Hispanic","Other","Asian","Other",NA,"White")

# Rename and extract columns
print(sprintf("[%s] extracting columns", this_state))

d$state                 <- this_state
d$stop_date             <- make_date(d$VIOL_EVNT_DTE)
d$stop_time             <- strftime(strptime(sprintf(
'%04d',
ifelse(is.na(d$AM_PM_IND), d$VIOL_TME,  # Assume times with AM/PM flag are in 24-hour time
       ifelse(d$AM_PM_IND == 'P',
              ifelse(d$VIOL_TME<1200, d$VIOL_TME+1200,d$VIOL_TME),
              d$VIOL_TME))), "%H%M"), format = '%H:%M')
d$id                    <- make_row_id(d)
d$location_raw            <- d$CNTY_NBR
counties_clean          <- normalize_county(d)
d$county_name           <- counties_clean$county_name
d$county_fips           <- counties_clean$fips
d$fine_grained_location <- NA  # not included
d$state_patrol          <- TRUE
d$police_department     <- NA  # not included
d$driver_gender         <- ifelse(d$SEX_IND %in% c("M","F"), d$SEX_IND, NA)
d$driver_age_raw        <- NA  # not included
d$driver_age            <- NA  # not included
d$driver_race_raw       <- map(d$RACE_IND, race_keys, race_vals)
d$driver_race           <- map(d$RACE_IND, race_keys, race_vals_clean)
d$violation_raw         <- normalize_violation(d, d$ORIG_TRFC_VIOL_CDE)
d$violation             <- normalize_violation(d, d$ORIG_TRFC_VIOL_CDE, clean=TRUE)
d$search_conducted      <- NA  # not included
d$search_type_raw       <- NA  # not included
d$search_type           <- NA  # not included
d$contraband_found      <- NA  # not included
d$stop_outcome          <- "Citation"
d$is_arrested           <- NA  # not included

# Extra fields
d$road_number           <- d$UP_STR_HWY
d$milepost              <- d$MLE_MRK_NBR

# Close-up
write_cleaned_state(d, extra_cols=c('road_number','milepost'))
change_path(NA)
