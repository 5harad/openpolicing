# Processing code for Connecticut

# Set-up
this_state <- 'CT'
change_path(this_state)

# Read in and combine data
print(sprintf("[%s] reading in the data", this_state))
d1 <- read_csv("connecticut-r1.csv", col_types=cols(InterventionTime=col_character(), InterventionIdentificationID=col_character()))
d2 <- read_csv("connecticut-r2.csv", col_types=cols(InterventionTime=col_character(), InterventionIdentificationID=col_character()))
colnames(d2)[26:27] = c("StatutoryReasonForStop","StatutatoryCitationPostStop")
d <- rbind(d1, d2) %>%
       # separate date and time
       separate(InterventionDateTime, c("date","time"), sep=' ', remove=F)

# Combine duplicates
message(sprintf("Prior to combining duplicates, %i rows.", nrow(d)))
d <- d %>% group_by(
  date, InterventionTime, InterventionLocationName, `Department Name`, InterventionLocationDescriptionText,
  SubjectEthnicityCode, SubjectAge, SubjectRaceCode, SubjectSexCode,
  SearchAuthorizationCode, VehicleSearchedIndicator, ContrabandIndicator, CustodialArrestIndicator,
  ReportingOfficerIdentificationID, InterventionDurationCode,
  InterventionLocationLatitude, InterventionLocationLongitude) %>%
  summarize(StatutoryReasonForStop = paste(sort(unique(StatutoryReasonForStop)), collapse=','), 
            InterventionDispositionCode = paste(InterventionDispositionCode, collapse = ',')) %>%
  ungroup()
message(sprintf("After combining duplicates, %i rows.", nrow(d)))

# Value dictionaries
race_keys <- c("W", "B", "A", "H", "I")
race_vals <- c("White", "Black", "Asian", "Hispanic", "Native American")
race_vals_clean <- c("White", "Black", "Asian", "Hispanic", "Other")
outcome_keys <- c("I","M","N","U","V","W")
outcome_vals <- c("Ticket","Summons",NA,"Arrest","Verbal Warning","Written Warning") 
search_type_keys <- c("C","I","N","O")
search_type_vals <- c("Consent", "Inventory", NA, "Probable Cause")
search_type_vals_clean <- c("Consent", "Inventory", NA, "Probable Cause")

# Rename and extract columns
print(sprintf("[%s] extracting columns", this_state))
d$state                 <- this_state
d$stop_date             <- make_date(d$date)
d$stop_time             <- strftime(strptime(d$InterventionTime, "%H:%M"), format = '%H:%M')
d$stop_time[as.character(d$stop_time) == '00:00'] = NA  # We have an overdensity of stops at midnight; this probably indicates unreliable data, setting these to NA. 
d$id                    <- make_row_id(d)
d$location_raw          <- tolower(d$InterventionLocationName)
counties_clean          <- normalize_county(d)
d$county_name           <- counties_clean$county_name
d$county_fips           <- counties_clean$fips
d$fine_grained_location <- d$InterventionLocationDescriptionText
d$state_patrol          <- d$`Department Name` == 'State Police'
d$police_department     <- d$`Department Name`
d$driver_gender         <- d$SubjectSexCode
d$driver_age_raw        <- d$SubjectAge
d$driver_age            <- get_age(d, type='age')
d$driver_race_tmp       <- ifelse(d$SubjectEthnicityCode == 'H', d$SubjectEthnicityCode, d$SubjectRaceCode)
d$driver_race_raw       <- map(d$driver_race_tmp, race_keys, race_vals)
d$driver_race           <- map(d$driver_race_tmp, race_keys, race_vals_clean)
d$violation_raw         <- d$StatutoryReasonForStop
d$violation             <- normalize_violation_multiple(d, d$StatutoryReasonForStop, clean=TRUE)
d$search_conducted      <- (d$VehicleSearchedIndicator == 'True') | (d$SearchAuthorizationCode != 'N')
d$search_type_raw       <- map(d$SearchAuthorizationCode, search_type_keys, search_type_vals)
d$search_type           <- map(d$SearchAuthorizationCode, search_type_keys, search_type_vals_clean)
d$contraband_found      <- d$ContrabandIndicator == 'True'
d$contraband_found[!d$search_conducted] <- FALSE  # Keep search_conducted + contraband_found consistent: if no search is conducted, contraband cannot be found as result of search. 
# If a stop has multiple outcomes, report most severe outcome, consistent with other states. 
d$stop_outcome          <- multimap(d$id, d$InterventionDispositionCode, outcome_keys, outcome_vals, sep = ',')
d$stop_outcome          <- ifelse(grepl('Arrest', d$stop_outcome) | d$CustodialArrestIndicator == 'True', 'Arrest', 
                           ifelse(grepl('Summons', d$stop_outcome), 'Summons', 
                           ifelse(grepl('Ticket', d$stop_outcome), 'Ticket', 
                           ifelse(grepl('Written Warning', d$stop_outcome), 'Written Warning', 
                           ifelse(grepl('Verbal Warning', d$stop_outcome), 'Verbal Warning', NA)))))
d$is_arrested           <- d$stop_outcome == 'Arrest'

# Temporary function to fix the lat/long format
fix_latlong <- function(ll) {
  paste(str_extract(ll, '-?\\d\\d'),
        str_replace_all(str_extract(ll, '(?<=\\s).*'), '\\.|\\s', ''), sep='.')
}

# Extra fields
d$lat                   <- as.numeric(fix_latlong(d$InterventionLocationLatitude))
d$lon                   <- as.numeric(fix_latlong(d$InterventionLocationLongitude))
d$lon                   <- ifelse(d$lon > 0, -1*d$lon, d$lon)
d$officer_id            <- d$ReportingOfficerIdentificationID
d$stop_duration         <- map(d$InterventionDurationCode, 1:3, c("1-15 min", "16-30 min", "30+ min"))

# Close-up
write_cleaned_state(d, extra_cols=c('officer_id', 'stop_duration'))
change_path(NA)
