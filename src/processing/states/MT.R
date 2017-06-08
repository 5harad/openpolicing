# Processing code for Montana

# Set-up
this_state <- 'MT'
change_path(this_state)

# Read in and combine data
print(sprintf("[%s] reading in the data", this_state))

d <- NULL
for (fn in list.files(path='.')) {
  dread <- read_csv(fn, na = c("", "NA", "NULL"))
  d <- rbind(d, dread)
}

# Value dictionaries
race_keys <- c("A","B","I","U","W")
race_vals <- c("Asian / Pacific Islander","Black","Native American","Unknown","White")
race_vals_clean <- c("Asian","Black","Other",NA,"White")
search_type_keys <- c("CONSENT SEARCH CONDUCTED","INVENTORY","PLAIN VIEW","PROBABLE CAUSE",
                      "PROBATION/PAROLE OFFICER CONSENT","SEARCH INCIDENT TO ARREST",
                      "SEARCH WARRANT", "STOP AND FRISK (OFFICER SAFETY) (S.901.151, F.S.)")
search_type_vals <- c("Consent","Inventory","Probable Cause","Probable Cause",
                      "Parole/Probation","Incident to Arrest","Warrant",
                      "Protective Frisk")

# Rename and extract columns
print(sprintf("[%s] extracting columns", this_state))
d$state                 <- this_state
d$stop_date             <- make_date(substr(d$StopTime, 1, 10))
d$stop_time             <- strftime(strptime(substr(d$StopTime, 12, 16), "%H:%M"), format='%H:%M')
d$id                    <- make_row_id(d)
d$location_raw            <- d$County
counties_clean          <- normalize_county(d)
d$county_name           <- counties_clean$county_name
d$county_fips           <- counties_clean$fips
d$fine_grained_location <- d$Location
d$state_patrol          <- TRUE
d$police_department     <- NA  # not included
d$driver_gender         <- ifelse(d$Sex %in% c("M","F"), d$Sex, NA)
d$driver_age_raw        <- d$Age
d$driver_age            <- get_age(d)
d$driver_race_raw       <- ifelse(d$Ethnicity=='H', 'Hispanic', map(d$Race, race_keys, race_vals))
d$driver_race           <- ifelse(d$Ethnicity=='H', 'Hispanic', map(d$Race, race_keys, race_vals_clean))
d$violation_raw         <- d$Violation1
d$violation_raw         <- ifelse(!is.na(d$Violation2), paste(d$violation_raw, d$Violation2, sep=','), d$violation_raw)
d$violation_raw         <- ifelse(!is.na(d$Violation3), paste(d$violation_raw, d$Violation3, sep=','), d$violation_raw)
d$violation             <- normalize_violation_multiple(d, d$violation_raw, clean=TRUE, sep=',')
d$search_conducted      <- !is.na(d$SearchType) & (!(d$SearchType %in% c('NULL','NO SEARCH REQUESTED','NO SEARCH / CONSENT DENIED')))
d$search_type_raw       <- ifelse(d$search_conducted, d$SearchType, NA)
d$search_type           <- map(d$search_type_raw, search_type_keys, search_type_vals)
d$contraband_found      <- NA  # not included
d$stop_outcome_raw          <- apply(cbind(d$EnforcementAction1, d$EnforcementAction2, d$EnforcementAction3), 1, function(x) paste(sort(unique(x)), collapse=","))
d$stop_outcome          <- ifelse(grepl('ARREST', d$stop_outcome_raw), 'Arrest', 
                         ifelse(grepl('CITATION', d$stop_outcome_raw), 'Citation', 
                         ifelse(grepl('WARNING', d$stop_outcome_raw), 'Warning', 
                         ifelse(grepl('FAULTY EQUIPMENT', d$stop_outcome_raw), 'Faulty Equipment Notice',
                         ifelse(grepl('NO ENFORCEMENT|NONE', d$stop_outcome_raw), 'No Action', NA)))))
                                
                          
d$is_arrested           <- grepl('ARREST', d$stop_outcome_raw)

# Extra fields
d$lat                   <- d$Latitude
d$lon                   <- d$Longitude
d$ethnicity             <- d$Ethnicity
d$city                  <- d$City
d$out_of_state          <- d$VehicleTagNoState != 'MT'
d$vehicle_year          <- d$VehicleYear
d$vehicle_make          <- d$VehicleMake
d$vehicle_model         <- d$VehicleModel
d$vehicle_style         <- d$VehicleStyle
d$search_reason         <- ifelse(d$search_conducted, 
                                  apply(cbind(d$SearchRationale1, d$SearchRationale2, d$SearchRationale3, d$SearchRationale4), 1, 
                                        function(x) paste(sort(unique(x[x != 'NOT APPLICABLE'])), collapse=",")), NA)
d$search_reason[nchar(d$search_reason) == 0] = NA
# Close-up
write_cleaned_state(d, extra_cols=c('lat','lon','ethnicity','city','out_of_state','vehicle_year',
                                    'vehicle_make','vehicle_model','vehicle_style', 'search_reason', 'stop_outcome_raw'))

change_path(NA)
