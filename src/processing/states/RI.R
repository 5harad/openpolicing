# Processing code for Rhode Island

# Set-up
this_state <- 'RI'
change_path(this_state)

# Read in and combine data
print(sprintf("[%s] reading in the data", this_state))

d = NULL
for (year in 2005:2015) {
	d = rbind(d, read_csv(sprintf("RhodeIsland_%d_converted.csv", year)))
}

# Value dictionaries
# L = 'Black Hispanic' -> Hispanic, consistent with coding policy in other states. 
race_keys <- c('B','H','I','L','N','W','O')
race_vals <- c('Black', 'Hispanic','Asian','Hispanic','Other', 'White','Other')
outcome_keys <- c('A','D','M','N','P','W')
outcome_vals <- c('No Action', 'Arrest Driver', 'Citation', 'N/D', 'Arrest Passenger', 'Warning')
searched_reason_keys <- c('A','C','I','O','P','R','T', 'N')
searched_reason_vals <- c('Incident to Arrest','Plain View','Inventory/Tow','Odor of Drugs/Alcohol', 'Probable Cause', 'Reasonable Suspicion','Terry Frisk', NA)
cleaned_searched_reason_vals <- c('Incident to Arrest','Probable Cause','Inventory','Probable Cause', 'Probable Cause', 'Reasonable Suspicion','Protective Frisk', NA)

# Rename and extract columns
print(sprintf("[%s] extracting columns", this_state))
d$state                 <- this_state
d$stop_date             <- make_date(paste(substr(d$StopDate, 1, 4), substr(d$StopDate, 5, 6), substr(d$StopDate, 7, 8), sep='-'))
d$stop_time             <- strftime(strptime(d$StopTime, "%H%M"), format = '%H:%M')
d$id                    <- make_row_id(d)
d$location_raw          <- paste('Zone', d$Zone)  # trooper zones, unmapped, can't find mapping, best guess: http://www.scannewengland.net/wiki/index.php?title=Rhode_Island_State_Police
counties_clean          <- normalize_county(d)
d$county_name           <- counties_clean$county_name
d$county_fips           <- counties_clean$fips
d$district              <- d$location_raw  # set district to be trooper zone. 
d$fine_grained_location <- NA
d$state_patrol          <- TRUE
d$police_department     <- as.character(d$AgencyORI)
d$driver_gender         <- map(d$OperatorSex, c('N','U'), c(NA,NA))
d$driver_age_raw        <- as.numeric(d$YearOfBirth)
d$driver_age            <- get_age(d, type='birthyear')
d$driver_race_raw       <- d$OperatorRace
d$driver_race           <- map(d$driver_race_raw, race_keys, race_vals)  # map race with dictionary approach
d$violation_raw         <- normalize_violation(d, d$BasisForStop)
d$violation             <- normalize_violation(d, d$BasisForStop, clean=TRUE)
d$search_conducted      <- (d$Searched == 'Y') | (d$Frisked == 'Y')

# combine search reasons, removing redundant ones. Short helper function to do this. 
# d is the dataframe, from_mapping is the original values, to_mapping is the values to map them to. 
map_search_reasons = function(d, from_mapping, to_mapping){
  d$x1 = map(d$SearchReasonOne, from_mapping, to_mapping)
  d$x2 = map(d$SearchReasonTwo, from_mapping, to_mapping)
  d$x3 = map(d$SearchReasonThree, from_mapping, to_mapping)
  d$mapped_search_type = apply(d[,c('x1', 'x2', 'x3')], 
                            1, 
                            function(x) paste0(sort(unique(x)), collapse = ','))
  d$mapped_search_type[d$mapped_search_type == ''] = NA 
  return(d$mapped_search_type)
}

d$search_type_raw = map_search_reasons(d, searched_reason_keys, searched_reason_vals)
d$search_type = map_search_reasons(d, searched_reason_keys, cleaned_searched_reason_vals)

combine_multiple_occupants = function(x){
  return(paste(sort(unique(na.omit(x[!(x == 'NOT INDICATED' | x == 'NOT APPLICABLE')]))), collapse='|'))}
# There can be multiple search bases for a single stop (also individual vs vehicle)
d$contraband_found      <- (d$SearchResultOne   %in% c('A','D','M','O','W')) | 
                           (d$SearchResultTwo   %in% c('A','D','M','O','W')) | 
                           (d$SearchResultThree %in% c('A','D','M','O','W'))
d$stop_outcome          <- map(d$ResultOfStop, outcome_keys, outcome_vals)
d$is_arrested           <- (d$ResultOfStop == 'P') | (d$ResultOfStop == 'D') 

# Extra fields
d$stop_duration        <- map(d$Duration, c('A','B','C'), c('0-15 Min','16-30 Min','30+ Min'))
d$out_of_state         <- d$RegState != 'RI'
d$drugs_related_stop   <- grepl('D', paste(d$SearchResultOne, d$SearchResultTwo, d$SearchResultThree))

# Close-up
write_cleaned_state(d, extra_cols=c('stop_duration','out_of_state','drugs_related_stop', 'district'))
change_path(NA)
