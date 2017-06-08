# Processing code for South Carolina

# Set-up
this_state <- 'SC'
change_path(this_state)

# Read in and combine data
print(sprintf("[%s] reading in the data", this_state))
d <- read_csv("FOIA1605298_2.csv", na=c('','NA','NULL'))

# Rename and extract columns
print(sprintf("[%s] extracting columns", this_state))

d$state                 <- this_state
d$stop_date             <- make_date(d$DateIssued, "%m/%d/%Y")
d$stop_time             <- NA  # not included
d$id                    <- make_row_id(d)
d$location_raw            <- d$County
counties_clean          <- normalize_county(d)
d$county_name           <- counties_clean$county_name
d$county_fips           <- counties_clean$fips
d$fine_grained_location <- NA  # not included
d$state_patrol          <- TRUE
d$police_department     <- d$PoliceDivision   # these are agencies, 98% are highway patrol
d$driver_gender         <- ifelse(d$Sex %in% c("M","F"), d$Sex, NA)
d$driver_age_raw        <- d$`Defendent Age`
d$driver_age            <- get_age(d)
d$driver_race_raw       <- d$Race
d$driver_race           <- ifelse(d$Race %in% c("Unknown","0"), NA, d$Race)
# Add whether speeding was radar-triggered or not
d$violation_raw         <- ifelse(grepl("Speeding", d$OffenseCode) & d$ContactReason == 'Radar Triggered',
	                          paste(d$OffenseCode, "(RADAR)"), d$OffenseCode)
d$violation             <- normalize_violation(d, d$OffenseCode, clean=TRUE)
d$search_conducted      <- ifelse(d$Searched == '1' | d$SubjectSearched == '1' | d$PassengerSearched == '1' | d$VehicleSearched == '1', TRUE, FALSE)
d$search_conducted      <- ifelse(is.na(d$search_conducted), FALSE, d$search_conducted)
d$search_type_raw       <- NA  # not included
d$search_type           <- NA  # not included
d$contraband_found      <- ifelse(d$Contraband == '1' | d$ContrabandDrugs == '1' | d$ContrabandDrugParaphenalia == '1' | d$ContrabandWeapons == '1' | !is.na(d$ContrabandDesc), TRUE, FALSE)
d$contraband_found      <- ifelse(is.na(d$contraband_found), FALSE, d$contraband_found)
d$contraband_found[d$search_conducted == FALSE] <- FALSE # contraband can only be found as result of a search. 
d$stop_outcome          <- ifelse(d$FelonyArrest == '1', 'Felony Arrest',
	                       ifelse(d$Jailed == '1', 'Arrest', 
	                       ifelse(d[,1] == 'Citation', 'Citation', 'Warning')))
d$is_arrested           <- ifelse(d$Jailed == '1' | d$FelonyArrest == '1', TRUE, FALSE)

# Remove pedestrian and collision stops
d <- d %>% filter(!ContactReason %in% c('Collision', 'Pedestrian'))

# Extra fields
d$officer_id            <- d$`Officer Badge Number`
d$officer_race          <- d$`Officer Race`
d$officer_age           <- d$`Officer Age`
d$lat                   <- ifelse(as.numeric(d$LatitudeDec)  != 0, as.numeric(d$LatitudeDec) , NA)
d$lon                   <- ifelse(as.numeric(d$LongitudeDec) != 0, as.numeric(d$LongitudeDec), NA)
d$highway_type          <- d$HighwayType
d$road_number           <- d$HighwayNum
d$stop_purpose          <- d$ContactReason

# Close-up
write_cleaned_state(d, extra_cols=c('lat','lon','highway_type','road_number','stop_purpose',
									'officer_id','officer_race','officer_age'))
change_path(NA)
