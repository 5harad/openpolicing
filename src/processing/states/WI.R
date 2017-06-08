# Processing code for Wisconsin

# Set-up
this_state <- 'WI'
change_path(this_state)

# Read in and combine data
print(sprintf("[%s] reading in the data", this_state))
d10W <- read_csv("TraCS10_TrafficStops_OutcomeWarnings.csv")
d10C <- read_csv("TraCS10_TrafficStops_OutcomeCitations.csv")
d73W <- read_csv("TraCS7.3_TrafficStops_OutcomeWarnings.csv")
d73C <- read_csv("TraCS7.3_TrafficStops_OutcomeCitations.csv")

# Align column names, some are spelled slightly differently
colnames(d73W)[41 ] <- 'IndividualGrp_lnk'
colnames(d73C)[41 ] <- 'IndividualGrp_lnk'
colnames(d73W)[43 ] <- 'summaryDateOccurred'
colnames(d73C)[43 ] <- 'summaryDateOccurred'
colnames(d73W)[80 ] <- 'vehicleCompanyName'
colnames(d73C)[80 ] <- 'vehicleCompanyName'
colnames(d10W)[40 ] <- 'individualMultiKey'
colnames(d10C)[40 ] <- 'individualMultiKey'
colnames(d10W)[89 ] <- 'vehicleCompanyName'
colnames(d10C)[89 ] <- 'vehicleCompanyName'
colnames(d73W)[135] <- 'Statute'
colnames(d73C)[135] <- 'Statute'
colnames(d73W)[136] <- 'StatuteDescription'
colnames(d73C)[136] <- 'StatuteDescription'
colnames(d10W)[133] <- 'Statute'
colnames(d10C)[133] <- 'Statute'
colnames(d10W)[134] <- 'StatuteDescription'
colnames(d10C)[134] <- 'StatuteDescription'

# take the overlapping columns in the right order
d73W <- d73W[, colnames(d10W)]
d73C <- d73C[, colnames(d10W)]

# extract stop time from d73W, so that strftime / strptime functions can be applied to both files
d73W$summaryTimeOccurred <- substr(d73W$summaryTimeOccurred, 12, 16)
d73C$summaryTimeOccurred <- substr(d73C$summaryTimeOccurred, 12, 16)

# Bind it together
d <- rbind(d10W, d73W, d10C, d73C)

# Dictionary mappings
race_keys <- c("A", "B", "H", "I", "W", "O")
race_vals <- c("Asian", "Black", "Hispanic", "Other", "White", NA)
search_type_keys <- c('01', '02', '2', '03', '3', '04', '4', '05', '5', '06', '6', '99')
search_type_vals <- c('Warrant', 'Incident to Arrest', 'Incident to Arrest', 'Probable Cause', 'Probable Cause', 'Exigent Circumstances',
                      'Exigent Circumstances', 'Consent', 'Consent', 'Inventory', 'Inventory', NA)
d$search_type_codes = paste(d$individualSearchBasis, d$vehicleSearchBasis, sep=',')

# Provided dictionary for vehicle make
map_vehicle_make <- read_csv("dictionaries/Jennifer Mobley - TraCS 10-Codes-VehicleMake.csv")

# Search cleaning, make sure no NA problems
d$individualContraband <- ifelse(d$individualContraband=='00', NA, d$individualContraband)
d$vehicleContraband    <- ifelse(d$vehicleContraband=='00', NA, d$vehicleContraband)

# clean warningStatuteDescription
d$StatuteDescription <- gsub(',', '', d$StatuteDescription)
d$search_basis <- paste(d$individualSearchBasis, d$vehicleSearchBasis, sep=',')

# Cleaning counties - some are 'code', some are 'code - name'
counties_from <- unique(d$countyDMV)
mapping       <- data.frame(do.call('rbind', strsplit(counties_from,' - ',fixed=TRUE)))
mapping$id    <- as.numeric(as.character(mapping$X2))
joins         <- mapping[as.character(mapping$X1) != as.character(mapping$X2),]
joins$id      <- as.numeric(as.character(joins$X2))
mapping       <- left_join(mapping, joins, by='id')
counties_to   <- as.character(mapping[,4])

# Filter out duplicate rows. A stop can have multiple violations and each violation can appear as separate rows.
d <- d %>%
  group_by(summaryDateOccurred, summaryTimeOccurred, onHighwayDirection, onHighwayName, fromAtStreetName, agencyDOTOfficerIdentificationNumber, 
           race, sex, longitude, latitude, countyDMV, agencyNameDepartment, individualSearchConducted, 
           vehicleSearchConducted, individualContraband, vehicleContraband, summaryOutcome, make, model, modelYear, search_basis) %>%
  summarise(all_violations = paste(str_trim(StatuteDescription), collapse=',')) %>%
  ungroup()

# Rename and extract columns
print(sprintf("[%s] extracting columns", this_state))
d$state                 <- this_state
d$stop_date             <- make_date(d$summaryDateOccurred, min_ds=as.Date("2010-01-01"))
d$stop_time             <- strftime(strptime(d$summaryTimeOccurred, "%H:%M"), format = '%H:%M')
d$id                    <- make_row_id(d)
d$location_raw            <- map(d$countyDMV, counties_from, counties_to) 
counties_clean          <- normalize_county(d)
d$county_name           <- counties_clean$county_name
d$county_fips           <- counties_clean$fips
d$fine_grained_location <- paste(d$onHighwayDirection, d$onHighwayName, d$fromAtStreetName)
d$state_patrol          <- TRUE
d$police_department     <- d$agencyNameDepartment  # state patrol, only 6 'groups'
d$driver_gender         <- map(d$sex, c("U", "1", 1), c(NA, NA, NA))
d$driver_age_raw        <- NA  # not included
d$driver_age            <- NA  # not included
d$driver_race_raw       <- d$race
d$driver_race           <- map(d$race, race_keys, race_vals)  # map race with dictionary approach
d$violation_raw         <- d$all_violations
d$violation             <- normalize_violation_multiple(d, d$violation_raw, clean=TRUE)
d$search_conducted      <- ifelse(d$individualSearchConducted=='Y' | d$vehicleSearchConducted=='Y', TRUE, FALSE)
d$search_type_raw       <- multimap(d$id, d$search_basis, search_type_keys, search_type_vals)
d$search_type           <- d$search_type_raw  # same
d$contraband_found      <- ifelse((!is.na(d$individualContraband)) | (!is.na(d$vehicleContraband)), TRUE, FALSE)
d$stop_outcome          <- ifelse(grepl("05", d$summaryOutcome), 'Arrest',
                           ifelse(grepl("04", d$summaryOutcome), 'Citation',
                           ifelse(grepl("03", d$summaryOutcome), 'Written Warning',
                           ifelse(grepl("02", d$summaryOutcome), 'Verbal Warning',
                           ifelse(grepl("01", d$summaryOutcome), 'No Action', NA)))))
d$is_arrested           <- grepl("05", d$summaryOutcome)

# Extra fields
d$lat                   <- d$latitude
d$lon                   <- d$longitude
d$officer_id            <- d$agencyDOTOfficerIdentificationNumber
d$vehicle_type          <- paste(map(d$make, map_vehicle_make$CodeValue, map_vehicle_make$CodeText), d$model, d$modelYear)
d$drugs_related_stop    <- grepl("03", d$vehicleContraband) | grepl("03", d$individualContraband)

# Close-up
write_cleaned_state(d, extra_cols=c('lat','lon','officer_id','vehicle_type','drugs_related_stop'))
change_path(NA)
