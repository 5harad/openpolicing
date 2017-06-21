# Processing code for Illinois
# The data for Illinois is quite messy. The format differs by year and Chicago is not always included.

# Set-up
this_state <- 'IL'
change_path(this_state)

# Read in and combine data
print(sprintf("[%s] reading in the data", this_state))
search_types       <- c(NA,"Consent","Reasonable Suspicion","Probable Cause","Incident to Arrest","Custodial Arrest","Drug Dog Alert", "Other","Consent Denied")
search_type_keys   <- c(NA, "Incident to Arrest", "Probable Cause", "Consent",  "Other", "Custodial Arrest", "Reasonable Suspicion", "Consent Denied" , "Unknown","Drug Dog Alert", 'Consent Search Request Denied')
search_type_vals <- c(NA, "Incident to Arrest", "Probable Cause", "Consent",  "Other", "Incident to Arrest", "Reasonable Suspicion", NA , NA, "K9 Search", NA)

# Make final column list
first_cols <- colnames(read_csv("2014_ITSS_Data.csv", n_max=100))[1:15]
first_cols <- c(first_cols[1:14], "TypeOfRoadway", first_cols[15])
# Iterate over files
d <- NULL
for (fn in list.files(path='.')) {
  # make file name
  year <- as.integer(substr(fn, 1, 4))
  print(fn)
  # read data
  new_d <- read_csv(fn, na=c("", "NA", "N/A"), col_types = cols(DateOfStop = col_character(), 
                                                                TimeOfStop = col_character(),
                                                                DateAndTimeOfStop = col_character(),
                                                                DriversYearofBirth = col_character()))
                  
  #print(colnames(new_d)[1:16])
  new_d$year <- year
  # 2007 chicago data is messed up
  chicago07 <- fn == '2007_Raw_Data_Statewide_Chicago.csv' 
  if(chicago07) {
    colnames(new_d) <- gsub(' ', '', colnames(new_d))
    colnames(new_d)[3] <- 'DateAndTimeOfStop'
    colnames(new_d)[16] <- 'Passenger1SearchType'
  }
  if(year == 2006){#
    new_d$ContrabandFound[is.na(new_d$ContrabandFound)] = FALSE  # lots of NANs here. 
  }

  # separate date column
  if(year < 2011) new_d <- new_d %>% separate(DateAndTimeOfStop, c("DateOfStop","TimeOfStop"), sep=' ', extra="merge")
  if(year == 2013) 
  {
    new_d$DateOfStop <- ifelse(grepl('-', new_d$DateOfStop), 
                               format(as.Date(substr(new_d$DateOfStop, 1, 10)), '%m/%d/%Y'), 
                               new_d$DateOfStop)
    new_d$DriversYearofBirth = str_sub(new_d$DriversYearofBirth, -4, -1)
  }
  print(sprintf("Number of nans in stop date for filename %s: %i\n", fn, sum(is.na(new_d$DateOfStop))))                              
  
  # add missing columns
  if(year %in% 2004:2006 | chicago07) { new_d <- new_d %>% add_new_col("DurationOfStop", 5) }
  if(year %in% 2004:2009) { new_d <- new_d %>% add_new_col("ZIPCode", 6) }
  if(year %in% 2007:2015) { new_d <- new_d %>% add_new_col("TypeOfRoadway", 15) }
  # merge column names

  colnames(new_d)[1:16] <- first_cols
  # make outcome columns, wildly different by year
  if(year %in% 2004:2006 | chicago07) {
    new_d <- new_d %>% mutate(
      contraband_found   = ContrabandFound == 'Yes',
      search_conducted   = SearchConducted == 'Yes',
      search_type_raw    = ifelse(is.na(coalesce(VehicleSearchType, DriverSearchType, Passenger1SearchType)), NA,
                                  paste(VehicleSearchType, DriverSearchType, Passenger1SearchType, sep=',')),
      search_type        = coalesce(VehicleSearchType, DriverSearchType, Passenger1SearchType),
      drugs_related_stop = DrugsFound == 'Yes'
    )
  } 
  if((year %in% 2007:2009) & (!chicago07)) {
    new_d <- new_d %>% mutate(
      contraband_found   = ContrabandFound == 'Yes' | WasConsentContrabandFound == 'Yes' ,
      search_conducted   = SearchConducted == 'Yes' | WasConsentSearchPerformed == 'Yes',
      drugs_related_stop = (DrugsFound == 'Yes') | (ParaphernaliaFound == 'Yes') | (ConsentDrugsFound == 'Yes') |
        (ConsentParaphernaliaFound == 'Yes'),
      search_type_raw    = ifelse(is.na(coalesce(VehicleSearchType, DriverSearchType, PassengersSearchType)), NA,
                                  paste(VehicleSearchType, DriverSearchType, PassengersSearchType, sep=',')),
      search_type        = coalesce(VehicleSearchType, DriverSearchType, PassengersSearchType)
    )
    new_d$search_conducted[is.na(new_d$search_conducted)] = FALSE # vast majority of values are NA in 2008, 2009
    new_d$contraband_found[is.na(new_d$contraband_found)] = FALSE # vast majority of values are NA in 2008, 2009
    
  }
  if(year %in% 2010:2011) {
    new_d <- new_d %>% mutate(
      contraband_found     = ContrabandFound == 1 | WasConsentContrabandFound == 1,
      search_conducted     = SearchConducted == 1 | WasConsentSearchPerformed == 1,
      drugs_related_stop   = DrugsFound == 1| ParaphernaliaFound == 1 | ConsentDrugsFound == 1 |
        ConsentParaphernaliaFound == 1,
      VehicleSearchType    = map(VehicleSearchType, 0:8, search_types),
      DriverSearchType     = map(DriverSearchType,  0:8, search_types),
      PassengersSearchType = map(PassengersSearchType, 0:8, search_types),
      search_type_raw      = ifelse(is.na(coalesce(VehicleSearchType, DriverSearchType, PassengersSearchType)), NA,
                                    paste(VehicleSearchType, DriverSearchType, PassengersSearchType, sep=',')),
      search_type          = ifelse(WasConsentSearchPerformed == 1, 'Consent',
                                    coalesce(VehicleSearchType, DriverSearchType, PassengersSearchType))
    )
  }
  if(year %in% 2012:2015) { 
    new_d <- new_d %>% mutate(
      contraband_found = VehicleContrabandFound == 1 | VehicleDrugsFound == 1 | VehicleDrugParaphernaliaFound == 1 |
        VehicleAlcoholFound == 1 | VehicleWeaponFound == 1 | VehicleStolenPropertyFound == 1 |
        VehicleOtherContrabandFound == 1 | DriverPassengerContrabandFound == 1 |
        DriverPassengerDrugsFound == 1 | DriverPassengerDrugParaphernaliaFound == 1 |
        DriverPassengerAlcoholFound == 1 | DriverPassengerWeaponFound == 1 |
        DriverPassengerStolenPropertyFound == 1 | DriverPassengerOtherContrabandFound == 1 | 
        PoliceDogContrabandFound == 1 | PoliceDogDrugsFound == 1 | PoliceDogDrugParaphernaliaFound == 1 | 
        PoliceDogAlcoholFound == 1 | PoliceDogWeaponFound == 1 | PoliceDogStolenPropertyFound == 1 | PoliceDogOtherContrabandFound == 1,
      search_conducted   = VehicleSearchConducted == 1 | DriverSearchConducted == 1 |
        PassengerSearchConducted == 1 | PoliceDogVehicleSearched == 1,
      # Sometimes contraband is found but no search is recorded. We set these to FALSE (because we define it as contraband found as the result of a search).
      # The vast majority of vals for contraband are NA, which we set to FALSE. 
      contraband_found   = ifelse(is.na(contraband_found) | (!search_conducted), FALSE, contraband_found),
      search_type_raw    = NA, # it's all booleans
      search_type        = ifelse(VehicleConsentGiven == 1 | DriverConsentGiven == 1 | PassengerConsentGiven == 1, 'Consent',
                                  ifelse(PoliceDogAlertIfSniffed == 1, 'Drug Dog Alert', 'Unknown')),
      drugs_related_stop = VehicleDrugsFound == 1 | VehicleDrugParaphernaliaFound == 1 | VehicleDrugAmount > 0 |
        DriverPassengerDrugsFound == 1 | DriverPassengerDrugParaphernaliaFound == 1 | DriverPassengerDrugAmount > 0 |
        PoliceDogDrugsFound == 1 | PoliceDogDrugParaphernaliaFound == 1 | PoliceDogDrugAmount > 0
    )
  }
  # add new row
  d <- rbind(d, new_d %>% select(1:16, contraband_found, search_conducted, search_type_raw, search_type, drugs_related_stop, year))
}



# Value dictionaries
# different years have different ways of recording race. The dictionaries below contain all values. 
# race_keys is the values in the raw data. 
# race_vals maps the numeric race codes to their dictionary codes (provided by the state). 
# race_vals_clean is the standardized mapping. 
race_keys       <- c("1", "2", "3", "4", "5", "6", "7", "9", "999", "African American", "Caucasian", "Asian/Pacific Islander", "Native American/Alaskan")
race_vals       <- c("Caucasian", "Black or African American", "Native American/Alaskan", "Hispanic", "Asian/Pacific Islander", "Native Hawaiian or Other Pacific Islander", "7", "9", "999", "African American", "Caucasian", "Asian/Pacific Islander", "Native American/Alaskan")
race_vals_clean <- c("White", "Black", "Other", "Hispanic", "Asian", "Asian", NA, NA, NA, "Black", "White", "Asian", "Other")


# fix spelling in district names
district_keys <- c("0", "1", "2", "O2", "3",  "4", "5", "6", "7", "8", "9",  "92", "93", "97", "D02", "D-02", "D-15", "D2", "I290 NB", "SEO","ZONE 4","25","29","30","33","50", '70','ZONE 1', 'D15') 
district_vals <- c("00", "01", "02", "02", "03", "04", "05", "06", "07", "08", "09",   "09",   "09",   "09",   "02", "02","15", "02", NA, NA, "04",NA,NA,NA,NA,NA, NA,NA, '15')
d$BeatLocationOfStop = map(d$BeatLocationOfStop, district_keys, district_vals)

# Rename and extract columns
print(sprintf("[%s] extracting columns", this_state))

d$state                 <- this_state
# Time and date format is different across years
d$stop_date             <- make_date(ifelse(is.na(d$DateOfStop), paste0('01/01/', year), d$DateOfStop), '%m/%d/%Y')
d$stop_time             <- ifelse(grepl('M', d$TimeOfStop),
                                  strftime(strptime(d$TimeOfStop, "%I:%M:%S %p"), format='%H:%M'),
                                  strftime(strptime(d$TimeOfStop, "%H:%M"), format='%H:%M'))
d$id                    <- make_row_id(d)
# State patrol districts map : http://www.isp.state.il.us/districts/districtfinder.cfm
# For state patrol stops (ie, those made by the Illinois State Police), we define county_name_raw as the district
# For local stops, we define it as the agency name. 
d$location_raw          <- ifelse(d$AgencyName == 'ILLINOIS STATE POLICE', paste(d$AgencyName, d$BeatLocationOfStop), d$AgencyName) # we mapped police departments to counties
d$location_raw[d$location_raw == 'ILLINOIS STATE POLICE NA'] = NA
counties_clean          <- normalize_county(d) 
d$county_name           <- counties_clean$county_name
d$county_fips           <- counties_clean$fips
d$fine_grained_location <- d$BeatLocationOfStop
d$state_patrol          <- ifelse(d$AgencyName == 'ILLINOIS STATE POLICE', TRUE, FALSE)
d$district              <- ifelse(d$state_patrol, d$location_raw, NA)  # set district to be the beat if state patrol, otherwise don't fill in. 
d$police_department     <- str_to_title(trim(d$AgencyName))
d$driver_gender         <- map(d$DriverSex, c(1, 2, "Male", "Female", 9), c('M','F','M','F', NA))
d$driver_age_raw        <- as.numeric(d$DriversYearofBirth)  # sometimes it's 2 digits, is it age or 'xx? This is v rare. 
d$driver_age            <- get_age(d, type='birthyear')
d$driver_race_raw       <- map(d$DriverRace, race_keys, race_vals)
d$driver_race           <- map(d$DriverRace, race_keys, race_vals_clean)
d$reasons               <- paste(d$ReasonForStop, d$TypeOfMovingViolation, sep=',')
d$violation_raw         <- normalize_violation(d, d$reasons)
d$violation             <- normalize_violation(d, d$reasons, clean=TRUE)
# search_conducted, contraband_found, search_type, are already included
d$contraband_found[d$search_conducted == FALSE] = FALSE
d$search_type_raw       <- ifelse(d$search_conducted, d$search_type, NA)
d$search_type           <- map(d$search_type_raw, search_type_keys, search_type_vals)
d$stop_outcome          <- map(d$ResultOfStop, 1:3, c("Citation", "Written Warning", "Verbal Warning"))
d$is_arrested           <- NA # We have data on searches conducted incident to arrest, but that is not quite the same thing. 

# Extra fields
d$stop_duration         <- d$DurationOfStop
d$vehicle_type          <- paste(str_to_title(d$VehicleMake), d$VehicleYear)
# drugs_related_stop has already been created above

# Close-up
write_cleaned_state(d, extra_cols=c('stop_duration','vehicle_type','drugs_related_stop', 'district'))
change_path(NA)
