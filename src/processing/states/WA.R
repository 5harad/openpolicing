# Processing code for Washington

# Set-up
this_state <- 'WA'
change_path(this_state)

# Read in and combine data
print(sprintf("[%s] reading in the data", this_state))

# Column names
cols <- c('EmployeeLast', 'EmployeeFirst', 'officer_race_raw', 'officer_gender', 'contact_date', 'contact_hour', 'highway_type', 
          'road_number', 'milepost', 'contact_type_orig', 'driver_race_orig', 'driver_age_raw', 'driver_gender_raw', 'search_type_orig', 
          'violation1', 'enforcement1', 'violation2', 'enforcement2', 'violation3', 'enforcement3',
          'violation4', 'enforcement4', 'violation5', 'enforcement5', 'violation5_dup', 'enforcement5_dup', 'violation6', 
          'enforcement6', 'violation7', 'enforcement7', 'violation8','enforcement8', 'violation9', 'enforcement9', 'violation10',
          'enforcement10', 'violation11', 'enforcement11',  'violation12', 'enforcement12')

# Read in each file
d <- NULL
for (fn in list.files('.', pattern='.csv')) {
  print(fn)
  new_d <- read_csv(fn, col_names=cols, na=c("NA", "", "NULL", "-")) %>%
    # deal with later year differences
    separate(contact_date, c("contact_date"), sep=' ') %>%
    mutate(contact_date=if_else(grepl('/', contact_date), as.Date(contact_date, '%m/%d/%Y'), as.Date(contact_date)))
  d <- rbind(d, new_d)
}

# remove park (P) and weigh stations (W).
# P is a very small group of stops we cannot map
# W are not normal traffic stops (all related to truck weigh station violations)
d <- d %>% filter( highway_type !='P' & highway_type != 'W')

#Always run location mapping script; we have refactored so this only takes a few seconds. 
message("Saving dataframe so we can rerun location mapping script.")
d$state     <- this_state
d$stop_date <- make_date(d$contact_date)
d$stop_time             <- strftime(strptime(d$contact_hour, "%H"), format = '%H:%M')
d$id        <- make_row_id(d)
grouped_d = d %>% group_by(highway_type, road_number, milepost) %>% #we do not need a unique row for every stop, just one for each milepost_id. 
                  summarise(n_stops = n()) %>% ungroup()
save(grouped_d, file="../tmp/raw_data_with_ids.RData")
source(paste0(code_path, '/src/processing/scripts/WA_map_locations.R'))
# Dictionaries
search_type_keys <- c( "A1", "A2", "C1", "C2", "I1", "I2", "K1", "K2", "N", "P1", "P2", "W1", "W2", "-", "NULL")
search_type_vals <- c("Incident to Arrest", "Incident to Arrest", "Consent", "Consent", "Impound Search",
                      "Impound Search", "K9 Search", "K9 Search", "No Search", "Pat Down Search", "Pat Down Search", "Warrant", "Warrant", NA, NA)
search_type_vals_clean <- c("Incident to Arrest", "Incident to Arrest", "Consent", "Consent", "Impound", "Impound", 
                            "K9 Search", "K9 Search", NA, "Protective Frisk", "Protective Frisk", "Warrant","Warrant", NA, NA)

race_keys <- c(1:8)
race_vals <- c('White', 'African American', 'Native American', 'Asian', 'Pacific Islander', 'East Indian', 'Hispanic', 'Other') # the English language values of driver race, as provided by the state. 
race_vals_clean <- c('White', 'Black', 'Other', 'Asian', 'Asian', 'Asian', 'Hispanic', 'Other') # the standardized values of driver race. 

officer_race_keys <- c("AMER IND/AK NATIVE","ASIAN/PI","BLACK","HISPANIC","WHITE")
officer_race_vals <- c("Other","Asian","Black","Hispanic","White")

contact_type_keys <- c(1:13)
contact_type_vals <- c('Self-Initiated Contact', 'Calls for service', NA, 'Collisions', 'Collisions enf. follow-up', 
                       'Other enf. follow-up', 'Aggressive driving', 'Road rage', 'Emphasis patrol', 'CMV inspect/weighing', NA, 
                       'Self-Initiated Physical Assist', 'Distracted driving')

violations <- read_csv('violation_dictionary.txt') %>%
  mutate(code=as.numeric(violation), desc=violation_descrip)

# Some post-processing
d <- d %>%
  # Remove duplicate columns
  select(-c(violation5_dup, enforcement5_dup)) %>%
  mutate(
    officer_name = paste(str_trim(EmployeeLast), str_trim(EmployeeFirst)),
    road_number  = as.character(road_number),
    road_number  = ifelse(nchar(road_number)==1, str_pad(road_number, 2, pad = "0"), road_number),
    road_number  = ifelse(nchar(road_number)==2, str_pad(road_number, 3, pad = "0"), road_number),
    road_number  = replace(road_number, road_number == "97A", "097AR"),
    road_number  = replace(road_number, road_number == "28B", "028"),
    road_number  = replace(road_number, road_number == "20S", "020SPANACRT"), 
    milepost_id  = paste(highway_type, road_number, milepost, sep='-')
  )

# Join in locations from other script (WA_map_location.R)
d_locations <- read_csv("../tmp/wa_location.csv") %>% select(milepost_id, longitude, latitude, county=county_name)
d <- d %>% left_join(d_locations, by='milepost_id')


# Concat violations and enforcements
d <- d %>% mutate(
  # Concat violations
  enforcements = paste(enforcement1,enforcement2 ,enforcement3 ,enforcement4,
                       enforcement5,enforcement6 ,enforcement7 ,enforcement8,
                       enforcement9,enforcement10,enforcement11,enforcement12, sep=','),
  enforcements = str_trim(gsub("NA,|,NA", "", enforcements)),
  # Concat enforcements
  violations = paste(as.numeric(violation1), 
                     as.numeric(violation2), 
                     as.numeric(violation3), 
                     as.numeric(violation4), 
                     as.numeric(violation5), 
                     as.numeric(violation6), 
                     as.numeric(violation7), 
                     as.numeric(violation8), 
                     as.numeric(violation9), 
                     as.numeric(violation10), 
                     as.numeric(violation11), 
                     as.numeric(violation12), sep=','),
  violations = gsub("NA,|,NA", "", violations)
)
d$violations[d$violations == 'NA'] = NA#remap Nans so they don't get mapped to "other (unmapped)". 
d$enforcements[d$enforcements == 'NA'] = NA#remap Nans so they don't get mapped to "other (unmapped)". 

# Rename and extract columns
print(sprintf("[%s] extracting columns", this_state))

d$state                 <- this_state
d$stop_date             <- make_date(d$contact_date)
d$stop_time             <- strftime(strptime(d$contact_hour, "%H"), format = '%H:%M')
d$id                    <- make_row_id(d)
d$location_raw          <- d$county
counties_clean          <- normalize_county(d)
d$county_name           <- counties_clean$county_name
d$county_fips           <- counties_clean$fips
d$fine_grained_location <- d$milepost_id
d$state_patrol          <- TRUE
d$police_department     <- NA  # not included
d$driver_gender         <- map(d$driver_gender_raw, '-', NA)
d$driver_age_raw        <- as.numeric(d$driver_age_raw)
d$driver_age            <- get_age(d)
d$driver_race_raw       <- map(as.character(d$driver_race_orig), race_keys, race_vals)
d$driver_race           <- map(as.character(d$driver_race_orig), race_keys, race_vals_clean)
d$violation_raw         <- normalize_violation_multiple(d, d$violations)
d$violation             <- normalize_violation_multiple(d, d$violations, clean=TRUE)
d$search_type_raw       <- map(d$search_type_orig, search_type_keys, search_type_vals)
d$search_type           <- map(d$search_type_raw, search_type_vals, search_type_vals_clean) 
d$search_conducted      <- !(d$search_type %in% c(NA, 'No Search'))
d$contraband_found      <- ifelse(grepl('1', d$search_type_orig), TRUE, FALSE)
d$stop_outcome          <- ifelse(grepl('1', d$enforcements), 'Arrest or Citation',
                                  ifelse(grepl('2', d$enforcements), 'Written Warning',
                                         ifelse(grepl('3', d$enforcements), 'Verbal Warning', NA)))
d$is_arrested           <- NA  # not included

# Extra columns
d$officer_id            <- d %>% transform(id=match(officer_name, unique(officer_name))) %>% .$id
d$officer_gender        <- ifelse(d$officer_gender == 1, 'M', 'F')
d$officer_race          <- map(d$officer_race_raw, officer_race_keys, officer_race_vals)
d$highway_type          <- d$highway_type
d$road_number           <- d$road_number
d$milepost              <- d$milepost
d$lat                   <- d$latitude
d$lon                   <- d$longitude
d$contact_type          <- map(as.numeric(d$contact_type_orig), contact_type_keys, contact_type_vals)
d$enforcements          <- d$enforcements  # full list of outcomes
d$drugs_related_stop    <- grepl('Drugs', d$violation_raw)

na_check = d %>% group_by(highway_type) %>% summarise(n = n(), 
                                                      lat_is_na = mean(is.na(lat)), 
                                                      fips_is_na = mean(is.na(county_fips)), 
                                                      total_na = sum(is.na(county_fips) | is.na(lat)))
print(na_check)
  
# Close-up
write_cleaned_state(d, c('violations', 'officer_id','officer_gender','officer_race',
                         'highway_type','road_number','milepost','lat','lon',
                         'contact_type','enforcements','drugs_related_stop'))
change_path(NA)
