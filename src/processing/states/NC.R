# Processing code for North Carolina

# Set-up
this_state <- 'NC'
change_path(this_state)
district_map <- read_csv(sprintf("%s/resources/dictionaries/districts.csv", code_path)) %>% 
  filter(state == 'NC')

# Read in each part.
person       <- read_csv('PERSON.csv')
search       <- read_csv('SEARCH.csv')
contraband   <- read_csv('CONTRABAND.csv')
stops        <- read_csv('STOP.csv')
county_codes <- read_csv('county_codes.csv') %>%
                # cast and rename
                mutate(county_id=as.character(county_id)) %>%
                select(county_id, county_mapped=county_name)

# Remove search basis duplicates
search_basis_keys <- c('ER','OB','OI','SM','TIP','WTNS')
search_basis_vals <- c("Erratic Suspicious Behaviour", "Observation Suspected Contraband", "Other Official Info",
                      "Suspicious Movement", "Informant Tip", "Witness Observation")
search_basis <- read_csv('SEARCHBASIS.csv') %>%
                  mutate(Basis=map(Basis, search_basis_keys, search_basis_vals)) %>%
                  group_by(StopID, SearchID, PersonID) %>%
                  summarize(Basis=paste(Basis, collapse=','))

# Combine them all
d <- stops %>%
     left_join(search, by='StopID') %>%
     left_join(search_basis, by=c('StopID', 'SearchID')) %>%
     left_join(contraband, by=c('StopID','SearchID')) %>%
     left_join(person %>% filter(Type=='D'), by='StopID') %>%
     left_join(county_codes, by=c("StopLocation"="county_id")) %>%
     # Separate date,time
     separate(StopDate, c("date","time"), sep=' ', remove=F)

# Dictionaries
race_keys <- c("A","B","I","U","W") 
race_vals <- c("Asian","Black","Other","Other","White")
outcome_keys <- 1:5
outcome_vals <- c("Verbal Warning","Written Warning","Citation","Arrest","No Action")
search_type_keys <- 1:5
search_type_raw_vals = c('Consent', 'Search Warrant', 'Probable Cause', 'Search Incident to Arrest', 'Protective Frisk')
search_type_vals <- c("Consent","Warrant","Probable Cause","Incident to Arrest","Protective Frisk")

# Rename and extract columns
print(sprintf("[%s] extracting columns", this_state))

d$state                 <- this_state
d$stop_date             <- make_date(d$date)
d$stop_time             <- strftime(strptime(substr(d$time,1,5), "%H:%M"), format = '%H:%M')
d$stop_time[as.character(d$stop_time) == '00:00'] <- NA  # overdensity at 00:00, unreliable times 
d$id                    <- make_row_id(d)
d$location_raw          <- d$county_mapped
counties_clean          <- normalize_county(d)
d$county_name           <- counties_clean$county_name
d$county_fips           <- counties_clean$fips
# create a district column (a two-letter code which keeps the location coding consistent across years; it changes mid-2013)
d$district              <- ifelse(nchar(d$location_raw) == 2, # if it's already a two-letter district
                                  d$location_raw, # use the raw two-letter code; otherwise remap county name. 
                                  plyr::mapvalues(d$county_name, from = district_map$county_name, to = district_map$district)) 
d$fine_grained_location <- d$StopCity
d$police_department     <- d$AgencyDescription
d$state_patrol          <- d$police_department == 'SHP - Motor Carrier Enforcement Section' | d$police_department == 'NC State Highway Patrol'
d$driver_gender         <- d$Gender
d$driver_age_raw        <- d$Age
d$driver_age            <- get_age(d)
d$driver_race_raw       <- paste(d$Race, d$Ethnicity)
d$driver_race           <- ifelse(d$Ethnicity=='H', 'Hispanic', map(d$Race, race_keys, race_vals))
d$violation_raw         <- normalize_violation(d, d$Purpose)
d$violation             <- normalize_violation(d, d$Purpose, clean=TRUE)
d$search_conducted      <- !is.na(d$SearchID)
d$search_type_raw       <- map(d$Type.x, search_type_keys, search_type_raw_vals)
d$search_type           <- map(d$Type.x, search_type_keys, search_type_vals)
d$contraband_found      <- !is.na(d$ContrabandID)
d$stop_outcome          <- map(d$Action, outcome_keys, outcome_vals)
d$is_arrested           <- (d$Action == 4) | (d$DriverArrest == 1) | (d$PassengerArrest == 1)
d$stop_outcome[d$is_arrested] = 'Arrest' # if someone is arrested, code stop_outcome as Arrest, consistent with policy for other states: stop_outcome is most severe outcome of stop. 

# Extra columns
d$search_basis          <- d$Basis
d$ethnicity             <- d$Ethnicity
d$officer_id            <- ifelse(d$OfficerId=='Unknown', NA, d$OfficerId)
d$drugs_related_stop    <- (d$Ounces > 0) | (d$Pounds > 0) | (d$Grams > 0) | (d$Kilos > 0) | (d$Dosages > 0)

# Close-up
write_cleaned_state(d, extra_cols=c('search_basis','officer_id','drugs_related_stop', 'ethnicity', 'district'))
change_path(NA)
