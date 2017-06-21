# Processing code for New Hampshire

# Set-up
this_state <- 'NH'
change_path(this_state)

# Read in and combine data
print(sprintf("[%s] reading in the data", this_state))

# Read in column names (which differ per file)
c14.1 <- read_csv("NewHampshire_2014_redacted_eTicket_file_NHSP_sheet1.csv", n_max=10) %>%colnames()
c14.2 <- c(c14.1[1:44], "DEF_COMPANY", c14.1[45:56])
c14.3 <- c(c14.1[1:44], "DEF_COMPANY", c14.1[45], "DEF_BIRTH_DATE", c14.1[46:48], "STATE_CDE", c14.1[49:56])
c15   <- read_csv("NewHampshire_2015_eticket_totals_by_trooper_sheet1.csv", n_max=10) %>% colnames()

# Read data files with different column names
d14.1 <- read_csv("NewHampshire_2014_redacted_eTicket_file_NHSP_sheet1.csv", col_names=c14.1, skip=1, col_types = cols(INFRACTION_TIME = col_character()))
d14.2 <- read_csv("NewHampshire_2014_redacted_eTicket_file_NHSP_sheet2.csv", col_names=c14.2, col_types = cols(INFRACTION_TIME = col_character()))
d14.3 <- read_csv("NewHampshire_2014_redacted_eTicket_file_NHSP_sheet3.csv", col_names=c14.3, col_types = cols(INFRACTION_TIME = col_character())) 
d15.1 <- read_csv("NewHampshire_2015_eticket_totals_by_trooper_sheet1.csv" , col_names=c15, skip=1, col_types = cols(INFRACTION_TIME = col_character()))
d15.2 <- read_csv("NewHampshire_2015_eticket_totals_by_trooper_sheet2.csv" , col_names=c15, col_types = cols(INFRACTION_TIME = col_character()))
d15.3 <- read_csv("NewHampshire_2015_eticket_totals_by_trooper_sheet3.csv" , col_names=c15, col_types = cols(INFRACTION_TIME = col_character()))

# Add in the new columns where required
d14.1 <- cbind(d14.1[, c(1:44)], data.frame(DEF_COMPANY=NA)   , d14.1[, c(45)], data.frame(DEF_BIRTH_DATE=NA), d14.1[, c(46:48)], data.frame(STATE_CDE=NA), d14.1[, c(49:56)]) %>% as.tbl() %>% mutate(DEF_COMPANY=as.character(DEF_COMPANY), DEF_BIRTH_DATE=as.character(DEF_BIRTH_DATE), STATE_CDE=as.character(STATE_CDE))
d14.2 <- cbind(d14.2[, c(1:46)], data.frame(DEF_BIRTH_DATE=NA), d14.2[, c(47:49)], data.frame(STATE_CDE=NA), d14.2[, c(50:57)]) %>% as.tbl() %>% mutate(DEF_BIRTH_DATE=as.character(DEF_BIRTH_DATE), STATE_CDE=as.character(STATE_CDE))
d14.3 <- d14.3  # reference
d15.1 <- cbind(d15.1[, c(1:44)], data.frame(DEF_COMPANY=NA), d15.1[, c(45:58)]) %>% as.tbl() %>% mutate(DEF_COMPANY=as.character(DEF_COMPANY))
d15.2 <- cbind(d15.2[, c(1:44)], data.frame(DEF_COMPANY=NA), d15.2[, c(45:58)]) %>% as.tbl() %>% mutate(DEF_COMPANY=as.character(DEF_COMPANY))
d15.3 <- cbind(d15.3[, c(1:44)], data.frame(DEF_COMPANY=NA), d15.3[, c(45:58)]) %>% as.tbl() %>% mutate(DEF_COMPANY=as.character(DEF_COMPANY))

# Join them all together
d14  <- rbind(d14.1, d14.2, d14.3)
d15  <- rbind(d15.1, d15.2, d15.3)
d <- rbind(d14, d15)

# Remove duplicates
d <- d %>%
  group_by(INFRACTION_DATE, INFRACTION_TIME, INFRACTION_COUNTY_NAME, INFRACTION_CITY_NME,
           INFRACTION_LOCATION_TXT, GENDER_CDE, DEF_BIRTH_DATE, RACE_CDE, LATITUDE,
           LONGITUDE, LICENSE_STATE_CDE, AIRCRAFT_EVENT_ID) %>%
  summarize(
  	INFRACTION_RSA_CDE    = paste(unique(INFRACTION_RSA_CDE), collapse=','),
  	CITATION_RESPONSE_DSC = paste(unique(CITATION_RESPONSE_DSC), collapse=',')) %>%
  ungroup() 

# Value dictionaries

# RACE_CDE , inspect with : data.frame(from=race_keys, to=race_vals)
# Extremely messy data so we just do our best. We do not use NH in our main analysis because the race data is so messy. 
# the values below cover >99% of cases. Beyond that codes often become hard to interpret and we don't want to make assumptions, so we simply truncate at the most common codes.
# There's also a long tail of misspelled versions of "white" all beginning with W which we map to white. 
# In any case 36% of stops are missing race data entirely, so the long tail of mispellings and ambiguous codes does not make much difference to the bad race data. 
race_keys <- c('WHT', NA, 'W', 'BLK', 'HIS', 'UNK', 'ASI', 'BLACK/', 'PIS', 'WHI', 'LAT', 'BLACK', 'Indian')
race_vals <- c('White', NA, 'White', 'Black', 'Hispanic', NA, 'Asian', 'Black', 'Asian', 'White', 'Hispanic', 'Black', 'Asian')

# Citation codes
citation_keys <- c("C", "MA", "PBM", "W")
citation_vals <- c("Checkup", "Summons", "Ticket", "Warning") # citation code interpretations provided by NH police. 

# Rename and extract columns
print(sprintf("[%s] extracting columns", this_state))

d$state                 <- this_state
d$stop_date             <- make_date(d$INFRACTION_DATE, "%m/%d/%Y")
d$stop_time             <- substr(strptime(d$INFRACTION_TIME, "%I:%M %p" ),12,16)
d$id                    <- make_row_id(d)
d$location_raw          <- d$INFRACTION_COUNTY_NAME
counties_clean          <- normalize_county(d)
d$county_name           <- counties_clean$county_name
d$county_fips           <- counties_clean$fips
d$fine_grained_location <- paste(d$INFRACTION_CITY_NME, d$INFRACTION_LOCATION_TXT)
d$state_patrol          <- TRUE
d$police_department     <- NA  # not included
d$driver_gender         <- d$GENDER_CDE
d$driver_age_raw        <- d$DEF_BIRTH_DATE
d$driver_age            <- get_age(d, type='birthdate', format="%m/%d/%Y")
d$driver_race_raw       <- d$RACE_CDE
d$driver_race           <- ifelse(d$RACE_CDE %in% race_keys, map(d$RACE_CDE, race_keys, race_vals), NA)
d$driver_race[tolower(str_sub(d$driver_race, 1, 1)) == 'w'] = 'White' # long tail of misspelled versions of "white" all beginning with W 
d$violation_raw         <- normalize_violation_multiple(d, d$INFRACTION_RSA_CDE, clean=FALSE)
d$violation             <- normalize_violation_multiple(d, d$INFRACTION_RSA_CDE, clean=TRUE)
d$search_conducted      <- NA  # not included
d$search_type_raw       <- NA  # not included
d$search_type           <- NA  # not included
d$contraband_found      <- NA  # not included
d$stop_outcome          <- multimap(d$id, d$CITATION_RESPONSE_DSC, citation_keys, citation_vals) 
d$stop_outcome          <- ifelse(grepl('Summons', d$stop_outcome), 'Summons', # take most severe stop outcome, consistent with other states. 
                           ifelse(grepl('Ticket', d$stop_outcome), 'Ticket', 
                           ifelse(grepl('Warning', d$stop_outcome), 'Warning',
                           ifelse(grepl('Checkup', d$stop_outcome), 'Checkup', NA))))
                                
d$is_arrested           <- NA

# Convert hour-based coordinate location to numeric lat/lng values
geo_hour_to_latlng <- Vectorize(function(str) {
  if(is.na(str)) { return(NA) }
  Q <- substr(str, 0, 1)
  D <- substr(str, 2, 3)
  M <- substr(str, 5, 11)
  as.numeric(sp::char2dms(paste(D, 'd', M, '\'', Q, sep='')))
})

# Extra fields
d$lat                   <- geo_hour_to_latlng(d$LATITUDE)   # map lat/lng with manual function
d$lon                   <- geo_hour_to_latlng(d$LONGITUDE)  # map lat/lng with manual function
d$out_of_state          <- d$LICENSE_STATE_CDE != 'NH'
d$aerial_enforcement    <- !is.na(d$AIRCRAFT_EVENT_ID)
# Driving under influence is statute 265A, but no records of it...

# Close-up
write_cleaned_state(d, extra_cols=c('lat','lon','out_of_state','aerial_enforcement'))
change_path(NA)
