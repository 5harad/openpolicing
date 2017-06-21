# Processing code for Ohio

# Set-up
this_state <- 'OH'
change_path(this_state)

# Read in and combine data
print(sprintf("[%s] reading in the data", this_state))

# Read in data
d10 <- read_csv("OSHP_2010.csv")
d11 <- read_csv("OSHP_2011.csv")
d12 <- read_csv("OSHP_2012.csv")
d13 <- read_csv("OSHP_2013.csv")
d14 <- read_csv("OSHP_2014.csv")
d15.1 <- read_csv("OSHP_2015_mostOfTheYear.csv")
d15.2 <- read_csv("OSHP_2015_endOfTheYear.csv", col_names=colnames(d15.1))

# Join them all together
d <- rbind(d10, d11, d12, d13, d14, d15.1, d15.2) %>%
  # split date column
  separate(DATE_STAMP, c("date", "time"), sep=" ")
# Value dictionaries

# inspect with : data.frame(from=race_keys, to=race_vals)
race_keys <- c("1", "2", "3", "4", "5", "6")
race_vals <- c("White", "Black", "Hispanic", "Asian", "Native American", "Unknown")
race_vals_clean <- c("White", "Black", "Hispanic", "Asian", "Other", NA)
search_type_keys <- c('Commercial Vehicle K-9 Search Conducted', 'Any K9 Used for Search', 'Consent to Search')
search_type_vals <- c('K9 Search', 'K9 Search', 'Consent')

# Rename and extract columns
print(sprintf("[%s] extracting columns", this_state))

d$state                 <- this_state
d$stop_date             <- make_date(d$date, "%m/%d/%Y")
d$stop_time             <- substr(strptime(d$time, "%H:%M:%S"), 12, 16)
d$id                    <- make_row_id(d)
d$location_raw          <- as.character(d$COUNTY_CODE)
counties_clean          <- normalize_county(d)
d$county_name           <- counties_clean$county_name
d$county_fips           <- counties_clean$fips
d$fine_grained_location <- d$ADDRESS
d$state_patrol          <- TRUE
d$police_department     <- NA  # not included
# extract the 'gender + race' field from the disposition code
d$gender_race           <- str_extract(d$DISP_STRING, "[1-6][FM]")
d$driver_gender         <- substr(d$gender_race, 2, 2)
d$driver_age_raw        <- NA  # not included
d$driver_age            <- NA  # not included
d$driver_race_raw       <- map(substr(d$gender_race, 1, 1), race_keys, race_vals)
d$driver_race           <- map(substr(d$gender_race, 1, 1), race_keys, race_vals_clean)
d$violation_raw         <- normalize_violation_multiple(d, toupper(as.character(d$ORC_STRING)))
d$violation             <- normalize_violation_multiple(d, toupper(as.character(d$ORC_STRING)), clean=TRUE)
d$search_conducted      <- grepl('24', d$DISP_STRING)
d$search_type_raw       <- ifelse(grepl('24CK', d$DISP_STRING), 'Commercial Vehicle K-9 Search Conducted', 
                           ifelse(grepl('24K', d$DISP_STRING), 'Any K9 Used for Search', 
                           ifelse(grepl('24R', d$DISP_STRING), 'Consent to Search', NA)))
d$search_type_raw       <- ifelse(grepl('24R', d$DISP_STRING), 'Consent to Search', d$search_type_raw)
d$search_type           <- map(d$search_type_raw, search_type_keys, search_type_vals)
d$contraband_found      <- NA  # not included
d$stop_outcome          <- ifelse(grepl('75ARR|75OVI|75WAR|76WAR', d$DISP_STRING), 'Arrest', # all disposition codes with "arrest" in them
                           ifelse(grepl('WARN', d$DISP_STRING), 'Warning', NA)) # only one disposition code with "warning", can't find disposition codes that clearly indicate whether a ticket was given. 
d$is_arrested           <- grepl('75ARR|75OVI|75WAR|76WAR', d$DISP_STRING)

# Extra columns
d$lat                   <- d$LATITUDE
d$lon                   <- d$LONGITUDE
d$officer_id            <- d$UNIT
d$drugs_related_stop    <- grepl('2925', d$ORC_STRING)

# Close-up
write_cleaned_state(d, extra_cols=c('lat','lon','officer_id','drugs_related_stop'))
change_path(NA)
