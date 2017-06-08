# Processing code for Michigan

# Set-up
this_state <- 'MI'
change_path(this_state)

# Read in and combine data
print(sprintf("[%s] reading in the data", this_state))
colnames <- read_csv("colnames.csv", col_names='col')
d <- read_csv("data.csv", col_names=colnames$col, na=c("NA","","NULL")) %>%
  # Separate out date and time
  separate(TicketDate, c("date","time"), " ")

# Value dictionaries
race_keys <- c("A", "B", "H", "I", "U", "W")
race_vals <- c("Asian", "Black", "Hispanic", "Native American", "Unknown", "White")
race_vals_clean <- c("Asian", "Black", "Hispanic", "Other", NA, "White")

# Do a group_by to remove duplicate stops because each stop corresponds not to a stop but to a violation reason. 
print(sprintf("Prior to removing duplicates, %i rows.", nrow(d)))
d = group_by(d, PrimaryOfficerID, date, time, VehicleID, CountyCode, UponStreet, Department, Race, ArrestNum) %>% 
  summarise(Felony = max(Felony), 
            Misdemeanor = max(Misdemeanor), 
            CivilInfraction = max(CivilInfraction), 
            Warning = max(Warning), 
            Description = paste0(Description, collapse = ';;')) %>%
  ungroup()
print(sprintf("After removing duplicates, %i rows.", nrow(d)))

# Rename and extract columns
print(sprintf("[%s] extracting columns", this_state))
d$state                 <- this_state
d$stop_date             <- make_date(d$date)
d$stop_time             <- strftime(strptime(d$time, "%H:%M:%S"), format='%H:%M')
d$id                    <- make_row_id(d)
d$location_raw            <- d$CountyCode
counties_clean          <- normalize_county(d)
d$county_name           <- counties_clean$county_name
d$county_fips           <- counties_clean$fips
d$fine_grained_location <- d$UponStreet
d$state_patrol          <- TRUE
d$police_department     <- d$Department
d$driver_gender         <- NA  # not included
d$driver_age_raw        <- NA  # not included
d$driver_age            <- NA  # not included
# There is one weird race value to scrub
d$Race                  <- ifelse(d$Race %in% race_keys, d$Race, NA)
d$driver_race_raw       <- map(d$Race, race_keys, race_vals)
d$driver_race           <- map(d$Race, race_keys, race_vals_clean)
d$violation_raw         <- d$Description
d$violation             <- normalize_violation_multiple(d, d$Description, clean=TRUE, sep = ';;')
d$search_conducted      <- NA  # not included
d$search_type_raw       <- NA  # not included
d$search_type           <- NA  # not included
d$contraband_found      <- NA  # not included
# stop outcome is the most severe outcome of the stop, consistent with other states. 
d$stop_outcome          <- ifelse(!is.na(d$ArrestNum), 'Arrest', 
                           ifelse(d$Felony == 1, 'Felony',
                           ifelse(d$Misdemeanor == 1, 'Misdemeanor',
                           ifelse(d$CivilInfraction == 1, 'Infraction',
                           ifelse(d$Warning == 1, 'Warning', NA)))))
d$is_arrested           <- !is.na(d$ArrestNum)

# Extra fields
d$officer_id            <- d$PrimaryOfficerID

# Close-up
write_cleaned_state(d, extra_cols=c('officer_id'))
change_path(NA)
