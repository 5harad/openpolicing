# Processing code for North Dakota

# Set-up
this_state <- 'ND'
change_path(this_state)

# Read in and combine data
print(sprintf("[%s] reading in the data", this_state))
d10 <- read_csv("Request2010-2012.csv")
d13 <- read_csv("Request2013-2015june.csv")
# Put it all together
d <- rbind(d10, d13) %>%
  # separate out date and time
  separate(violation_date_time, c("date","time"), ' ')

# Each row in the dataset is not a single stop, but a single citation,
# so we group by the other values to remove duplicates. 
d <- group_by(d, 
              date, 
              time, 
              Age, 
              sex, 
              Race, 
              county_name, 
              street_cnty_rd_location, 
              highway, 
              hwy_suffix, 
              desc_of_area) %>% 
  summarize(
    century_code_viol = paste(century_code_viol, collapse=';'), 
    description_50 = paste(description_50, collapse=';')
  ) %>% ungroup() 

# Value dictionaries
race_keys <- c("African American", "Native American")
race_vals <- c("Black", "Other")
# Rename and extract columns
print(sprintf("[%s] extracting columns", this_state))
d$state                 <- this_state
d$stop_date             <- make_date(d$date)
d$stop_time             <- strftime(strptime(d$time, "%H:%M:%S"), format = '%H:%M')
d$id                    <- make_row_id(d)
d$location_raw          <- d$county_name
counties_clean          <- normalize_county(d)
d$county_name           <- counties_clean$county_name
d$county_fips           <- counties_clean$fips
d$fine_grained_location <- paste(d$street_cnty_rd_location, d$highway, d$hwy_suffix, d$desc_of_area)
d$state_patrol          <- TRUE
d$police_department     <- NA  # not included
d$driver_gender         <- d$sex
d$driver_age_raw        <- d$Age
d$driver_age            <- get_age(d, type='age')
d$driver_race_raw       <- d$Race
d$driver_race           <- map(d$Race, race_keys, race_vals)  # map race with dictionary approach
d$violation_raw         <- normalize_violation_multiple(d, d$description_50, sep = ';')
d$violation             <- normalize_violation_multiple(d, d$description_50, clean=TRUE, sep = ';')
d$search_conducted      <- NA  # not included
d$search_type_raw       <- NA  # not included
d$search_type           <- NA  # not included
d$contraband_found      <- NA  # not included
d$stop_outcome          <- NA  # not included
d$is_arrested           <- NA  # not included

# Extra fields
d$drugs_related_stop    <- grepl('1903404|1903403', d$century_code_viol)

# Close-up
write_cleaned_state(d, extra_cols=c('drugs_related_stop'))
change_path(NA)
