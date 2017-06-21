# Processing code for South Dakota

# Set-up
this_state <- 'SD'
change_path(this_state)

# Read in and combine data
print(sprintf("[%s] reading in the data", this_state))
# Get the right column names
cols <- read_csv(list.files(path='.')[2], n_max=10) %>% select(-1, -County) %>% colnames()
# read the data
d <- NULL
for (fn in list.files(path='.')) {
  d_new <- read_csv(fn) %>%
    # remove bogus first column
    select(-1) %>% 
    # remove bogus lines which are '*******'
    filter(is.na(County)) %>% select(-County)
  # remove bogus last summation line
  d_new <- head(d_new, n=dim(d_new)[1]-1)
  # get the right columns
  d_new <- d_new[, cols]
  # deduplicate rows
  d_new <- distinct(d_new) %>%
  # add warning/citation
    mutate(outcome = ifelse(grepl('WARN', fn), 'Warning', 'Citation'))
  # add to the data
  d <- rbind(d, d_new)
}

# separate out date and time
d <- separate(d, `Issued Date/Time`, c("date","time"), ' ')

# Rename and extract columns
print(sprintf("[%s] extracting columns", this_state))
d$state                 <- this_state
d$stop_date             <- make_date(d$date, format='%Y/%m/%d')
d$stop_time             <- strftime(strptime(d$time, "%H:%M:%S"), format = '%H:%M')
d$id                    <- make_row_id(d)
d$location_raw          <- tolower(d$`County Freeform`)
counties_clean          <- normalize_county(d)
d$county_name           <- counties_clean$county_name
d$county_fips           <- counties_clean$fips
d$fine_grained_location <- d$`Location Freeform`
d$state_patrol          <- TRUE
d$police_department     <- NA  # not included
d$driver_gender         <- map(d$Sex, c("Female","Male","Unknown"), c("F","M",NA))
d$driver_age_raw        <- NA  # not included
d$driver_age            <- NA  # not included
d$driver_race_raw       <- NA  # not included
d$driver_race           <- NA  # not included
d$violation_raw         <- d$`Statutes/Charges`
d$violation             <- normalize_violation_multiple(d, d$`Statutes/Charges`, sep=';', clean=TRUE)
d$search_conducted      <- NA  # not included
d$search_type_raw       <- NA  # not included
d$search_type           <- NA  # not included
d$contraband_found      <- NA  # not included
d$stop_outcome          <- d$outcome
d$is_arrested           <- NA  # not included

# Extra fields
d$vehicle_type          <- paste(d$Make, d$Model, d$Year)
d$out_of_state          <- d$`Plate State` != 'SD'
d$drugs_related_stop    <- grepl("Marijuana|Possession Controlled", d$violation_raw)

# Close-up
write_cleaned_state(d, extra_cols=c('vehicle_type','out_of_state','drugs_related_stop'))
change_path(NA)
