# Processing code for Georgia

# Set-up
this_state <- 'GA'
change_path(this_state)

# Read in and combine data
print(sprintf("[%s] reading in the data", this_state))

d <- lapply(list.files('.'), function(fn) {
    read_csv(fn, col_names=F, col_types=paste(rep('c',103), collapse=''))
  }) %>%
  bind_rows()

# separate date and time
d <- d %>%
  separate(X4, c("date","time"), " ")

# Group same stop rows to account for multiple violations
d <- d %>%
  group_by(date,time,X9,X11,X13,X29,X30,X31,X33,X39,X48,X49,X52,X53,X56) %>%
  summarize(
    dob  = get_unique_value_if_exists(X14),
    sex  = get_unique_value_if_exists(X17),
    race = get_unique_value_if_exists(X15),
    loc  = paste(na.omit(unique(X42)), collapse=','),
    violation = str_to_lower(paste0(X103, collapse = ','))
  ) %>%
  ungroup()

# Value dictionaries
race_keys <- c("A","B","H","I","O","W","")
race_vals <- c("Asian","Black","Hispanic","Native American","Other","White",NA)
race_vals_clean <- c("Asian","Black","Hispanic","Other","Other","White",NA)

# Rename and extract columns
print(sprintf("[%s] extracting columns", this_state))
d$state                 <- this_state
d$stop_date             <- make_date(d$date)
d$stop_time             <- strftime(strptime(substr(d$time, 1, 5), "%H:%M"), format='%H:%M')
d$id                    <- make_row_id(d)
d$location_raw          <- as.character(d$X9)
counties_clean          <- normalize_county(d)
d$county_name           <- counties_clean$county_name
d$county_fips           <- counties_clean$fips
d$fine_grained_location <- paste(d$X11, d$loc)  # city, location
d$state_patrol          <- TRUE
d$police_department     <- d$X13
d$driver_gender         <- ifelse(d$sex %in% c("M","F"), d$sex, NA)
d$driver_age_raw        <- substr(d$dob, 1, 10)
d$driver_age            <- get_age(d, type='birthdate')
d$driver_race_raw       <- map(d$race, race_keys, race_vals)
d$driver_race           <- map(d$race, race_keys, race_vals_clean)
d$driver_race           <- ifelse(d$driver_race %in% c("Asian","Black","Hispanic","Other","White", NA), d$driver_race, "Other")
d$violation_raw         <- normalize_violation(d, str_to_lower(d$violation), clean=F)
d$violation             <- normalize_violation(d, str_to_lower(d$violation), clean=T)
d$search_type_raw       <- NA  # not included
d$search_type           <- NA  # not included
d$search_conducted      <- NA  # not included
d$contraband_found      <- NA  # not included
d$stop_outcome          <- "Warning"
d$is_arrested           <- NA  # not included

# Extra fields
d$lat                   <- d$X48
d$lon                   <- d$X49
d$officer_id            <- str_pad(d$X56, 4, pad='0')
d$officer_troop         <- d$X52
d$officer_rank          <- d$X53
d$out_of_state          <- d$X39 == 'GA'
d$vehicle_make          <- d$X30
d$vehicle_model         <- d$X31
d$vehicle_color         <- d$X33
d$vehicle_year          <- d$X29

# Close-up
write_cleaned_state(d, extra_cols=c('lat','lon','officer_id','officer_troop','officer_rank',
    'out_of_state','vehicle_make','vehicle_model','vehicle_color','vehicle_year'))
change_path(NA)
