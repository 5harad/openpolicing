# Processing code for Oregon

# Set-up
this_state <- 'OR'
change_path(this_state)

# Read in and combine data
print(sprintf("[%s] reading in the data", this_state))
d <- read_csv("Oregon_Data_Public_Records_Request_54552.csv", col_types="cci")

# Race dictionaries
race_keys = c("African American", "Middle Eastern", "Native American", "Unknown")
race_vals = c("Black", "Other", "Other", NA)

# Data is aggregated, so unaggregate
d = data.frame(
	year=rep(d$year, d$count),
	race=rep(d$race, d$count)
  )

# Rename and extract columns
print(sprintf("[%s] extracting columns", this_state))

d$state                 <- this_state
d$stop_date             <- make_date(paste0(substr(d$year, 1, 4), '-01-01'))
d$stop_time             <- NA  # not included
d$id                    <- make_row_id(d)
d$location_raw          <- NA  # not included
d$county_name           <- NA  # not included
d$county_fips           <- NA  # not included
d$fine_grained_location <- NA  # not included
d$state_patrol          <- TRUE
d$police_department     <- NA  # not included
d$driver_gender         <- NA  # not included
d$driver_age_raw        <- NA  # not included
d$driver_age            <- NA  # not included
d$driver_race_raw       <- d$race
d$driver_race           <- map(d$driver_race_raw, race_keys, race_vals)
d$violation_raw         <- NA  # not included
d$violation             <- NA  # not included
d$search_conducted      <- NA  # not included
d$search_type_raw       <- NA  # not included
d$search_type           <- NA  # not included
d$contraband_found      <- NA  # not included
d$stop_outcome          <- NA  # not included
d$is_arrested           <- NA  # not included

# Close-up
write_cleaned_state(d)
change_path(NA)
