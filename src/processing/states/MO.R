# Processing code for Missouri

# Set-up
this_state <- 'MO'
change_path(this_state)

# Read in and combine data
print(sprintf("[%s] reading in the data", this_state))
d = NULL
for(year in 2010:2015) {
  d_stat = read_csv(paste0(year, "/STATISTICS_RACE_SPECIFIC.csv"))
  d_dpt  = read_csv(paste0(year, "/01_LAW_ENFORCEMENT_AGENCIES.csv"))
  new_d <- inner_join(d_stat, d_dpt, by='AgencyID') %>%
      select(dept=PoliceDepartment, city=WorkCity, race=Race,
             n=TotalStops, ns=TotalStops_Searches, nc=TotalStops_Discovery) %>%
      mutate(
        year=year,
        n=ifelse(is.na(n), 0, n),
        ns=ifelse(is.na(ns), 0, ns),
        nc=ifelse(is.na(nc), 0, nc))
    new_d = filter(new_d, nc <= ns, ns <= n)
    d = rbind(d, new_d)
}

# De-aggregate the data
d <- rbind(
  # Searched and found
  data.frame(
    year=rep(d$year, d$nc),
    dept=rep(d$dept, d$nc),
    city=rep(d$city, d$nc),
    race=rep(d$race, d$nc)
  ) %>% mutate(search_conducted = TRUE, contraband_found = TRUE),
  # Searched and not found
  data.frame(
    year=rep(d$year, d$ns-d$nc),
    dept=rep(d$dept, d$ns-d$nc),
    city=rep(d$city, d$ns-d$nc),
    race=rep(d$race, d$ns-d$nc)
  ) %>% mutate(search_conducted = TRUE, contraband_found = FALSE),
  # Not searched and not found
  data.frame(
    year=rep(d$year, d$n - d$ns),
    dept=rep(d$dept, d$n - d$ns),
    city=rep(d$city, d$n - d$ns),
    race=rep(d$race, d$n - d$ns)
  ) %>% mutate(search_conducted = FALSE, contraband_found = FALSE)
 )

# Rename and extract columns
print(sprintf("[%s] extracting columns", this_state))
d$state                 <- this_state
d$stop_date             <- make_date(paste0(d$year, '-01-01'))
d$stop_time             <- NA  # not included
d$id                    <- make_row_id(d)
# All of state patrol is mapped to the same city
# 'city' here refers to the city of the police department's office, generally resides within a county
d$location_raw          <- ifelse(d$dept == 'Missouri State Highway Patrol', NA, as.character(d$city))
counties_clean          <- normalize_county(d)
d$county_name           <- counties_clean$county_name
d$county_fips           <- counties_clean$fips
d$fine_grained_location <- NA  # not included
d$state_patrol          <- d$dept == 'Missouri State Highway Patrol'
d$police_department     <- d$dept
d$driver_gender         <- NA  # not included
d$driver_age_raw        <- NA  # not included
d$driver_age            <- NA  # not included
d$driver_race_raw       <- d$race
d$driver_race           <- map(d$driver_race_raw, c("Native American"), c("Other"))
d$violation_raw         <- NA  # not included
d$violation             <- NA  # not included
d$search_conducted      <- d$search_conducted
d$search_type_raw       <- NA  # not included
d$search_type           <- NA  # not included
d$contraband_found      <- d$contraband_found
d$stop_outcome          <- NA  # not included
d$is_arrested           <- NA  # not included

# Close-up
write_cleaned_state(d)
change_path(NA)
