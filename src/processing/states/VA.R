# Processing code for Virginia

# Set-up
this_state <- 'VA'
change_path(this_state)

# Read in and combine data
print(sprintf("[%s] reading in the data", this_state))
d_raw <- read_csv("data.csv")

# Overwrite column names (from documentations)
colnames(d_raw) <- c(
  'WEEK',
  'JURISDICTION CODE',
  'CODE NUMBER',
  'NUMBER OF TRAFFIC ARRESTS - WHITE',
  'NUMBER OF AERIAL ENFORCEMENT ARRESTS - WHITE',
  'NUMBER OF SEARCH ARRESTS - WHITE',
  'NUMBER OF SEARCH STOPS - WHITE',
  'NUMBER OF TRAFFIC ARRESTS - AFRICA/AMERICAN',
  'NUMBER OF AERIAL ENFORCEMENT ARRESTS - AFRICA/AMERICAN',
  'NUMBER OF SEARCH ARRESTS - AFRICA/AMERICAN',
  'NUMBER OF SEARCH STOPS - AFRICA/AMERICAN',
  'NUMBER OF TRAFFIC ARRESTS - HISPANIC',
  'NUMBER OF AERIAL ENFORCEMENT ARRESTS - HISPANIC',
  'NUMBER OF SEARCH ARRESTS - HISPANIC',
  'NUMBER OF SEARCH STOPS - HISPANIC',
  'NUMBER OF TRAFFIC ARRESTS - ASIAN',
  'NUMBER OF AERIAL ENFORCEMENT ARRESTS - ASIAN',
  'NUMBER OF SEARCH ARRESTS - ASIAN',
  'NUMBER OF SEARCH STOPS - ASIAN',
  'NUMBER OF TRAFFIC ARRESTS - INDIAN',
  'NUMBER OF AERIAL ENFORCEMENT ARRESTS - INDIAN',
  'NUMBER OF SEARCH ARRESTS - INDIAN',
  'NUMBER OF SEARCH STOPS - INDIAN',
  'NUMBER OF TRAFFIC ARRESTS - OTHER',
  'NUMBER OF AERIAL ENFORCEMENT ARRESTS - OTHER',
  'NUMBER OF SEARCH ARRESTS - OTHER',
  'NUMBER OF SEARCH STOPS - OTHER',
  'NUMBER OF TRAFFIC ARRESTS - UNKNOWN',
  'NUMBER OF AERIAL ENFORCEMENT ARRESTS - UNKNOWN',
  'NUMBER OF SEARCH ARRESTS - UNKNOWN',
  'NUMBER OF SEARCH STOPS - UNKNOWN',
  'TROOPER LASTNAME',
  'TROOPER FIRSTNAME',
  'TROOPER RACE'
)

# collapse different race colunms
d <- rbind(
  d_raw %>% select(1:3, 32:34, traffic_arrests=4 , search_arrests=6 , search_stops=7 ) %>% mutate(race='White'),
  d_raw %>% select(1:3, 32:34, traffic_arrests=8 , search_arrests=10, search_stops=11) %>% mutate(race='Black'),
  d_raw %>% select(1:3, 32:34, traffic_arrests=12, search_arrests=14, search_stops=15) %>% mutate(race='Hispanic'),
  d_raw %>% select(1:3, 32:34, traffic_arrests=16, search_arrests=18, search_stops=19) %>% mutate(race='Asian'),
  d_raw %>% select(1:3, 32:34, traffic_arrests=20, search_arrests=22, search_stops=23) %>% mutate(race='Indian'),
  d_raw %>% select(1:3, 32:34, traffic_arrests=24, search_arrests=26, search_stops=27) %>% mutate(race='Other'),
  d_raw %>% select(1:3, 32:34, traffic_arrests=28, search_arrests=30, search_stops=31) %>% mutate(race=NA)
) %>%
  distinct() %>%
  # sum different counts together
  mutate(stops=traffic_arrests+search_arrests+search_stops, searches=search_arrests+search_stops) %>%
  # remove rows with zero stops
  filter(stops > 0)

# Dictionaries
trooper_race_keys = c("A","B",'b',"C","D","E","U")
trooper_race_vals = c("White","Black","Black","Hispanic","Asian","Other",NA)

# Rename and extract columns
print(sprintf("[%s] extracting columns", this_state))
d$state                 <- this_state
d$stop_date             <- make_date(paste(substr(d$WEEK, 1, 4), substr(d$WEEK, 5, 6), substr(d$WEEK, 7, 8), sep='-')) # week, not date
d$stop_time             <- NA  # not included
d$id                    <- make_row_id(d)
d$location_raw          <- d$`JURISDICTION CODE`
counties_clean          <- normalize_county(d)
d$county_name           <- counties_clean$county_name
d$county_fips           <- counties_clean$fips
d$fine_grained_location <- NA  # not included
d$state_patrol          <- TRUE
d$police_department     <- NA  # not included
d$driver_gender         <- NA  # not included
d$driver_age_raw        <- NA  # not included
d$driver_age            <- NA  # not included
d$driver_race_raw       <- d$race
d$driver_race           <- map(d$driver_race_raw, c("Indian"), c("Asian"))
d$violation_raw         <- NA  # not included
d$violation             <- NA  # not included
d$search_conducted      <- FALSE  # set later
d$search_type_raw       <- NA  # not included
d$search_type           <- NA  # not included
d$contraband_found      <- NA  # not included
d$stop_outcome          <- NA  # not included
d$is_arrested           <- NA  # not incuded

# Exra columns
d$officer_id            <- d$`CODE NUMBER`
d$officer_race          <- map(d$`TROOPER RACE`, trooper_race_keys, trooper_race_vals)

# De-aggregate the data
n = dim(d)[1]
d <- rbind(
    # Stops without searches
    d[rep(1:n, d$stops-d$searches), ],
    # Stops with searches
    d[rep(1:n, d$searches), ] %>% mutate(search_conducted=TRUE)
  )
# Redo id's
d$id <- make_row_id(d)

# Close-up
write_cleaned_state(d, extra_cols=c('officer_id','officer_race'))
change_path(NA)
