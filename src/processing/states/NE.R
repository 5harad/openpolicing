# Processing code for Nebraska
# This is aggregate data

# Set-up
this_state <- 'NE'
change_path(this_state)

# Read in and combine data
print(sprintf("[%s] reading in the data", this_state))
# Read mappings
d_dept <- read_csv("tbl_ORI.csv") %>% select(DeptID=ID, dept=ORI_Description, dept_lvl=ORITYPEID, county=AgencyCounty)
d_time <- read_csv("tbl_Quarters.csv") %>% select(TimeID=QuarterID, TimeStart=`Actual Quarter Date Start`)
d_repo <- read_csv("tbl_Reports.csv") %>% select(ReportID, Race, Reason=TopicDescription, Outcome=DetailDescription)
# Read data
d <- read_csv("tbl_Quarter_ID.csv") %>%
  select(TimeID=QuarterID, DeptID=ID, ReportID, n=Value) %>%
  inner_join(d_time, by='TimeID') %>%
  inner_join(d_dept, by='DeptID') %>%
  inner_join(d_repo, by='ReportID') %>%
  select(-TimeID, -DeptID, -ReportID) %>%
  # Remove Federal agencies, Other and Private agencies, respectively
  filter(!dept_lvl %in% c(4, 7, 12))

# Note : grouping by Reason leads to different sum totals per (Time, Deptartment, Race) group.
# We only look at the 'search' grouping for this script.
d <- d %>% filter(Reason=='Searches')

# Value dictionaries
race_keys <- c("Asian / Pacific Islander", "Native American - Alaskan")
race_vals <- c("Asian", "Other")

# Rename and extract columns
print(sprintf("[%s] extracting columns", this_state))
d$state                 <- this_state
d$stop_date             <- make_date(substr(d$TimeStart, 1, 8), format='%m/%d/%y')
d$stop_time             <- NA  # not included
d$id                    <- make_row_id(d)
d$location_raw          <- ifelse(d$county %in% c('Inactive', 'NSP and Other', 'Private'), NA, d$county)
counties_clean          <- normalize_county(d)
d$county_name           <- counties_clean$county_name
d$county_fips           <- counties_clean$fips
d$fine_grained_location <- NA  # not included
d$state_patrol          <- d$dept_lvl %in% c(1,5,9,10,11)  # these are all state agencies, the rest are local PDs
d$police_department     <- str_to_title(d$dept)
d$driver_gender         <- NA  # not included
d$driver_age_raw        <- NA  # not included
d$driver_age            <- NA  # not included
d$driver_race_raw       <- d$Race
d$driver_race           <- map(d$driver_race_raw, race_keys, race_vals)  # map race with dictionary approach
d$violation_raw         <- NA  # not included
d$violation             <- NA  # included, but not cross-tabulated
d$search_conducted      <- d$Outcome == 'Search Conducted'
d$search_type_raw       <- NA  # not included
d$search_type           <- NA  # not included
d$contraband_found      <- NA  # not included
d$stop_outcome          <- NA  # included, but not cross-tabulated
d$is_arrested           <- NA  # included, but not cross-tabulated

# Un-aggregate rows
n = dim(d)[1]
ids = rep(1:n, d$n)
d <- d[ids, ]
# Redo id's
d$id <- make_row_id(d)

# Close-up
write_cleaned_state(d)
change_path(NA)
