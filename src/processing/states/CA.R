# Processing code for California

# Set-up
this_state <- 'CA'
change_path(this_state)

# Read in and combine data
print(sprintf("[%s] reading in the data", this_state))
files <- list.files(path='.', pattern='*.csv')
d = NULL
for(fn in files){
 d = rbind(d,  read_csv(fn, na=c("", "NA","NULL")))
}

# Separate date and time
d <- d %>%
  # separate Date for date
  separate(Date, c("date1","time1"), " ") %>%
  # separate Shift for time
  separate(Shift, c("date2","time2"), " ")

# Value dictionaries
race_keys <- c("A","B","C","D","F","G","H","I","J","K","L","O","P","S","U","V","W","Z","M")
# Map races to standardized values -- map "Indian" to "Other" because it seems like it's being used to denote "American Indian" (since they have an "Asian Indian" category).
race_vals <- c("Other Asian","Black","Chinese","Cambodian", "Filipino","Guamanian", "Hispanic","Indian","Japanese","Korean","Laotian","Other","Other Pacific Islander","Samoan","Hawaiian","Vietnamese","White","Asian Indian","M")
race_vals_clean <- c("Asian","Black","Asian","Asian","Asian","Asian", "Hispanic","Other","Asian","Asian","Asian","Other","Asian","Asian","Asian","Asian","White","Asian",NA)  # We map Pacific Islanders to Asian. 
search_type_keys <- 0:9
search_type_vals <- c('No Search',"Probable Cause (positive)","Probable Cause (negative)","Consent (positive) 202D Required",
	                  "Consent (negative) 202D Required","Incidental to Arrest","Vehicle Inventory","Parole/Probation/Warrant","Other","Pat Down/Frisk")
search_type_vals_clean <- c(NA,"Probable Cause","Probable Cause","Consent", "Consent","Incident to Arrest","Inventory","Warrant","Other","Protective Frisk")
outcome_keys <- 0:9
outcome_vals <- c(NA, "Arrest","CHP 215","CHP 281", "Verbal Warning","Traffic Collision","Motorist/Public Service","CVSA Sticker","Turnover/Agency Assist", NA)
age_keys <- 1:6
age_vals <- c("0-14","15-25","25-32","33-39","40-48","49+")
# special dictionary for original location code name
ca_map <- read_csv(sprintf("%s/resources/dictionaries/ca_districts.csv", code_path))

# Rename and extract columns
print(sprintf("[%s] extracting columns", this_state))
d$state                 <- this_state
d$stop_date             <- make_date(d$date1, format='%m/%d/%Y')
d$stop_time             <- NA  # shift time is provided but this is not granular enough to use as stop time
d$id                    <- make_row_id(d)
d$location_raw            <- as.character(d$LocationCode)
counties_clean          <- normalize_county(d)
d$county_name           <- counties_clean$county_name
d$county_fips           <- counties_clean$fips
d$location_raw            <- map(d$location_raw, ca_map$location_code, ca_map$district_name)  # remap to get original location_raw
d$fine_grained_location <- NA  # not included
d$state_patrol          <- TRUE
d$police_department     <- NA  # not included
d$driver_gender         <- d$Gender
d$driver_age_raw        <- map(d$Age, age_keys, age_vals)
d$driver_age            <- NA  # not included -- or at any rate, we don't have the precise age
d$driver_race_raw       <- map(d$Ethnicity, race_keys, race_vals)
d$driver_race           <- map(d$Ethnicity, race_keys, race_vals_clean)
d$violation_raw         <- normalize_violation(d, d$Reason)
d$violation             <- normalize_violation(d, d$Reason, clean=TRUE)
d$search_type_raw       <- map(d$Description, search_type_keys, search_type_vals)
d$search_type           <- map(d$Description, search_type_keys, search_type_vals_clean)
d$search_conducted      <- d$search_type_raw != 'No Search'
d$contraband_found      <- ifelse(d$search_type_raw %in% c('Consent (positive) 202D Required','Probable Cause (positive)'), TRUE, 
                           ifelse(d$search_type_raw %in% c('No Search','Consent (negative) 202D Required','Probable Cause (negative)'), FALSE, NA))
d$stop_outcome          <- map(d$Result, outcome_keys, outcome_vals)
d$is_arrested           <- d$stop_outcome == 'Arrest'

# Extra fields
d$ethnicity            <- d$Ethnicity

# Close-up
write_cleaned_state(d, extra_cols=c('ethnicity'))
change_path(NA)
