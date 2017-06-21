# Processing code for Connecticut

# Set-up
this_state <- 'CT'
change_path(this_state)

# Read in and combine data
print(sprintf("[%s] reading in the data", this_state))
d <- read_csv("Connecticut_DownloadedMarch27_2016_Racial_Profiling_Prohibition_Project_Traffic_Stop_Data.csv", 
              col_types = cols(`Intervention Time` = col_character()))

# Combine duplicates
message(sprintf("Prior to combining duplicates, %i rows.", nrow(d)))
d <- d %>% group_by(
  `Intervention Date`, `Intervention Time`, `Intervention Location Name`, `Subject Age`, 
  `Intervention Location Description Text`, `Department Name`, `Subject Ethnicity Code`, 
  `Subject Sex Code`, `Search Authorization Code`, `Vehicle Searched Indicator`,
  `Contraband Indicator`, `Subject Race Code`,
  `Custodial Arrest Indicator`, `Reporting Officer Identification ID`, `Intervention Duration Code`) %>%
  summarize(`Statute Code Description` = paste(sort(unique(`Statute Code Description`)), collapse=','), 
            `Intervention Disposition Code` = paste(`Intervention Disposition Code`, collapse = ',')) %>%
  ungroup()
message(sprintf("After combining duplicates, %i rows.", nrow(d)))

# Value dictionaries
race_keys <- c("W", "B", "A", "H", "I")
race_vals <- c("White", "Black", "Asian", "Hispanic", "Native American")
race_vals_clean <- c("White", "Black", "Asian", "Hispanic", "Other")
outcome_keys <- c("I","M","N","U","V","W")
outcome_vals <- c("Ticket","Summons",NA,"Arrest","Verbal Warning","Written Warning") 
search_type_keys <- c("C","I","N","O")
search_type_vals <- c("Consent", "Inventory", NA, "Other")
search_type_vals_clean <- c("Consent", "Inventory", NA, "Other")

# Rename and extract columns
print(sprintf("[%s] extracting columns", this_state))
d$state                 <- this_state
d$stop_date             <- make_date(substr(d$`Intervention Date`, 1, 10), format='%m/%d/%Y')
d$stop_time             <- strftime(strptime(d$`Intervention Time`, "%H:%M"), format = '%H:%M')
d$stop_time[as.character(d$stop_time) == '00:00'] = NA  # We have an overdensity of stops at midnight; this probably indicates unreliable data, setting these to NA. 
d$id                    <- make_row_id(d)
d$location_raw          <- tolower(d$`Intervention Location Name`)
counties_clean          <- normalize_county(d)
d$county_name           <- counties_clean$county_name
d$county_fips           <- counties_clean$fips
d$fine_grained_location <- d$`Intervention Location Description Text`
d$state_patrol          <- d$`Department Name` == 'State Police'
d$police_department     <- d$`Department Name`
d$driver_gender         <- d$`Subject Sex Code`
d$driver_age_raw        <- d$`Subject Age`
d$driver_age            <- get_age(d, type='age')
d$driver_race_tmp       <- ifelse(d$`Subject Ethnicity Code` == 'H', d$`Subject Ethnicity Code`, d$`Subject Race Code`)
d$driver_race_raw       <- map(d$driver_race_tmp, race_keys, race_vals)
d$driver_race           <- map(d$driver_race_tmp, race_keys, race_vals_clean)
d$violation_raw         <- d$`Statute Code Description`
d$violation             <- normalize_violation_multiple(d, d$`Statute Code Description`, clean=TRUE)
d$search_conducted      <- d$`Vehicle Searched Indicator` | (d$`Search Authorization Code` != 'N')
d$search_type_raw       <- map(d$`Search Authorization Code`, search_type_keys, search_type_vals)
d$search_type           <- map(d$`Search Authorization Code`, search_type_keys, search_type_vals_clean)
d$contraband_found      <- d$`Contraband Indicator`
d$contraband_found[!d$search_conducted] <- FALSE  # Keep search_conducted + contraband_found consistent: if no search is conducted, contraband cannot be found as result of search. 
d$stop_outcome          <- multimap(d$id, d$`Intervention Disposition Code`, outcome_keys, outcome_vals, sep = ',')
# If a stop has multiple outcomes, report most severe outcome, consistent with other states. 
d$stop_outcome          <- ifelse(grepl('Arrest', d$stop_outcome) | d$`Custodial Arrest Indicator`, 'Arrest', 
                           ifelse(grepl('Summons', d$stop_outcome), 'Summons', 
                           ifelse(grepl('Ticket', d$stop_outcome), 'Ticket', 
                           ifelse(grepl('Written Warning', d$stop_outcome), 'Written Warning', 
                           ifelse(grepl('Verbal Warning', d$stop_outcome), 'Verbal Warning', NA)))))
d$is_arrested           <- d$stop_outcome == 'Arrest'

# Extra fields
d$officer_id            <- d$`Reporting Officer Identification ID`
d$stop_duration         <- map(d$`Intervention Duration Code`, 1:3, c("1-15 min", "16-30 min", "30+ min"))

# Close-up
write_cleaned_state(d, extra_cols=c('officer_id', 'stop_duration'))
change_path(NA)
