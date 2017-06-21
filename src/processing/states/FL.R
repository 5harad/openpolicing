# Processing code for Florida. 
# Florida data comes in two formats, which we combine. 

# Set-up
this_state <- 'FL'
change_path(this_state)

# Read in and combine data
print(sprintf("[%s] reading in the data", this_state))

# Read old data
d_old <- NULL
for (fn in list.files(path='.')) {
  if(fn == "SRIS1470_EditedTSDR.csv") 
    next;
  d_read = read_csv(fn, col_types='cccccciccciccccc', na=c("","NA","NULL"))
  stopifnot(colnames(d_read)[7] == 'Age') # make sure integer columns are where we expect
  stopifnot(colnames(d_read)[11] == 'NumberOfPassengers')
  print(fn)
  print(dim(d_read))
  if(nrow(d_read) == 0)
    next;
  d_old <- rbind(d_old, d_read)
}
d_old = filter(d_old, OfficerAgency == 'FLORIDA HIGHWAY PATROL') # filter out a small number of weird stops by the Florida department of agriculture. 

# Remove completely duplicated rows.
duplicate_idxs <- duplicated(d_old)
message(sprintf("Removing duplicate rows in old data: %2.1f%% of rows.\n", 100 * mean(duplicate_idxs)))
d_old <- d_old[!duplicate_idxs, ]

# There are also duplicate rows due to multiple violations per stop. Some pertain
# to different passengers, in which we sometimes cannot uniquely identify the race
# of the driver.
# We group together rows by Officer ID, stop datetime (down to the second) and county. 
# We use these fields because they are available for all stops in both old and new data 
# and the probability of two distinct stops having the same values of these fields is essentially 0. 

# define small helper function for pasting together fields from multiple occupants. 
# removes NAs and bad values, takes unique values, sorts them and pastes them together. 
# this is necessary to avoid NAs in strings, duplicated values, or strings in non-consistent sorted order. 
combine_multiple_occupants = function(x){
  return(paste(sort(unique(na.omit(x[!(x == 'NOT INDICATED' | x == 'NOT APPLICABLE')]))), collapse='|'))}
d_old <- d_old %>%
  group_by(ReportDateTime, County, OfficerIDNo) %>%
  summarize(
    # sometimes different passengers are cited in the same stop
    City = get_unique_value_if_exists(City),
    VehicleTagNo = get_unique_value_if_exists(VehicleTagNo),
    VehicleTagNoState = get_unique_value_if_exists(VehicleTagNoState),
    NumberOfPassengers = get_unique_value_if_exists(NumberOfPassengers),
    OfficerAgency = get_unique_value_if_exists(OfficerAgency),
    OfficerRank = get_unique_value_if_exists(OfficerRank),
    OfficerName = get_unique_value_if_exists(OfficerName),
    Race=get_unique_value_if_exists(Race),
    Ethnicity=get_unique_value_if_exists(Ethnicity),
    Sex=get_unique_value_if_exists(Sex),
    Age=as.integer(get_unique_value_if_exists(Age)),
    Comments=combine_multiple_occupants(Comments)
  ) %>%
  ungroup()

message(sprintf("In old data, fraction of stops with same time, officer id, and county is %2.3f", 
                mean(duplicated(d_old %>% select(ReportDateTime, County, OfficerIDNo)))))


# Read new data
d_new <- read_csv("SRIS1470_EditedTSDR.csv", na=c("","NA","NULL"),
                col_types='cccccciicccccccccccccccccccccccc')
# Fix column name issue
colnames(d_new)[1] <- 'County'

# Similar de-duplication (takes long)
d_new <- d_new %>%
  filter(!is.na(ReportDateTime)) %>%
  group_by(County, ReportDateTime, OfficerIDNo) %>% 
  summarize(
    # These fields should be unique for each stop. 
    Race = get_unique_value_if_exists(Race),
    Ethnicity = get_unique_value_if_exists(ethnicity),
    City = get_unique_value_if_exists(City), 
    NAME = get_unique_value_if_exists(NAME),
    Off_Age_At_Stop = get_unique_value_if_exists(Off_Age_At_Stop), 
    Off_Sex = get_unique_value_if_exists(Off_Sex), 
    Off_Race = get_unique_value_if_exists(Off_Race), 
    OfficerCurrentRank = get_unique_value_if_exists(OfficerCurrentRank), 
    # combine violation reasons
    ReasonForStop = combine_multiple_occupants(ReasonForStop),
    EnforcementAction = combine_multiple_occupants(c(EnforcementAction1, EnforcementAction2, EnforcementAction3)),
    Violation = combine_multiple_occupants(c(Violation1, Violation2, Violation3)), 
    SearchType = combine_multiple_occupants(SearchType), 
    SearchRationale = combine_multiple_occupants(c(SearchRationale1, SearchRationale2, SearchRationale3)),
    ItemsSiezed = combine_multiple_occupants(ItemsSiezed),
    Comments = combine_multiple_occupants(Comments)
  ) %>%
  ungroup()

message(sprintf("In new data, fraction of stops with same time, officer id, and county is %2.3f", 
                mean(duplicated(d_new %>% select(ReportDateTime, County, OfficerIDNo)))))

# Join the new and old data
# The old data goes only until 2015-06, the new data until 2016-03.

d <- full_join(
  d_old %>% mutate(ReportDateTime=gsub('/', '-', ReportDateTime)),
  d_new %>% mutate(ReportDateTime=substr(ReportDateTime, 1, 19)),
  by=c("ReportDateTime", "City", "County", "OfficerIDNo", "Race", "Ethnicity"), 
  suffix = c('_old', '_new')) %>%
  # split date and time
  separate(ReportDateTime, c("date","time"), sep=' ', remove=F)
message(sprintf("After joining, fraction of stops with same time, officer id, and county is %2.3f", 
                mean(duplicated(d %>% select(ReportDateTime, County, OfficerIDNo)))))

# Value dictionaries
race_keys <- c("W", "B", "A", "H", "I", "O", "", "N", "U")
race_vals <- c("White", "Black", "Asian", "Hispanic", "Other", "Other", NA, NA, NA)
search_type_keys <- c("PROBABLE CAUSE","SEARCH INCIDENT TO ARREST","NO SEARCH REQUESTED",
                      "STOP AND FRISK (OFFICER SAFETY) (S.901.151, F.S.)","SEARCH WARRANT",
                      "PLAIN VIEW","NOT INDICATED","NO SEARCH / CONSENT DENIED",
                      "INVENTORY","CONSENT SEARCH CONDUCTED", "")
search_type_vals <- c("Probable Cause","Incident to Arrest", NA, 
                      "Protective Frisk","Warrant",
                      "Probable Cause", NA, NA, 
                      "Inventory", "Consent", NA)

# Rename and extract columns
print(sprintf("[%s] extracting columns", this_state))
d$state                 <- this_state
d$stop_date             <- make_date(d$date)
d$stop_time             <- strftime(strptime(d$time, "%H:%M:%S"), format = '%H:%M')
d$id                    <- make_row_id(d)
d$location_raw          <- d$County
counties_clean          <- normalize_county(d)
d$county_name           <- counties_clean$county_name
d$county_fips           <- counties_clean$fips
d$fine_grained_location <- d$City
d$state_patrol          <- TRUE
d$police_department     <- NA # No granular info available .
d$driver_gender         <- ifelse(d$Sex %in% c("M","F"), d$Sex, NA)
d$driver_age_raw        <- d$Age
d$driver_age            <- get_age(d, type='age')
d$driver_race_raw       <- ifelse(grepl('H', d$Ethnicity), paste(d$Race, 'H'), d$Race) # concatenate race and ethnicity. 
d$driver_race           <- map(d$Race, race_keys, race_vals)  # map race with dictionary approach
d$driver_race[grepl('H', d$Ethnicity)] <- 'Hispanic'  # map drivers to Hispanic if either their race is marked as H or if their ethnicity is H 
d$violation_raw         <- d$Violation # This has some missing  data which appears to be because the old data has no violation codes. 
d$violation             <- normalize_violation_multiple(d, d$violation_raw, clean=TRUE, sep='\\|') 
# We say a search was conducted if search type indicates a search was conducted. This data is missing for some of the old data. 
# Search rationale does not necessarily indicate a search: per the Florida PD's reply, 
#"If it says Search Rationale, then that was the reason they choose to either search or not search"
d$search_type_raw       <- d$SearchType # search data is missing for old data. 
d$search_type           <- multimap(d$id, d$search_type_raw, search_type_keys, search_type_vals, sep='\\|')
d$search_conducted      <- grepl("Probable Cause|Consent|Inventory|Incident to Arrest|Plain View|Stop and Frisk|Warrant|Protective Frisk", d$search_type)
d$search_conducted[is.na(d$search_type_raw)] = NA # This indicates old data, for which we do not know whether a search was conducted. 
d$contraband_found      <- NA # there is data on items seized, but it is not really clear how much this is as a result of a search. 
d$stop_outcome_raw      <- tolower(d$EnforcementAction)
d$stop_outcome          <- ifelse(grepl("felony arrest"     , d$stop_outcome_raw), "Felony Arrest",
                           ifelse(grepl("misdemeanor arrest", d$stop_outcome_raw), "Misdemeanor Arrest",
                           ifelse(grepl("infraction arrest|ucc issued|dver issued" , d$stop_outcome_raw), "Citation", # these are actually citations, per Florida PD's clarification. 
                           ifelse(grepl("warning"            , d$stop_outcome_raw), "Warning", 
                           ifelse(grepl("faulty equipment notice"           , d$stop_outcome_raw), "Faulty Equipment Notice", NA)))))
# we can also fill in stop outcome data for the old data using the Comments field. 
d$stop_outcome[is.na(d$stop_outcome) & grepl('Warning', d$Comments_old)] = 'Warning'
d$stop_outcome[is.na(d$stop_outcome) & grepl('Citation', d$Comments_old)] = 'Citation'

d$is_arrested           <- grepl("felony arrest|misdemeanor arrest", d$stop_outcome_raw)
d$is_arrested[is.na(d$stop_outcome_raw)] = NA # have no arrest data for old stop data. 

# Extra fields
d$officer_id            <- d$OfficerIDNo
d$officer_gender        <- ifelse(d$Off_Sex %in% c("M","F"), d$Off_Sex, NA)
d$Off_Age_At_Stop = as.integer(d$Off_Age_At_Stop)
d$officer_age           <- ifelse(d$Off_Age_At_Stop > 14 & d$Off_Age_At_Stop < 100, d$Off_Age_At_Stop, NA)
d$officer_race          <- map(d$Off_Race, race_keys, race_vals)
d$officer_rank          <- d$OfficerRank
d$out_of_state          <- d$VehicleTagNoState != 'FL'

# Close-up
write_cleaned_state(d, extra_cols=c('officer_id','officer_gender','officer_age','officer_race','officer_rank','out_of_state'))
change_path(NA)
