# Processing code for Colorado

# Set-up
this_state <- 'CO'
change_path(this_state)

# Read in and combine data
print(sprintf("[%s] reading in the data", this_state))
d <- read_csv("data.csv", na = c("", "NA","NULL", "N/A"), col_types = cols(Search = col_double(), 
                                                                           IncidentDate = col_character(), 
                                                                           IncidentTime = col_character())) 
# Some column names are duplicated
colnames(d)[1  ] <- "id.1"
colnames(d)[78 ] <- "id.2"
colnames(d)[79 ] <- "traffic_stop_id.1"
colnames(d)[86 ] <- "id.3"
colnames(d)[87 ] <- "traffic_stop_id.2"
colnames(d)[104] <- "id.4"
colnames(d)[107] <- "driver_last_name"
colnames(d)[108] <- "driver_first_name"
colnames(d)[105] <- "traffic_stop_id.3"
colnames(d)[112] <- "driver_gender"
colnames(d)[138] <- "officer_id"
colnames(d)[139] <- "traffic_stop_id.4"
colnames(d)[142] <- "officer_last_name"
colnames(d)[143] <- "officer_first_name"
colnames(d)[146] <- "officer_gender"

#each row in the dataset is not a single stop, but a single citation, so we have to group_by to remove duplicates. 
#confirmed that this yields consistent results for search_conducted + contraband_found (>99.9% all stops in group have same value).
d = group_by(d, 
             officer_id,
             officer_first_name, 
             officer_last_name, 
             driver_first_name, 
             driver_last_name, 
             IncidentDate, 
             IncidentTime, 
             DOB, 
             LocationCounty,
             LocationMilePost) %>% 
  summarise(StatuteDesc = paste(StatuteDesc, collapse = ';'),
            SearchBase = first(SearchBase),
            TroopID = first(TroopID), 
            driver_gender = first(driver_gender), 
            Ethnicity = first(Ethnicity), 
            officer_gender = first(officer_gender), 
            Make = first(Make), 
            Model = first(Model), 
            Year = first(Year), 
            PlateState = first(PlateState), 
            Arrest = ifelse(length(unique(Arrest)) == 1, first(Arrest), NA), 
            Citation = ifelse(length(unique(Citation)) == 1, first(Citation), NA),
            Search = ifelse(length(unique(Search)) == 1, first(Search), NA),
            SearchContraband = ifelse(length(unique(SearchContraband)) == 1, first(SearchContraband), NA),
            WrittenWarning = ifelse(length(unique(WrittenWarning)) == 1, first(WrittenWarning), NA),
            OralWarning = ifelse(length(unique(OralWarning)) == 1, first(OralWarning), NA)) %>%
  ungroup()

d$StatuteDesc[d$StatuteDesc == 'NA'] = NA

# Value dictionaries
dept_keys = read_csv('CSP Troops.csv')$Troop
dept_vals = read_csv('CSP Troops.csv')$`Office Location`
race_keys <- c("A","AI","AP","B","H","I","U","W","Z")
race_vals <- c("Asian","Other","Asian","Black","Hispanic",NA,NA,"White",NA)

# Rename and extract columns
print(sprintf("[%s] extracting columns", this_state))
d$state                 <- this_state
d$stop_date             <- make_date(d$IncidentDate)
d$stop_time             <- strftime(strptime(substr(d$IncidentTime, 1, 5), "%H:%M"), format = '%H:%M')
d$id                    <- make_row_id(d)
d$location_raw          <- as.character(d$LocationCounty)
counties_clean          <- normalize_county(d)
d$county_name           <- counties_clean$county_name
d$county_fips           <- counties_clean$fips
d$fine_grained_location <- d$LocationMilePost
d$state_patrol          <- TRUE
d$police_department     <- map(d$TroopID, dept_keys, dept_vals)
d$driver_gender         <- map(d$driver_gender, c("Male","Female","Unknown"), c("M","F",NA))
d$driver_age_raw        <- d$DOB
d$driver_age            <- get_age(d, type='birthdate')
d$driver_race_raw       <- d$Ethnicity
d$driver_race           <- map(d$driver_race_raw, race_keys, race_vals)  # map race with dictionary
d$violation_raw         <- normalize_violation_multiple(d, as.character(d$StatuteDesc), sep = ';')
d$violation             <- normalize_violation_multiple(d, as.character(d$StatuteDesc), clean=TRUE, sep = ';')
d$search_conducted      <- (!is.na(d$SearchBase)) | ((d$Search == 1) & (!is.na(d$Search)))
d$search_type_raw       <- d$SearchBase
d$search_type           <- d$SearchBase
d$contraband_found      <- (d$SearchContraband == 1) & (d$search_conducted)
d$stop_outcome          <- ifelse(d$Arrest == 1, "Arrest",
                           ifelse(d$Citation == 1, "Citation",
                           ifelse(d$WrittenWarning == 1, "Written Warning",
                           ifelse(d$OralWarning == 1, "Verbal Warning", NA))))
d$stop_outcome[year(d$stop_date) > 2013] = NA # stop_outcome data becomes unreliable post-2013. 

d$is_arrested           <- d$Arrest == 1
d$is_arrested[is.na(d$is_arrested)] <- FALSE
d$is_arrested[year(d$stop_date) > 2013] = NA # arrest data becomes unreliable post-2013. 

# Extra fields
d$officer_id             <- d$officer_id
d$officer_gender         <- d$officer_gender
d$vehicle_type           <- paste(d$Make, d$Model, d$Year)
d$out_of_state           <- d$PlateState != 'CO'

# Close-up
write_cleaned_state(d, extra_cols=c('officer_id','officer_gender','vehicle_type','out_of_state'))
change_path(NA)
