# Processing code for Arizona

# Set-up
this_state <- 'AZ'
change_path(this_state)

# Read in and combine data
print(sprintf("[%s] reading in the data", this_state))

# 2009-2010
d_old <- rbind(
  read_csv("2009/2009_Tracs_10_sheet1.csv"),
  read_csv("2010/2010_Tracs_10_sheet1.csv")
)

# 2011-2015
d_new <- NULL
for (year in 2011:2015) {
  print(year)
  for (file in list.files(path=paste0(year,'/.'))) {
    dread <- read_csv(paste0(year, '/', file), col_types='ccccccccccccccccccccccccccccccccccccccccccccccccccccccccc')
    d_new <- rbind(d_new, dread)
  }
}

# fix columns (column position actually doesn't matter) 
d_old <- d_old %>%
  select(-BackupBadge2, -CMV, -ImmigrationStatusCheck)
d_old$County = NA
d_old$SearchOfVehicle = NA
d_old$StopDuration = NA
d_old$PreStopIndicators = NA
d_new <- d_new %>%
  select(-DistractedDriving, -SubjectDemeanor)
d_new$DriverItemsSeized = NA
d <- rbind(d_old[, colnames(d_new)], d_new)

# Value dictionaries
race_keys <- c("A","B","H","M","N","W","X")
race_vals <- c("Asian / Pacific Islander","Black","Hispanic","Middle Eastern","Native American","White","Other or Undetermined")
race_vals_clean <- c("Asian","Black","Hispanic","Other","Other","White","Other")
outcome_keys <- c("IO","AR","CI","DV","FI","RO","TC","WA","WR","VC")
outcome_vals <- c("Information Only","Arrest","Citation","Citation","Field Interview","Repair Order","Tribal Citation","Warning","Warrant Arrest","Voided")
pre_stop_keys <- c("VT","BL","PB","DB","OT","NO")
pre_stop_vals <- c("Vehicle Type, Condition or Modification","Driver Body Language","Passenger Behavior","Driving Behavior","Other","None")
search_type_keys <- c("BI","DR","PS","PE","VE")
search_type_vals <- c("Bicyclist","Driver","Passenger","Pedestrian","Vehicle")
stop_duration_keys <- c("A","B","C","D","E","F")
stop_duration_vals <- c("0-10","11-20","21-30","31-45","45-60","60+")

# Rename and extract columns
print(sprintf("[%s] extracting columns", this_state))
d$state                 <- this_state
d$stop_date             <- make_date(d$DateOfStop)
d$stop_time             <- strftime(strptime(d$TimeOfStop, "%H:%M"), format = '%H:%M')
d$id                    <- make_row_id(d)
d$location_raw          <- d$County
counties_clean          <- normalize_county(d)
d$county_name           <- counties_clean$county_name
d$county_fips           <- counties_clean$fips
# County data is bad before 2015-05-20
d$county_name[as.character(d$stop_date) < '2015-05-20'] = NA
d$county_fips[as.character(d$stop_date) < '2015-05-20'] = NA
d$fine_grained_location <- paste(d$Highway, d$Milepost, d$OtherLocation)
d$state_patrol          <- TRUE
d$police_department     <- NA  # not included  There is a PD field (BackupAgency1), but it looks like it does not refer to the agency conducting the stop
d$driver_gender         <- d$Gender
d$driver_age_raw        <- NA  # not included
d$driver_age            <- NA  # not included
d$driver_race_raw       <- map(d$Ethnicity, race_keys, race_vals)
d$driver_race           <- map(d$Ethnicity, race_keys, race_vals_clean)
d$violation_raw         <- normalize_violation_multiple(d, d$CitationArrestViolations, clean=FALSE)
d$violation             <- normalize_violation_multiple(d, d$CitationArrestViolations, clean=TRUE)
d$search_conducted      <- d$SearchPerformed == 'Y' # there are multiple search fields but this is the cleanest definition, and it yields essentially the same results as combining the results from all search fields.  
d$search_type_raw       <- ifelse((d$ConsentSearchAccepted == 'Y') & (d$search_conducted == TRUE), 'Consent', NA) # VehicleSearchAuthority might correspond to this but we cannot map it. 
d$search_type           <- ifelse((d$ConsentSearchAccepted == 'Y') & (d$search_conducted == TRUE), 'Consent', NA)  
d$contraband_found      <- !is.na(d$DrugSeizureType) |
                             (d$DriverItemsSeized != 'N' & !is.na(d$DriverItemsSeized)) |
                             (d$VehicleItemsSeized != 'N' & !is.na(d$VehicleItemsSeized))
d$stop_outcome          <- multimap(d$id, d$OutcomeOfStop, outcome_keys, outcome_vals) # this maps to multiple stop outcomes. We take the most severe, consistent with processing for other states. We do not count voided citations as citations. 
d$stop_outcome          <- ifelse(grepl('Arrest', d$stop_outcome), 'Arrest', 
                           ifelse(grepl('Citation', d$stop_outcome), 'Citation', 
                           ifelse(grepl('Warning', d$stop_outcome), 'Warning', 
                           ifelse(grepl('Repair Order', d$stop_outcome), 'Repair Order', 
                           ifelse(grepl('Field Interview', d$stop_outcome), 'Field Interview', 
                           ifelse(grepl('Information Only', d$stop_outcome), 'Information Only', NA))))))
d$is_arrested           <- grepl('Arrest', d$stop_outcome)

# Extra fields
d$officer_id            <- d$BadgeNumber
d$stop_duration_mins    <- as.numeric(strptime(d$EndOfStop, "%H:%M") - strptime(d$TimeOfStop, "%H:%M")) / 60
d$stop_duration         <- ifelse(!is.na(d$StopDuration), map(d$StopDuration, stop_duration_keys, stop_duration_vals),
                           ifelse(is.na(d$stop_duration_mins) | (d$stop_duration_mins < 0), NA, 
                           ifelse(d$stop_duration_mins < 11, '0-10',
                           ifelse(d$stop_duration_mins < 21, '11-20',
                           ifelse(d$stop_duration_mins < 31, '21-30',
                           ifelse(d$stop_duration_mins < 46, '31-45',
                           ifelse(d$stop_duration_mins < 61, '46-60', '60+')))))))
d$road_number           <- d$Highway
d$milepost              <- d$Milepost
d$consent_search        <- (d$ConsentSearchAccepted == 'Y') & (d$search_conducted)
d$pre_stop_indicators   <- multimap(d$id, d$PreStopIndicators, pre_stop_keys, pre_stop_vals)
d$vehicle_type          <- paste(d$VehicleStyle, d$VehicleYear)
d$ethnicity             <- d$Ethnicity

# map counties with mileposts. 
# first create a dataframe of all unique road-milepost pairs. 
unique_roads_and_mileposts = d %>% 
  select(road_number, milepost) %>% 
  unique() %>% 
  mutate(county_from_milepost = NA)

# now read in our hand-mapped roads and mileposts. 
# this has the min,max milepost for each road in each county. 
our_mapping = read_csv(paste0(code_path, 'resources/dictionaries/az_mileposts.csv'), 
                       col_types = list(min_milepost = col_double(), 
                                        max_milepost = col_double())) %>% 
  mutate(county = paste(county, 'County'))

# now attempt to map the road-milepost pairs which appear in our mapping dataframe. 
unique_roads_and_mileposts = filter(unique_roads_and_mileposts, 
                                    road_number %in% unique(our_mapping$road_number))
for(i in 1:nrow(unique_roads_and_mileposts)){
  road_number_i = unique_roads_and_mileposts$road_number[i]
  milepost_i = as.numeric(unique_roads_and_mileposts$milepost[i])
  county_mapping_i = filter(our_mapping, 
                            road_number == road_number_i, 
                            min_milepost <= milepost_i, 
                            max_milepost > milepost_i)
  if(nrow(county_mapping_i) == 1){
    unique_roads_and_mileposts$county_from_milepost[i] = county_mapping_i$county
  }
}

# join the mapped mileposts to the original dataset.
d = d %>% left_join(unique_roads_and_mileposts, by = c('road_number', 'milepost'))

# assess quality of mapping. 
stops_with_location_info_that_are_mapped = d %>% 
  filter((!is.na(road_number) & !is.na(milepost)) | !is.na(location_raw)) %>%
  summarise(p_mapped = mean((!is.na(county_from_milepost)) | (!is.na(county_name))))
message(sprintf("%2.1f%% of stops with any location information are mapped.", 
        100 * stops_with_location_info_that_are_mapped$p_mapped))

# combine two mapping method: use county name if possible, otherwise use milepost.
d$county_name = ifelse(!is.na(d$county_name), d$county_name, d$county_from_milepost)

# map all counties with names to their FIPS code. 
counties_index = read_csv(paste0(code_path, "/resources/county_index.csv"), col_types='cccccc') %>% 
  filter(state == 'AZ')
d$county_fips = plyr::mapvalues(d$county_name, 
                                counties_index$county_name, 
                                counties_index$fips)

# Close-up
write_cleaned_state(d, extra_cols=c('officer_id', 'stop_duration', 'road_number', 'milepost',
                                    'consent_search', 'vehicle_type', 'ethnicity'))
change_path(NA)
