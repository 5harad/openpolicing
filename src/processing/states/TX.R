# Processing code for Texas

# Set-up
this_state <- 'TX'
change_path(this_state)

# Read in and combine data
print(sprintf("[%s] reading in the data", this_state))

# Read the violation code and county look-up tables
violation_codes <- read_csv('TXDPS_Lookups/LKUP_Violation.csv')
county_names <- read_csv('TXDPS_Lookups/LKUP_County.csv') %>% select(HA_COUNTY=LK_Code, county_name=LK_Description)

d <- NULL
for (folder in list.files()){
  # For each year, must join stops to warnings + citations to figure out the stop reaons. 
  if(grepl("Statewide_Traffic|Lookups|pdf|py", folder)){ next; } # these folders do not contain stop data.
  # Two different data formats: prior to 2009, after 2009. 
  is_old <- grepl("2006|2007|2008", folder)
  files_in_folder <- list.files(folder)
  stop_files <- files_in_folder[grepl('Stops', files_in_folder)] # Recent years have multiple stop files.
  cat(sprintf('%s has %i files, %i stop files\n', folder, length(files_in_folder), length(stop_files)))

  # Read the warnings file, join to look-up table and group by traffic stop
  warnings <- files_in_folder[grepl('Warning', files_in_folder)] %>%
    paste0(folder, '/', .) %>%
    read_csv() %>%
    mutate(AW_VIOLATION_CODE=str_sub(paste0('00000', as.character(AW_VIOLATION_CODE)), -5, -1)) %>%
    left_join(violation_codes %>% select(AW_VIOLATION_CODE=LK_Code, warning_reason=LK_description), by="AW_VIOLATION_CODE") %>%
    group_by(AW_ARREST_KEY) %>%
    summarize(warning_reasons=paste(warning_reason, collapse=' AND ')) %>%
    ungroup()

  # Read the citations file, join to look-up table and group by traffic stop
  citations <- files_in_folder[grepl('Citation', files_in_folder)] %>%
    paste0(folder, '/', .) %>%
    read_csv() %>%
    mutate(AD_VIOLATION_CODE=str_sub(paste0('00000', as.character(AD_VIOLATION_CODE)), -5, -1)) %>%
    left_join(violation_codes %>% select(AD_VIOLATION_CODE=LK_Code, citation_reason=LK_description), by="AD_VIOLATION_CODE") %>%
    group_by(AD_ARREST_KEY) %>%
    summarize(citation_reasons=paste(citation_reason, collapse=' AND ')) %>% 
    ungroup()

  for(file in stop_files){
    small_d <- read_csv(paste0(folder, '/', file))
    if(is_old) {
      small_d$search_conducted <- small_d$HA_SEARCHED  == -1
      small_d$contraband_found <- small_d$HA_CONTRABAN == -1
    } else {
      small_d$search_conducted <- small_d$HA_SEARCHED  == 1
      small_d$contraband_found <- small_d$HA_CONTRABAN == 1
    }
    small_d$search_type <- NA
    small_d$stop_outcome = NA
    if(is_old) {
      small_d$search_type[small_d$HA_SEARCH_PC      == -1] <- 'Probable Cause'
      small_d$search_type[small_d$HA_SEARCH_CONCENT == -1] <- 'Consent'
      small_d$search_type[small_d$HA_INCIDTO_ARREST == -1] <- 'Incident to Arrest'
      small_d$search_type[small_d$HA_VEHICLE_INVENT == -1] <- 'Inventory'
    } else {
      small_d$search_type[small_d$HA_SEARCH_PC      == 1] <- 'Probable Cause'
      small_d$search_type[small_d$HA_SEARCH_CONCENT == 1] <- 'Consent'
      small_d$search_type[small_d$HA_INCIDTO_ARREST == 1] <- 'Incident to Arrest'
      small_d$search_type[small_d$HA_VEHICLE_INVENT == 1] <- 'Inventory'
    }
    # Make sure that joins keep the same length
    original_length <- nrow(small_d)
    # Join the warnings/citations back in
    small_d <- left_join(small_d, rename(warnings , HA_ARREST_KEY=AW_ARREST_KEY), by="HA_ARREST_KEY")
    small_d <- left_join(small_d, rename(citations, HA_ARREST_KEY=AD_ARREST_KEY), by="HA_ARREST_KEY")
    stopifnot((nrow(small_d) == original_length) & (original_length == length(unique(small_d$HA_ARREST_KEY))))
    # Map counties
    small_d <- left_join(small_d, county_names, by = "HA_COUNTY")
    stopifnot((nrow(small_d) == original_length))
    # Add columns back in if they don't exist
    if(!'HA_N_FIRST_DRVR' %in% colnames(small_d)) small_d$HA_N_FIRST_DRVR <- NA
    if(!'HA_N_LAST_DRVR' %in% colnames(small_d)) small_d$HA_N_LAST_DRVR <- NA
    if(!'HA_N_TROOPER' %in% colnames(small_d)) small_d$HA_N_TROOPER <- NA
    
    small_d <- select(small_d, 
                      HA_ARREST_DATE, 
                      HA_RACE_SEX, 
                      search_conducted,
                      contraband_found,
                      warning_reasons,
                      citation_reasons,
                      HA_LATITUDE,
                      HA_LONGITUDE,
                      HA_MILEPOST,
                      HA_OFFICER_ID,
                      HA_TICKET_TYPE,
                      county_name, 
                      search_type,
                      HA_N_FIRST_DRVR,
                      HA_N_LAST_DRVR,
                      HA_N_TROOPER
                  )
    d <- rbind(d, small_d)
  }
}

# Value dictionaries
race_keys <- c('A', 'H', 'W', 'B', 'I', 'U', 'O', 'M')
race_vals <- c('Asian', 'Hispanic', 'White', 'Black', 'Native American', 'Unknown', 'Other', 'Middle Eastern')
race_vals_clean <- c('Asian', 'Hispanic', 'White', 'Black', 'Other', NA, 'Other', 'Other')

# Rename and extract columns
print(sprintf("[%s] extracting columns", this_state))

d$state                 <- this_state
d$stop_date             <- make_date(substr(d$HA_ARREST_DATE, 1, 8), format = "%m/%d/%y")
d$stop_time             <- strftime(strptime(substr(d$HA_ARREST_DATE, 9, 14), "%H:%M"), format = '%H:%M')
d$id                    <- make_row_id(d)
d$location_raw          <- d$county_name
counties_clean          <- normalize_county(d)
d$county_name           <- counties_clean$county_name
d$county_fips           <- counties_clean$fips
d$fine_grained_location <- d$HA_MILEPOST
d$state_patrol          <- TRUE
d$police_department     <- NA  # not included
d$driver_gender         <- substr(d$HA_RACE_SEX, 2, 2)
d$driver_gender[!(d$driver_gender %in% c('M', 'F'))] = NA
d$driver_age_raw        <- NA  # not included
d$driver_age            <- NA  # not included
d$driver_race_raw       <- map(substr(d$HA_RACE_SEX, 1, 1), race_keys, race_vals) 
d$driver_race           <- map(substr(d$HA_RACE_SEX, 1, 1), race_keys, race_vals_clean)
# combine warnings and citations
d$violation_raw         <- paste(d$warning_reasons, d$citation_reasons, sep=' AND ')
d$violation_raw         <- trim(gsub("AND NA|NA AND|,", "", d$violation_raw))
d$violation             <- normalize_violation_multiple(d, d$violation_raw, sep=' AND ', clean=TRUE)
d$search_conducted      <- d$search_conducted 
d$search_conducted[!is.na(d$search_type)] = TRUE # tiny number of stops where the columns are inconsistent; rectify. 
d$search_type_raw       <- d$search_type
d$search_type           <- d$search_type_raw

d$contraband_found      <- d$contraband_found
# set contraband = FALSE in cases where search_conducted=FALSE (these seem to usually be stops where contraband is in plain view, ie.open container, drug paraphenelia, etc.)
# since we define contraband to be contraband found as result of a search. 
d$contraband_found[d$search_conducted == FALSE] = FALSE
d$stop_outcome          <- map(d$HA_TICKET_TYPE, c('THP6', 'TLE6', 'HP3'), c('Citation', 'Citation', 'Warning')) # HA_TICKET_TYPE has less missing data than HA_REASON_CITA / HA_REASON_WARN columns. 
d$is_arrested           <- NA  # not included

# Extra columns
d$officer_id            <- d$HA_OFFICER_ID
d$driver_first_name      <- str_to_title(d$HA_N_FIRST_DRVR) # need this to filter duplicates, but don't write it to output. 
d$driver_last_name      <- str_to_title(d$HA_N_LAST_DRVR)
d$lat                   <- as.numeric(d$HA_LATITUDE) / (1e6)
d$lon                   <- as.numeric(d$HA_LONGITUDE) / (1e6)

# Normalizes name
normalize_name <- Vectorize(function(x) {
  # Lower case and remove extra white space
  x <- tolower(trim(x))
  # Remove punctuation
  x <- gsub(',|\\.', '', x)
  # Remove title 
  x <- gsub(' jr$| sr$| i$| ii$| iii$| iv$|  v$| vi$', '', x)
  x <- gsub('\\-', ' ', x)
  pieces <- strsplit(x, ' ')
  x <- sapply(pieces, function(x) x[length(x)])    
  return(x)
})

# Do race relabeling
d <- d %>%
  mutate(
    # Set aside original race field
    driver_race_original = driver_race,
    # Normalize the name
    last_name = as.character(normalize_name(driver_last_name))
  ) %>%
  # Join with Census data
  left_join(
    # Read the surname dataset
    # Source: http://www.census.gov/topics/population/genealogy/data/2000_surnames.html
    read_csv(paste0(code_path, '/resources/surnames.zip'), na=c("","NA","(S)")) %>%
      select(name, pcthispanic) %>%
      mutate(
        last_name=tolower(name),
        pH=coalesce(as.numeric(pcthispanic)/100, 0)
      ) %>%
      filter(nchar(last_name) > 0)
    , by='last_name') %>%
  mutate(
    # Re-assign White and Other drivers to Hispanic if P(Hispanic|Name) > 0.75
    driver_race = ifelse((driver_race_original == "White" | is.na(driver_race_original)) & !is.na(pH) & pH > 0.75, "Hispanic", driver_race)
  )

# Filter out duplicate rows 
time_duplicates <- d %>%
  select(stop_date, stop_time, county_fips, HA_RACE_SEX, HA_MILEPOST, HA_OFFICER_ID, driver_first_name, driver_last_name) %>%
  duplicated()
d <- filter(d, time_duplicates == FALSE)
# Redo id
d$id <- make_row_id(d)

# Close-up
write_cleaned_state(d, extra_cols=c('lat','lon','officer_id','driver_race_original'))
change_path(NA)
