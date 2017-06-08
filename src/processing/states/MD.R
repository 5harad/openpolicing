# Processing code for Maryland

# Set-up
this_state <- 'MD'
change_path(this_state)

# Read in and combine data
print(sprintf("[%s] reading in the data", this_state))

na_list = c("NA","","#NULL!",'N/A','n/a')

# Read and merge newer data
d13 <- read_csv("2013.csv", na=na_list, col_types = cols(`Time of Stop` = col_character()))[, c(1:6, 9:32)]
d14 <- read_csv("2014.csv", na=na_list, col_names = colnames(d13), skip=1, col_types = cols(`Time of Stop` = col_character()))
d_new <- rbind(d13 %>% mutate(year=2013), d14 %>% mutate(year=2014)) %>% select(1:8, 12:22, 31, -`Arrest Made`) %>% rename(Registration=`State of Residence`)

# Read and merge old data
# 2007
d07  <- read_csv("2007.csv", na=na_list, col_types = cols(`Time of Stop` = col_character())) %>% mutate(year=2007) %>%
  mutate(OutcomeDetail =  ifelse(grepl('arr', tolower(Outcome)), 'arr', ifelse(grepl('cita', tolower(Outcome)), 'cit', ifelse(grepl('warn', tolower(Outcome)), 'warn', NA))),
    Outcome = ifelse(Arrest=='Y', 'arr', ifelse(Citation=='Y', 'cit', ifelse(Warning=='Y', 'warn', ifelse(SERO=='Y', 'sero', NA)))),
    Outcome = coalesce(Outcome, OutcomeDetail),
    SNarc = ifelse(tolower(SNarc) %in% c('b','y','yes','1','t','contraband','other'), 'Y', 'N'),
    SProp = ifelse(tolower(SProp) %in% c('b','y','yes','1','t'), 'Y', 'N'),
    Disposition = ifelse(SNarc=='Y', ifelse(SProp=='Y', 'both', 'contra'), ifelse(SProp=='Y', 'prop', NA))
  ) %>%
  select(-`StopE`, -Arrest, -Citation, -Warning, -SERO, -SNarc, -SProp, -VReg, -OutcomeDetail)
colnames(d07) <- c("Agency","Stop Reason","CitationCode","Gender","Race","Registration","Search","Consent","Search Reason","Outcome","Arrest Reason","year","Disposition")

# 2009-2012
d09  <- read_csv("2009.csv", na=na_list, col_types = cols(`Time of Stop` = col_character())) %>% mutate(year=2009)
d11  <- read_csv("2011.csv", na=na_list, col_types = cols(`Time of Stop` = col_character())) %>% mutate(year=2011)
d12  <- read_csv("2012.csv", na=na_list, col_types = cols(`Time of Stop` = col_character())) %>% mutate(year=2012) %>% select(1:10,12)
colnames(d09) <- colnames(d12);
colnames(d11) <- colnames(d12);
d_old <- rbind(d09, d11, d12) %>% mutate(CitationCode=NA, Consent=NA)
d_old <- rbind(d_old, d07[, colnames(d_old)]) %>%
  # fill out empty names
  mutate(Location=NA, `Date of Stop`=NA, `Time of Stop`=NA, DOB=NA, `Search Conducted`=NA, `Duration of Stop      (in minutes)`=NA, `Duration of Search (in minutes)`=NA, `Stop Reason (Abbreviated)`=`Stop Reason`)

# merge it all
d <- rbind(d_new, d_old[, colnames(d_new)])

# Value dictionaries
# inspect with data.frame(keys=race_keys, vals=race_vals, vals_clean=race_vals_clean)
race_keys <- c("w", "b", "a", "h", "o", "u", "asian", "black", "hispanic", "native american", "other",
              "unknown", "white", 'am. indian','american indian/alaskan', 'asian pacific','asian/pac. is.',
              'black/african american','blk', 'hispa','hispanic/latino', 'indian','indian/alaskin', 'other/unknown')
race_vals <- c("Causasian", "African American", "Asian", "Hispanic", "Other", "Unknown", "Asian", "Black",
               "Hispanic", "Native American", "Other", "Unknown", "White", 'Native American','Native American',
               'Asian/Pacific Islander','Asian/Pacific Islander', 'Black','Black', 'Hispanic','Hispanic', 'Native American','Native American', 'Other')
race_vals_clean <- c("White", "Black", "Asian", "Hispanic", "Other", NA, "Asian", "Black", "Hispanic", "Other",
                     "Other", NA, "White", 'Other','Other', 'Asian','Asian', 'Black','Black', 'Hispanic',
                     'Hispanic', 'Other','Other', 'Other')
outcome_keys <- c("arr","cit","sero","SERO","warn")
outcome_vals <- c("Arrest","Citation","Repair Order","Repair Order","Warning")
arrest_reason_keys <- c('dui', 'DUI Arrest','other','search','stop')
arrest_reason_vals <- c('DUI', 'DUI','Other','Search','Stop')

# Rename and extract columns
print(sprintf("[%s] extracting columns", this_state))
d$state                 <- this_state
d$stop_date             <- make_date(if_else(!is.na(d$`Date of Stop`), as.Date(round(as.numeric(d$`Date of Stop`)), origin='1900-01-01'), as.Date(paste0(d$year, '-01-01'))))
d$stop_time             <- strftime(strptime(d$`Time of Stop`, "%H:%M"), format = '%H:%M')
d$id                    <- make_row_id(d)
d$location_raw            <- d$Agency
counties_clean          <- normalize_county(d)
d$county_name           <- counties_clean$county_name
d$county_fips           <- counties_clean$fips
d$fine_grained_location <- d$Location
d$state_patrol          <- d$Agency %in% c('Maryland State Police', 'MSP', 'MDTA', 'MDTA Police')
d$police_department     <- d$Agency
d$driver_gender         <- map(tolower(d$Gender), c("f", "m", "female", "male"), c("F","M","F","M"))
d$driver_gender         <- ifelse(d$driver_gender %in% c("F","M"), d$driver_gender, NA)
d$driver_age_raw        <- as.Date(round(as.numeric(d$DOB)), origin='1900-01-01')
d$driver_age            <- get_age(d, type='birthdate')
d$driver_race_raw       <- map(tolower(d$Race), race_keys, race_vals)
d$driver_race           <- map(tolower(d$Race), race_keys, race_vals_clean)
d$driver_race           <- ifelse(d$driver_race %in% c("Black","White","Hispanic","Asian","Other"), d$driver_race, NA)
d$violation_raw         <- normalize_violation(d, d$`Stop Reason`)
d$violation             <- normalize_violation(d, d$`Stop Reason`, clean=TRUE)
no_search_occurred_vals = c('N', 'n', 'None', '*None', NA) # classify NA as no search because vast majority of values. 
d$search_conducted      <- (!(d$Search %in% no_search_occurred_vals)) | (!(d$`Search Reason` %in% no_search_occurred_vals)) # if either search reason or search indicates a search, define as search. 
d$search_type_raw       <- tolower(d$`Search Reason`)
d$search_type           <- ifelse(grepl('indicent|arr', d$search_type_raw), "Incident to Arrest", 
                           ifelse(grepl('cons', d$search_type_raw), "Consent",
                           ifelse(grepl('pro', d$search_type_raw), "Probable Cause",
                           ifelse(grepl('exigent', d$search_type_raw), "Exigent Circumstances",
                           ifelse(grepl('9', d$search_type_raw), "K9 Search", NA)))))
d$contraband_found      <- grepl('both|contra|prop', tolower(d$Disposition))
d$contraband_found[!d$search_conducted] = FALSE
d$stop_outcome          <- map(d$Outcome, outcome_keys, outcome_vals)
d$is_arrested           <- (d$stop_outcome == 'Arrest') & (!is.na(d$stop_outcome))

  # Extra fields
d$out_of_state         <- d$Registration == 'o' | (nchar(d$Registration) == 2 & d$Registration != 'MD')
d$arrest_reason        <- map(d$`Arrest Reason`, arrest_reason_keys, arrest_reason_vals)
d$stop_duration        <- as.numeric(d$`Duration of Stop      (in minutes)`)
d$search_duration      <- as.numeric(d$`Duration of Search (in minutes)`)

# Close-up
write_cleaned_state(d, extra_cols=c('out_of_state','arrest_reason','stop_duration','search_duration'))
change_path(NA)
