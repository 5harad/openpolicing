# Processing code for Iowa

# Set-up
this_state <- 'IA'
change_path(this_state)

# Read in and combine data

# Citations
dco <- read_csv("ecco_data.csv", col_types = cols(`\"VIOLATIONTIME\"` = col_character()))
dco$key = dco$`\"INDIVKEY\"`
dco$date = dco$`\"VIOLATIONDATE\"`
dco$time = dco$`\"VIOLATIONTIME\"`
dco$fine_grained_location = dco$`\"OFFENSELOCATN\"`
dco$violation_code = dco$`\"SECTIONVIOLATED\"`
dco$violation_description = dco$`\"VIOLDESCRIPTION\"`
dco$ethnicity = dco$`\"LOCKETHNICITY\"`
dco$officer_id = dco$`\"BADGENUMBER\"`
dco$county = dco$`\"LOCKCOUNTY\"`
dco$gender = dco$`\"LOCKGENDER\"`
dco$license_plate_state = dco$`\"LOCKVEHICLEPLATESTATE\"`
dco$vehicle_model = dco$`\"LOCKVEHICLEMODEL\"`
dco$vehicle_year = dco$`\"LOCKVEHICLEYEAR\"`
dco$police_department = NA
dco$outcome = 'Citation'

# Warnings
dwc <- read_csv("ewc_data.csv", col_types = cols(`\"VIOLATIONTIME\"` = col_character()))
dwc$key = dwc$`\"INDIVKEY\"`
dwc$date = dwc$`\"VIOLATIONDATE\"`
dwc$time = dwc$`\"VIOLATIONTIME\"`
dwc$police_department = dwc$`\"DEPARTMENTNAME\"`
dwc$fine_grained_location = dwc$`\"OFFENSELOCATN\"`
dwc$violation_code = dwc$`\"SECTIONVIOLATED\"`
dwc$violation_description = dwc$`\"VIOLDESCRIPTION\"`
dwc$ethnicity = NA
dwc$officer_id = NA
dwc$county = NA
dwc$gender = NA
dwc$license_plate_state = NA
dwc$vehicle_model = NA
dwc$vehicle_year = NA
dwc$outcome = 'Warning'

d <- rbind(
    dwc[, colnames(dwc)[26:40]],
    dco[, colnames(dwc)[26:40]]
  ) %>%
  # records with NA for key have no other fields
  filter(!is.na(key)) %>%
  group_by(key, date) %>%
    summarize(
      time                  = min(time, na.rm=T),
      police_department     = paste(na.omit(unique(police_department    )), collapse=','),
      fine_grained_location = paste(na.omit(unique(fine_grained_location)), collapse=','),
      violation_description = paste(na.omit(unique(violation_description)), collapse=','),
      violation_code        = paste(na.omit(unique(violation_code       )), collapse=','),
      ethnicity             = paste(na.omit(unique(ethnicity            )), collapse=','),
      officer_id            = paste(na.omit(unique(officer_id           )), collapse=','),
      county                = paste(na.omit(unique(county               )), collapse=','),
      gender                = paste(na.omit(unique(gender               )), collapse=','),
      license_plate_state   = paste(na.omit(unique(license_plate_state  )), collapse=','),
      vehicle_model         = paste(na.omit(unique(vehicle_model        )), collapse=','),
      vehicle_year          = paste(na.omit(unique(vehicle_year         )), collapse=','),
      outcome               = paste(na.omit(unique(outcome              )), collapse=',')
    ) %>% 
    ungroup()

# Rename and extract columns
print(sprintf("[%s] extracting columns", this_state))
d$state                 <- this_state
d$stop_date             <- make_date(d$date, format='%m/%d/%Y')
d$stop_time             <- strftime(strptime(d$time, "%H:%M"), format = '%H:%M')
d$id                    <- make_row_id(d)
d$location_raw          <- d$county  # only the citations have county (and only some)
counties_clean          <- normalize_county(d)
d$county_name           <- counties_clean$county_name
d$county_fips           <- counties_clean$fips
d$fine_grained_location <- d$fine_grained_location
d$state_patrol          <- TRUE
d$police_department     <- str_to_title(d$police_department)  # these are Iowa state patrol distrits
d$driver_gender         <- ifelse(d$gender %in% c("M","F"), d$gender, NA)
d$driver_age_raw        <- NA  # not included
d$driver_age            <- NA  # not included
d$driver_race_raw       <- map(d$ethnicity, c("N","H","U"), c("Non-Hispanic","Hispanic","Unknown"))
d$driver_race           <- map(d$ethnicity, c("N","H","U"), c(NA,"Hispanic",NA))
d$violation_raw         <- normalize_violation_multiple(d, d$violation_code)
d$violation             <- normalize_violation_multiple(d, d$violation_code, clean=T)
d$search_conducted      <- NA  # not included
d$search_type_raw       <- NA  # not included
d$search_type           <- NA  # not included
d$contraband_found      <- NA  # not included
d$stop_outcome          <- ifelse(grepl('Citation', d$outcome), 'Citation',
                           ifelse(grepl('Warning' , d$outcome), 'Warning' , NA)) 
d$is_arrested           <- NA  # not included

# Extra fields
d$officer_id            <- d$officer_id
d$out_of_state          <- d$license_plate_state != 'IA'

# Close-up
write_cleaned_state(d, extra_cols=c('officer_id', 'out_of_state'))
change_path(NA)
