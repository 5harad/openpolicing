# Processing code for Wyoming

# Set-up
this_state <- 'WY'
change_path(this_state)

# Read in and combine data
print(sprintf("[%s] reading in the data", this_state))
d11.1 = read_csv("Jan-June_2011_sheet1.csv") 
d11.2 = read_csv("Jul-Dec2011_sheet1.csv") 
d12.1 = read_csv("Jan-June2012_sheet1.csv") 
d12.2 = read_csv("Jul-Dec2012_sheet1.csv") 
colnames(d12.1)[1] <- 'trci_id'
colnames(d12.2)[1] <- 'trci_id'

# Remove empty columns
actual_cols = colnames(d11.1)[!is.na(colnames(d11.1))]

# Put it all together
d11.1 = d11.1[, actual_cols]
d11.2 = d11.2[, actual_cols]
d12.1 = d12.1[, actual_cols]
d12.2 = d12.2[, actual_cols] %>%
  # unfortunately date col is formatted differently
  mutate(tc_date = gsub('-', '/', as.character(tc_date)))

d = rbind(d11.1, d11.2, d12.1, d12.2) %>%
  # add extra statute colum
  separate(statute, into=c("statute","statute2"), sep='[\\( ]')

#remove duplicates. Each row appears to correspond to a citation, not a stop, and this produces about 10% extra rows;
#so we group by officer_id, driver features, stop location, and time and date. 
d = group_by(d, 
             tc_time, 
             tc_date, 
             race, 
             sex, 
             age, 
             offcr_id, 
             street, 
             city, 
             emdivision) %>% 
  summarise(statute = paste(statute, collapse = ';'), 
            charge = paste(charge, collapse = ';')) %>% 
  ungroup() 

# Value dictionaries
# Race - inspect with : data.frame(from=race_keys, to=race_vals)
race_keys = c("A", "B", "H", "I", "U", "W", "")
race_vals = c("Asian", "Black", "Hispanic", "Other", NA, "White", NA)

# Rename and extract columns
print(sprintf("[%s] extracting columns", this_state))

d$state                 <- this_state
d$stop_date             <- make_date(d$tc_date, "%Y/%m/%d")
d$stop_time             <- paste(substr(d$tc_time, 1, 2), substr(d$tc_time, 3, 5), sep=':')  # convert time to 24H
d$id                    <- make_row_id(d)
d$location_raw            <- d$city
counties_clean          <- normalize_county(d)
d$county_name           <- counties_clean$county_name
d$county_fips           <- counties_clean$fips
d$fine_grained_location <- d$street
d$state_patrol          <- TRUE
d$police_department     <- d$emdivision  # "the division where the trooper is located in the state"
d$driver_gender         <- ifelse(d$sex %in% c('U', ''), NA, d$sex)
d$driver_age_raw        <- d$age
d$driver_age            <- get_age(d)
d$driver_race_raw       <- d$race
d$driver_race           <- map(d$race, race_keys, race_vals)  # extract race with dictionary approach
d$violation_raw         <- d$charge
d$violation             <- normalize_violation_multiple(d, d$charge, clean=TRUE, sep = ';')
d$search_conducted      <- NA  # not included
d$search_type_raw       <- NA  # not included
d$search_type           <- NA  # not included
d$contraband_found      <- NA  # not included
d$stop_outcome          <- NA  # not included
d$is_arrested           <- NA  # not included

# Extra columns
d$officer_id            <- d$offcr_id
d$drugs_related_stop    <- grepl("35-7-1031|31-18-701", d$statute)

# Close-up
write_cleaned_state(d, extra_cols=c('officer_id','drugs_related_stop'))
change_path(NA)
