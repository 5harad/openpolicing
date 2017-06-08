# Processing code for Nevada

# Set-up
this_state <- 'NV'
change_path(this_state)

# Read in and combine data
print(sprintf("[%s] reading in the data", this_state))

d1 <- read_csv("Copy_of_CitationWOArrestFirstHalf_sheet1.csv")
d2 <- read_csv("Copy_of_CitationsWOArrestSecondHalf_sheet1.csv")
# first part different than second
d3 <- read_csv("Copy_of_CitationsWithArrest_Part3_sheet1.csv") %>%
  mutate(Result='ARREST') %>% select(-Arrest)
colnames(d3)[5:6] <- c("Offense Description", "Code")
d3 <- d3[, colnames(d1)]
d = rbind(d1, d2, d3)

# Value dictionaries

# inspect with : data.frame(from=race_keys, to=race_vals)
race_keys <- c("W", "B", "A", "I", "M", "U")
race_vals <- c("White", "Black", "Asian", "Other", "Other", NA)

# Rename and extract columns
print(sprintf("[%s] extracting columns", this_state))

d$state                 <- this_state
d$stop_date             <- make_date(d$Date, min_ds=as.Date("2012-02-01"))
d$stop_time             <- NA  # not included
d$id                    <- make_row_id(d)
d$location_raw            <- NA  # not included
d$county_name           <- NA  # not included
d$county_fips           <- NA  # not included
d$fine_grained_location <- NA  # not included
d$state_patrol          <- TRUE
d$police_department     <- NA  # not included
d$driver_gender         <- NA  # not included
d$driver_age_raw        <- d$Age
d$driver_age            <- get_age(d)
d$driver_race_raw       <- d$Race
d$driver_race           <- map(d$Race, race_keys, race_vals)  # map race with dictionary
d$violation_raw         <- normalize_violation(d, d$Code)
d$violation             <- normalize_violation(d, d$Code, clean=TRUE)
d$search_conducted      <- NA  # not included
d$search_type_raw       <- NA  # not included
d$search_type           <- NA  # not included
d$contraband_found      <- NA  # not included
d$stop_outcome          <- str_to_title(d$Result)
d$is_arrested           <- d$Result == 'ARREST'

# Extra fields
d$drugs_related_stop    <- grepl("Drugs|Marijuana|Controlled Substance", d$`Offense Description`)

# Close-up
write_cleaned_state(d, extra_cols=c('drugs_related_stop'))
change_path(NA)
