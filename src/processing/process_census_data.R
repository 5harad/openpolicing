# This script produces the data set that contains the population estimates we
# use for our population benchmark. They are based on the "Intercensal Estimates
# of the Resident Population by Five-Year Age Groups, Sex, Race, and Hispanic
# Origin for Counties" data set produced by the U.S. Census Bureau. 
# This script then processes the raw census data stored at raw_intercensal_data_path
# and produces a file in the format we need which is saved at processed_intercensal_data_path. 

# Raw data source     : https://www2.census.gov/programs-surveys/popest/datasets/2010-2015/counties/asrh/cc-est2015-alldata.csv
# Raw data description: https://www2.census.gov/programs-surveys/popest/datasets/2010-2015/counties/asrh/cc-est2015-alldata.pdf

message(sprintf("Processing census data stored at %s", raw_intercensal_data_path))
if(file.exists(processed_intercensal_data_path)){
  message(sprintf("Processed data file %s already exists; not overwriting", processed_intercensal_data_path))
}
d <- read_csv(raw_intercensal_data_path) %>%
  # Remove summary categories
  filter(YEAR > 2, AGEGRP > 0) %>%
  # extract and rename relatve columns
  select(
    state_fips  = STATE,
    county_fips = COUNTY,
    state       = STNAME,
    county      = CTYNAME,
    year        = YEAR,
    age         = AGEGRP,
    # Population Counts
    total_M     = TOT_MALE,
    total_F     = TOT_FEMALE,
    hispanic_M  = H_MALE,
    hispanic_F  = H_FEMALE,
    white_M     = NHWA_MALE,
    white_F     = NHWA_FEMALE,
    black_M     = NHBA_MALE,
    black_F     = NHBA_FEMALE,
    asian_M     = NHAA_MALE,
    asian_F     = NHAA_FEMALE
  ) %>%
  # Label values accordingly
  mutate(
    year = plyr::mapvalues(year, 3:8, 2010:2015),
    age  = plyr::mapvalues(age, 1:18, c('00-15','00-15','00-15',
                                        '16-19','20-29','20-29','30-39','30-39','40-49','40-49',
                                        '50+','50+','50+','50+','50+','50+','50+','50+'))
  )



# Clean it up a bit
d <- d %>%
  # Remove 00-15 age category as irrelevant
  filter(age != '00-15') %>%
  mutate(
    # Create single FIPS code
    fips = paste0(state_fips, county_fips),
    # Create statistic for 'other' group
    other_M = total_M - hispanic_M - white_M - black_M - asian_M,
    other_F = total_F - hispanic_F - white_F - black_F - asian_F
  ) %>%
  select(-total_M, -total_F, -state_fips, -county_fips) %>%
  # Turn it into short data
  gather(label, count, -state, -county, -year, -age, -fips) %>%
  separate(label, c("race", "gender"), sep='_') %>%
  # Capitalize race
  mutate(race=stringr::str_to_title(race)) %>%
  # Group to remove duplicate age rows
  group_by(state, fips, year, age, race, gender) %>%
  summarize(count=sum(count)) %>%
  ungroup()

# Change '15-19' bucket to '16-19' by subtracting 20% from it.
d <- rbind(
  # take 20% off 16-19 bucket
  filter(d, age == '16-19') %>% mutate(count = round(count*0.8)),
  # take un-affected buckets
  filter(d, age != '16-19')
)

message(sprintf("Saving census data to %s", processed_intercensal_data_path))
# Write out the data frame
uncompressed_processed_data_path  = gsub('.gz', '', processed_intercensal_data_path)
readr::write_csv(d, uncompressed_processed_data_path)
# Compress data set
system(sprintf("gzip -9 %s", uncompressed_processed_data_path))

