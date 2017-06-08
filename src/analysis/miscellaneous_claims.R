message("This script reproduces miscellaneous claims in the paper which are not included in a figure or table.")
# "We find that Hispanics are stopped at similar rates as whites in most jurisdictions;
# black drivers, however, are stopped more often than whites in over 80% of the locations we consider"
message("\n\n\n*****Confirming that black drivers are stopped more frequently than white drivers in most locations!")
df = create_analysis_df(states = FINAL_STATE_LIST, variable = 'is_stopped', analysis_to_perform = 'county') %>%
  select(county_fips, driver_race, pos_fraction) %>% 
  spread(key = driver_race, value = pos_fraction)
message(sprintf("Black drivers stopped at higher rate than white drivers in %2.1f%% of counties", 
                100 * mean(df$Black > df$White, na.rm = T)))
message(sprintf("Hispanic drivers stopped at higher rate than white drivers in %2.1f%% of counties", 
                100 * mean(df$Hispanic > df$White, na.rm = T)))

# PPCS analysis: "We found that Hispanic respondents were less likely than white respondents to report being stopped (odds ratio .85)."
# Medina analysis cited in the paper substantiates this: "African Americans were 16.5% more likely to be pulled over than whites; 
# however Latinos were 11% less likely to be pulled over compared to whites." (see also the second regression in Table 2)
message("\n\n\n*****Conducting PPCS analysis!")
d = read_tsv(ppcs_data_path, col_types = list(PPCSWGT2011 = col_double()))
d = d %>% 
  mutate(sex = plyr::mapvalues(SEX, c(1, 2), c("M", "F")), 
         race = factor(ifelse(HISP == 1, 'Hispanic', 
                              ifelse(RACE == 1, 'White', 
                                     ifelse(RACE == 2, 'Black', NA))), levels = c('White', 'Black', 'Hispanic')), 
         stopped = ifelse(V13 == 1, TRUE, 
                          ifelse(V13 == 2, FALSE, NA)), 
         categorical_age = bucket_age(AGE), # divide age into categories the same way we do in the rest of the paper for consistency. 
         city_population = plyr::mapvalues(PLACE, 1:4, c('under_100k', '100-500k', '500k-1M', '1M+')))

model = glm(stopped ~ sex + race + categorical_age + city_population, data = d, family = binomial)
message("PPCS model, controlling for race, age, gender, and size of city")
print(summary(model))
message(sprintf("Hispanic odds ratio: %2.2f", exp(model$coefficients['raceHispanic'])))

## Threshold test aggregated across all states. 
message("\n\n\n****Computing national aggregate search thresholds (combined across all states)!")
compute_national_aggregate_thresholds(GOOD_THRESHOLD_TEST_DATA)

# Decline in searches of drivers NOT carrying contraband post-legalization. 
message("\n\n\n****Computing decline in searches of law-abiding drivers post legalization!")
search_counts = NULL
for(state in c('CO', 'WA')){
  d = read_state(state)
  legalization_date = get_legalization_date_for_state(state)
  stopifnot(as.numeric(as.Date('2012-10-01') - as.Date('2012-10-02')) == -1)
  stopifnot(as.numeric(as.Date('2012-10-01') - as.Date('2011-10-01')) == 366)
  d = d %>% 
    mutate(date_relative_to_legalization = as.numeric(stop_date - legalization_date), 
           year_before_legalization = (date_relative_to_legalization < 0) & (date_relative_to_legalization >= -365), 
           year_after_legalization = (date_relative_to_legalization >= 0) & (date_relative_to_legalization < 365),
           search_eligible = !search_is_excluded_from_marijuana_analysis(search_type, marijuana_excluded_search_types),
           eligible_search_with_no_contraband = search_eligible & search_conducted & (!contraband_found))
  searches_with_no_contraband_in_year_before = sum(d$eligible_search_with_no_contraband[d$year_before_legalization], na.rm = T)
  searches_with_no_contraband_in_year_after = sum(d$eligible_search_with_no_contraband[d$year_after_legalization], na.rm = T)
  search_counts = rbind(search_counts, data.frame(state, searches_with_no_contraband_in_year_before, searches_with_no_contraband_in_year_after, stringsAsFactors = FALSE))
}
print(search_counts %>% as.data.frame())
message(sprintf('Total searches with no contraband in year before: %i; total searches with no contraband in year after: %i', 
                sum(search_counts$searches_with_no_contraband_in_year_before), sum(search_counts$searches_with_no_contraband_in_year_after)))

