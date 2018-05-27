# Paths
figures_folder = paste0(code_path, '/results/figures/')
tables_folder = paste0(code_path, '/results/tables/')
data_for_figures_folder = paste0(code_path, '/results/data_for_figures/')
threshold_test_code_folder = paste0(code_path, '/src/analysis/threshold_test/')

data_folder_local = paste0(data_and_models_path, 'data/') # local path for saving state data. 
aggregate_data_folder = paste0(data_and_models_path, 'aggregate_data/') # path for saving aggregate data. 
regression_results_folder = paste0(data_and_models_path, 'regression_results/') # save regression results in separate folder because regression Rdata files get large.  
threshold_test_output_folder = paste0(data_and_models_path, 'fitted_threshold_models/') # folder for fitted Stan models.
aggregate_marijuana_data_filename = paste0(aggregate_data_folder, 'marijuana_results.RData') # file to save intermediate marijuana analysis results
external_datasets_folder = paste0(data_and_models_path, 'external_datasets/') # folder that stores external datasets necessary to support the analysis. 
raw_intercensal_data_path = paste0(external_datasets_folder, 'census-raw.csv.gz') # filepath for the raw Census data. 
processed_intercensal_data_path = paste0(external_datasets_folder, 'census-clean.csv.gz') # filepath for the processed Census data. 
ppcs_data_path = paste0(external_datasets_folder, 'ppcs.tsv') # filepath for PPCS data. Source: https://doi.org/10.3886/ICPSR34276.v1

# make sure paths are correctly set up. 
stopifnot(file.exists(data_and_models_path), 
          file.exists(data_folder_local),
          file.exists(aggregate_data_folder),
          file.exists(regression_results_folder),
          file.exists(threshold_test_output_folder),
          file.exists(external_datasets_folder))
          

# Constants + global settings
THRESHOLD_TEST_MODEL_NAME = 'flat'
# states included in analysis
FINAL_STATE_LIST = c('AZ','CA','CO','CT','FL','IL','MA','MD','MO','MT',
                     'NC','NE','NJ','NY','OH','RI','SC','TX','VT','WA','WI')
# states which are processed but have inadequate data for paper
STATES_PROCESSED_BUT_NOT_USED_IN_PAPER = c('GA','IA','MI','MS','ND','NH','NV','OR','SD','TN','VA','WY')
# all states
ALL_PROCESSED_STATES = c(FINAL_STATE_LIST, STATES_PROCESSED_BUT_NOT_USED_IN_PAPER)
# states with sufficiently high-quality data to include in specific analyses. 
GOOD_SEARCH_CONDUCTED_DATA = c('AZ', 'CA', 'CO', 'CT', 'FL', 'IL', 'MA', 'MD', 'MO', 'MT', 'NC', 'NE', 'OH', 'RI', 'SC', 'TX', 'VT', 'WA', 'WI')
GOOD_CONSENT_DATA = c('CO', 'FL', 'MA', 'MD', 'NC', 'TX', 'WA')
GOOD_ARREST_DATA = c('AZ', 'CA', 'CO', 'CT', 'FL',  'MA', 'MD', 'MT', 'NC', 'OH', 'RI', 'SC', 'VT', 'WI')
GOOD_SPEEDING_CITATION_DATA =  c('CO', 'FL', 'IL', 'MT', 'NC', 'RI',  'TX', 'WI')
GOOD_COUNTY_LEVEL_CONTRABAND_DATA = c('CO', 'CT', 'IL', 'NC', 'RI', 'SC', 'TX', 'WA', 'WI' )
GOOD_THRESHOLD_TEST_DATA = GOOD_COUNTY_LEVEL_CONTRABAND_DATA
GOOD_CONTRABAND_FOUND_DATA = c(GOOD_COUNTY_LEVEL_CONTRABAND_DATA, c('MD', 'MO', 'VT'))
# MA and AZ have contraband found data but it's messy enough that we exclude from analyses. 
# MO and MD are excluded from county-level contraband analysis (including threshold test) because no county data. 
# VT is excluded from county-level contraband analysis + regressions because it has too few minorities to run threshold test + we want to be consistent. 
# However, we include MO, MD, and VT in aggregate contraband analysis because there is no need for county-level information. 

# states with bad data for either time/date covariates or demographic covariates (exclude from regressions which use these variables)
BAD_TIME_OR_DATE_DATA = c('CA', 'MA', 'MD', 'MO', 'NC', 'NE', 'SC')
BAD_AGE_OR_GENDER_DATA = c('AZ', 'CA', 'MD', 'MO', 'NE', 'NJ', 'OH', 'TX', 'WI')

# Constants for marijuana analysis: control states, legalization dates. 
MARIJUANA_CONTROL_STATES = c('AZ', 'CA', 'MA', 'MT', 
                             'NC', 'OH', 'RI', 'SC', 
                             'TX', 'VT', 'WI', 'FL')
COLORADO_LEGALIZATION_DATE   = as.Date("2012-12-10")
WASHINGTON_LEGALIZATION_DATE = as.Date("2012-12-09")

# list of variables we can run regressions over and make scatterplots for. 
ALL_BINARY_VARIABLES = c(
  "is_stopped",# stop rate benchmark analysis
  "search_conducted",# whether search is conducted
  "consent_search_conducted", # whether a consent search was conducted
  "is_arrested",#whether person is arrested
  "cited_speeding_only",#whether a person was cited, given that they were speeding.
  "contraband_found"# whether contraband was found (hit rate / outcome test)
)