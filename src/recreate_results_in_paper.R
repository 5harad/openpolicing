# This script processes all raw state data and runs all analyses in the paper. 
# It contains very little analysis code, and just sources the scripts that do the heavy lifting. 

rm(list=ls())

# IMPORTANT: set code_path and data_and_models_path to suit your system. 
# code_path is where you are storing the code. 
# data_and_models_path is where you are storing the data and models (these files can get large). 
code_path = "~/projects/old/2017/openpolicing/"
data_and_models_path = "~/projects/old/2017/openpolicing/data_and_models/"
setwd(code_path)

# First, source widely used utility scripts. 
# more specific analysis scripts are sourced only when they are used. 
# for additional explanation of script functions, see README in relevant directory. 
source('src/util/libraries.R') # libraries used throughout the analysis
source('src/util/constants_and_paths.R') # defines constants and paths.
source("src/processing/aggregation.R") # computes and loads aggregate data (working with stop-level data takes too long)
source("src/processing/processing_driver.R") # helper code for processing raw state data
source("src/analysis/stratify_by_covariates.R") # runs regressions and makes county scatterplots
source("src/util/util_functions.R") # utility functions. 
source('src/util/theme_for_plots.R') # ggplot theme + code to keep plots consistent. 

# Which analyses to run: 
# modify the parameters below depending on which analyses you want to run and what your computing resources are. 
n_cores = 1 # number of cores to use in processing data. 
install_libraries = FALSE # install libraries.
reprocess_census_data = FALSE # process the raw Census data needed to compute stop rates. 
reprocess_raw_state_data = FALSE # reprocess raw state data (takes a few hours)
reaggregate = FALSE # reaggregate state data into convenient aggregate statistics (takes some time)
rerun_regressions = FALSE # rerun regressions for binary variables (takes some time)
rerun_regressions_on_individual_states = FALSE # rerun disparate impact regressions on individual states (robustness check; takes some time).  
rerun_threshold_test = FALSE # refit all threshold test models (takes roughly a day, on 5 cores)
rerun_marijuana_threshold_test = FALSE # refit threshold test for pre- and post-legalization in CO and WA
remake_plots = FALSE # make all plots in paper
remake_tables = FALSE # make all tables in paper (takes some time to make some tables)
print_aggregate_rates = FALSE # print aggregate search, hit rate etc aggregated across all states
remake_summary_csv = FALSE # make the CSV of rates broken down by race + county. 
reproduce_miscellaneous_claims = FALSE # reproduce claims in the paper that have no plot or table attached. 

if(install_libraries){
  libs = c('dplyr','plyr', 'tidyr','readr','parallel','sandwich',
           'rgdal', 'xtable', 'lmtest', 'stringr', 'ggplot2', 
           'lubridate', 'scales', 'foreach', 'rstan', 'pracma',
           'matrixStats', 'boot', 'stats', 'MASS')
  install.packages(libs)
}

if(reprocess_census_data){source('src/processing/process_census_data.R')}

# rerunning states relies on code in processing/ directory. You will also need to download the raw state data prior to running this code. 
# it is not necessary to reprocess raw state data to run the rest of the analysis; you can start with the processed data. 
if(reprocess_raw_state_data){mclapply(c(FINAL_STATE_LIST, STATES_PROCESSED_BUT_NOT_USED_IN_PAPER), process_state, mc.cores = n_cores)}

# aggregation relies on code in src/processing/aggregation.R
if(reaggregate){
  create_all_aggregate_files(states_to_use = FINAL_STATE_LIST, 
                             n_cores = n_cores)
  # also reaggregate the data into the form we need for the marijuana analysis. 
  source('src/analysis/marijuana_legalization_analysis.R')
  save_aggregate_marijuana_data(aggregate_marijuana_data_filename)
}

if(rerun_regressions)
{
  run_all_regressions = function(var){
    # small helper method that just runs all regressions + makes age/gender and county graphs for a given variable. 
    analysis_regression(states = FINAL_STATE_LIST, variable = var, regression_variables_to_use = 'race_and_county')
    analysis_regression(states = FINAL_STATE_LIST, variable = var, regression_variables_to_use = 'race_and_county_and_demographics')
    if(var != 'is_stopped'){ # for non-stop rate variables, run time regressions as well as one with no county covariate (just raw race coefficient)
      analysis_regression(states = FINAL_STATE_LIST, variable = var, regression_variables_to_use = 'race')
      analysis_regression(states = FINAL_STATE_LIST, variable = var, regression_variables_to_use = 'race_and_county_and_time_and_demographics')
      analysis_regression(states = FINAL_STATE_LIST, variable = var, regression_variables_to_use = 'race_and_county_and_time')
    }else{ # for stop rate, run additional quasipoisson + sandwich models. 
      analysis_regression(states = FINAL_STATE_LIST, variable = var, regression_variables_to_use = 'race_and_county_and_demographics', stop_rate_regression_type = 'poisson_sandwich')
      analysis_regression(states = FINAL_STATE_LIST, variable = var, regression_variables_to_use = 'race_and_county_and_demographics', stop_rate_regression_type = 'quasipoisson')
    }
  }
  mclapply(ALL_BINARY_VARIABLES, run_all_regressions, mc.cores = n_cores)
}

if(rerun_regressions_on_individual_states){ 
  # robustness check: make sure regression results occur consistently across states (okay if occasional state deviates -- noise, data heterogeneity).
  # define a small helper method to run all specifications and outcome variables for a single state (so we can parallelize across states)
  run_disparate_impact_regressions_on_single_state = function(state){
    all_results_for_state = NULL
    for(var in c('cited_speeding_only', 'search_conducted', 'consent_search_conducted', 'is_arrested')){
      for(regression in c('race', 'race_and_county', 'race_and_county_and_demographics', 'race_and_county_and_time', 'race_and_county_and_time_and_demographics')){
        coefs = analysis_regression(states = c(state), variable = var, regression_variables_to_use = regression)
        if(!is.null(coefs)){
          black_driver_coefficient = coefs['C(driver_race)Black', 'Estimate']
          hispanic_driver_coefficient = coefs['C(driver_race)Hispanic', 'Estimate']
        }else{
          black_driver_coefficient = NA
          hispanic_driver_coefficient = NA
        }
        all_results_for_state = rbind(all_results_for_state, data.frame(state, var, regression, black_driver_coefficient, hispanic_driver_coefficient, stringsAsFactors = FALSE))
      }
    }
    return(all_results_for_state)
  }
  # now loop over all states in parallel and collect all regression coefficients in a single dataframe. 
  results_for_all_states = mclapply(FINAL_STATE_LIST, run_disparate_impact_regressions_on_single_state, mc.cores = n_cores) %>% bind_rows()
  output_path = sprintf('%s/coefficients_for_individual_states.csv', regression_results_folder)
  write_csv(results_for_all_states, output_path)
  message(sprintf("Successfully completed regressions for all states individually and saved to output path %s", output_path))
  message("Summary of robustness check results:")
  print(results_for_all_states %>% 
          group_by(var, regression) %>% 
          summarise(black_rate_lower_than_white_rate = sum(black_driver_coefficient < 0, na.rm = T), 
                    hispanic_rate_lower_than_white_rate = sum(hispanic_driver_coefficient < 0, na.rm = T),
                    total_states = sum((!is.na(hispanic_driver_coefficient)) & (!is.na(black_driver_coefficient))), 
                    states_with_lower_black_rate = paste(state[!is.na(black_driver_coefficient) & (black_driver_coefficient < 0)], collapse = ','), 
                    states_with_lower_hispanic_rate = paste(state[!is.na(hispanic_driver_coefficient) & (hispanic_driver_coefficient < 0)], collapse = ',')) %>% 
          as.data.frame())
}

if(rerun_threshold_test){
  source('src/analysis/threshold_test/run_threshold_test.R')
  run_threshold_test_for_all_states()
}

if(rerun_marijuana_threshold_test){
  source('src/analysis/threshold_test/marijuana_legalization_threshold_analysis.R')
  run_threshold_test_marijuana('CO')
  run_threshold_test_marijuana('WA')
}

if(remake_tables){ 
  
  # Make table 1 in main text.
  source("src/analysis/make_data_availability_table.R")
  summary_table(all=F, output_fn=paste0(tables_folder, 'table_1.tex'))
  
  # Make table 2 in main text.
  source("src/analysis/make_regression_table.R")
  vars_to_include = c('is_stopped', 'cited_speeding_only', 'search_conducted', 'consent_search_conducted', 'is_arrested')
  output_fn = paste0(tables_folder, 'table_2.tex')
  message("\n\nMaking table 2 (regression table in main text)")
  make_main_table_of_regression_results(regression_results_folder, vars_to_include, include_demographics = TRUE, output_fn = output_fn)
  
  # Make table 3 in main text. 
  source("src/analysis/rates_for_hypothetical_driver.R")
  output_fn = paste0(tables_folder, 'table_3_hypothetical_driver.tex')
  compute_rates_for_hypothetical_driver(vars_to_include, output_fn)
  
  # Make table 4 in main text (marijuana diff-in-diff)
  source('src/analysis/marijuana_legalization_analysis.R')
  diff_in_diff_analysis(aggregate_marijuana_data_filename, output_fn=paste0(tables_folder, 'table_4.tex'))
  
  # Make table 5 in main text (marijuana thresholds)
  source('src/analysis/threshold_test/marijuana_legalization_threshold_analysis.R')
  marijuana_threshold_table(paste0(tables_folder, 'table_5.tex'))
  
  # Make table 2 in SI.
  source('src/analysis/make_data_availability_table.R')
  summary_table(all=T, output_fn=paste0(tables_folder, 'table_S2.tex'))
  
  # Make table 3 in SI.
  message("\n\nMaking table S3 (regression table with all regression specifications)")
  output_fn = paste0(tables_folder, 'table_S3.tex')
  make_supplementary_table_of_regression_results(regression_results_folder, vars_to_include, output_fn = output_fn)
}

if(remake_plots){
  message("Remaking all figures!\n")
  
  # make map (fig 1)
  source('src/analysis/fig1_states_data_map.R')
  make_data_availability_map()
  
  # make binary variable plots: fig 2, 3a, 3b, 4a. This relies on stratify_by_covariates.R. 
  for(var in c('is_stopped', 'search_conducted', 'is_arrested', 'contraband_found')){
    message(sprintf("Making county scatterplot for %s\n", var))
    analysis_county(states = FINAL_STATE_LIST, variable = var)
  }
  # make threshold test plot (fig 4b)
  source('src/analysis/threshold_test_plots.R')
  make_threshold_scatterplot(GOOD_THRESHOLD_TEST_DATA, model_name = THRESHOLD_TEST_MODEL_NAME)
  
  # make marijuana plots (fig 5 - 6)
  source('src/analysis/marijuana_legalization_analysis.R')
  make_marijuana_plots(aggregate_marijuana_data_filename)
  message("Successfully made all figures.")
  
  # make threshold test posterior predictive check plots. 
  source('src/analysis/threshold_test_plots.R')
  rate_ppc('search_rate', ylim = .03)
  rate_ppc('hit_rate', ylim = .3)
  
}

if(print_aggregate_rates){
  source('src/analysis/compute_aggregate_rates.R')
}

if(remake_summary_csv){
  source('src/analysis/make_summary_statistics_csv.R')
}

if(reproduce_miscellaneous_claims){
  # This reproduces a few miscellaneous claims/figures in the paper which don't have plots or tables attached: 
  # the percentage of counties in which black drivers are stopped at higher rates, the PPCS analysis, 
  # the national aggregate thresholds, and the drop in searches of law-abiding drivers post marijuana legalization. 
  source('src/analysis/threshold_test/threshold_test_analysis.R') # need this to compute national aggregate thresholds
  source('src/analysis/marijuana_legalization_analysis.R') # need this to compute drop in count of searches. 
  source('src/analysis/miscellaneous_claims.R') # script which actually computes things. 
}

warnings()

