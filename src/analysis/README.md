This folder contains the code to produce the analyses in the paper from the standardized data. 

* `compute_aggregate_rates.R` computes the rates reported in the paper aggregated across all states. 
* `fig1_states_data_map.R` produces the state data map. 
* `make_data_availability_table.R` produces the tables which summarize which data is available by state.
* `make_regression_table.R` produces the tables which summarize regression results. 
* `make_summary_statistics_csv.R` creates the dataframe of rates and thresholds broken down by race and county. 
* `marijuana_legalization_analysis.R` produces the marijuana legalization plots and analysis.
* `miscellaneous_claims.R` backs up miscellaneous claims and figures in the paper which do not involve a plot or table. 
* `rates_for_hypothetical_driver.R` computes the rates reported in the paper for a representative driver. 
* `stratify_by_covariates.R` creates county scatterplots for most rates reported in the paper and performs regressions. 
* `threshold_test_plots.R` produces the scatter plot of thresholds by location and PPCs. It does not fit the threshold test models. 
* `/threshold_test` contains code to run the threshold test and analyze the results, including model code in Stan. 


