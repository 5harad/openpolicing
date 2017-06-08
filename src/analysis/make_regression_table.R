# this file makes the summary tables of regression coefficients. 

extract_coef = function(coef_name, regression_results){
  # small helper method to extract the beta and standard error for a given coefficient from a regression table
  # printed out by summary(model). 
  result_line = regression_results[grepl(coef_name, regression_results)][1] # find line with coef.
  stopifnot(sum(grepl(coef_name, regression_results)) > 0)
  result_line = gsub("\\s+", " ", str_trim(result_line)) # replace big spaces with single space. 
  coef = as.numeric(word(result_line, 2))
  err = as.numeric(word(result_line, 3))
  beta = sprintf('%2.2f (%2.2f)', coef, err)
  return(beta)
}

make_supplementary_table_of_regression_results = function(regression_results_folder, variables_to_examine, output_fn){
  
  # this makes the supplementary summary table of regression results which contains all regression specifications. 
  # loops over the regression results text files 
  # each one contains a summary of a regression as printed out by summary(model)
  # and takes the relevant coefficients + errorbars. 
  
  df = NULL
  regression_types = c('race', 'race_and_county', 'race_and_county_and_time', 'race_and_county_and_demographics', 'race_and_county_and_time_and_demographics')
  for(race in c('Black', 'Hispanic')){
    race_df = NULL
    for(var in variables_to_examine){
      for(regression_type in regression_types){
        if((var == 'is_stopped') & (regression_type %in% c('race', 'race_and_county_and_time', 'race_and_county_and_time_and_demographics'))){
          next;
        }
        results_filename = sprintf('%s/national_aggregate_%s_stratified_by_%s_regression.txt',
                                   regression_results_folder,
                                   var, 
                                   regression_type)
        stopifnot(file.exists(results_filename))
        regression_results = readLines(results_filename)
        states = gsub('States used: |\\.', 
                      '', 
                      regression_results[grepl('States used:', regression_results)])
        states = paste(sort(str_split(states, ',', simplify = TRUE)), collapse = ',') # put into alphabetical order

        race_beta = extract_coef(paste0('C\\(driver_race\\)', race), regression_results) # small helper method to extract coef + error.
        race_df = rbind(race_df, data.frame(race = race, 
                                            variable = var, 
                                            regression_type = regression_type, 
                                            beta = race_beta, 
                                            states = states,
                                            stringsAsFactors = FALSE))
      }
    }
    
    race_df = as.data.frame(race_df)
    
    if(race == 'Black'){
      df = race_df
    }else{
      df = left_join(df, race_df, by = c('variable', 'regression_type', 'states'))
    }
  }
  df = select(df, variable, regression_type, beta.x, beta.y, states) %>%
    mutate(variable = plyr::mapvalues(variable, c('is_stopped', 'search_conducted', 'is_arrested', 'consent_search_conducted', 'cited_speeding_only'),
                                             c('Stopped', 'Searched', 'Arrested', 'Consent searched', 'Cited')), 
           regression_type = gsub('_and_', ', ', regression_type))
  df$regression_type = gsub('demographics', 'demo', df$regression_type)
  colnames(df) = c('Variable', 'Covariates', 'Black', 'Hispanic', 'States')
  print(xtable(df), include.rownames=FALSE, file = output_fn)
  print(df)
}

make_main_table_of_regression_results = function(regression_results_folder, variables_to_examine, include_demographics, output_fn){
  # makes the table of regression coefficients included in the main paper. 
  # if include_demographics is TRUE, includes demographic coefficients as well; we include that table in the supplement. 
  all_results = NULL
  for(var in variables_to_examine){
    
    regression_type = ifelse(var == 'is_stopped', 'race_and_county_and_demographics', 'race_and_county_and_time_and_demographics')
    results_filename = sprintf('%s/national_aggregate_%s_stratified_by_%s_regression.txt',
                               regression_results_folder,
                               var, 
                               regression_type)
    stopifnot(file.exists(results_filename))
    regression_results = readLines(results_filename)
    black_coef = extract_coef(paste0('C\\(driver_race\\)Black'), regression_results)
    hispanic_coef = extract_coef(paste0('C\\(driver_race\\)Hispanic'), regression_results)
    age_20_29_coef = extract_coef(paste0('C\\(driver_age_categorical\\)20-29'), regression_results)
    age_30_39_coef = extract_coef(paste0('C\\(driver_age_categorical\\)30-39'), regression_results)
    age_40_49_coef = extract_coef(paste0('C\\(driver_age_categorical\\)40-49'), regression_results)
    age_50_plus_coef = extract_coef(paste0('C\\(driver_age_categorical\\)50+'), regression_results)
    male_coef = extract_coef(paste0('C\\(driver_gender\\)M'), regression_results)
    if(include_demographics){
      all_coefs = data.frame(var, black_coef, hispanic_coef, male_coef, age_20_29_coef, age_30_39_coef, age_40_49_coef, age_50_plus_coef)
    }
    else{
      all_coefs = data.frame(var, black_coef, hispanic_coef)
    }
    all_results = rbind(all_results, all_coefs)
  }
  all_results$var = plyr::mapvalues(all_results$var, c('is_stopped','cited_speeding_only',  'search_conducted', 'is_arrested', 'consent_search_conducted'),
                                    c('Stopped', 'Cited', 'Searched', 'Arrested', 'Consent searched'))
  if(include_demographics){
    colnames(all_results) = c('', 'Black', 'Hispanic', 'Male', 'Age 20-29', 'Age 30-39', 'Age 40-49', 'Age 50+')
    print(xtable(t(all_results)), file = output_fn)
  }
  else{
    colnames(all_results) = c('', 'Black', 'Hispanic')
    print(xtable(all_results), file = output_fn)
  }
  print(all_results)
}



