# This script computes the rates by race for a prototypical male driver age 20 - 29.

compute_rates_for_hypothetical_driver = function(variables_to_use, output_fn){
  message("Computing rates for hypothetical driver.")
  gender_to_use = 'M'
  age_category_to_use = '20-29'
  # Loop over all variables computing representative rates. 
  rates = NULL
  for(variable in variables_to_use){
    message(sprintf("Loading %s", variable))
    # if stop rate, load the county and demographics model. Otherwise, include time variables as well. 
    # (we do this to be consistent with the regression model in Table 2). 
    # we have previously saved the model frame (ie, the data the model is fitted on) and the model coefficients as an RData object.
    if(variable == 'is_stopped'){
      load(sprintf('%s/national_aggregate_is_stopped_stratified_by_race_and_county_and_demographics_regression.RData', 
                   regression_results_folder))
      # we need this to marginalize out coefficients by computing a weighted mean. 
      model_frame$col_to_weight_by = model_frame$pos_count # weight by the number of stops. 
      model_frame = model_frame %>% select(-`offset(log(total_count))`) # remove superfluous columns. 
      
    }else{
      load(sprintf('%s/national_aggregate_%s_stratified_by_race_and_county_and_time_and_demographics_regression.RData', 
                   regression_results_folder, 
                   variable))
      # weight by the number of stops (ie, pos_count + neg_count)
      model_frame$pos_count = model_frame$`cbind(pos_count, total_count - pos_count)`[,1] 
      model_frame$neg_count = model_frame$`cbind(pos_count, total_count - pos_count)`[,2]
      model_frame$col_to_weight_by = model_frame$pos_count + model_frame$neg_count 
      # remove superfluous columns. 
      model_frame = model_frame %>% select(-`cbind(pos_count, total_count - pos_count)`, 
                                           -pos_count, 
                                           -neg_count)
    }
    # rename columns for easy access. 
    colnames(model_frame) = gsub('C\\(|\\)', '', colnames(model_frame))
    model_frame = as.data.frame(model_frame)
    # Now extract the values of the relevant coefficients. 
    # For age and gender, we use a 20-29 year old male. We will add in race at the end. 
    coefs = c('(Intercept)',
              sprintf('C(driver_age_categorical)%s', age_category_to_use), 
              sprintf('C(driver_gender)%s', gender_to_use))
    val = as.numeric(sum(model_coefficients[coefs]))

    # for other coefficient categories, take a weighted sum to represent the "typical" driver. 
    # This prevents us from having to choose arbitrary categories (eg, drivers on Tuesday in Q2). 
    if(variable == 'is_stopped'){
      all_coef_cats = get_regression_variables('race_and_county_and_demographics')
    }else{
      all_coef_cats = get_regression_variables('race_and_county_and_time_and_demographics')
    }
    for(coef_cat in all_coef_cats){
      # skip coefficient category if we've already added it in. 
      if(coef_cat %in% c('driver_age_categorical', 'driver_gender',  'driver_race')){
        next;
      }
      # otherwise, take the weighted mean of coefficients. 
      model_frame$col_to_group_by = as.character(model_frame[,coef_cat])
      # compute count in each subcategory (ie, count on Saturday, Sunday, Monday)
      grouped_d = model_frame %>% 
        filter(driver_age_categorical == age_category_to_use, driver_gender == gender_to_use) %>%
        group_by(col_to_group_by) %>% 
        summarise(count = sum(col_to_weight_by)) %>% 
        ungroup() %>% 
        arrange(col_to_group_by)
      # extract betas for each subcategory (ie, beta for Saturday, Sunday, Monday)
      betas_for_cat = model_coefficients[grepl(coef_cat, names(model_coefficients))]
      betas_for_cat = c(0, betas_for_cat) # add in 0 (for reference category not included).
      # make sure betas + counts are properly aligned -- same length and same order of coefficients. 
      stopifnot(nrow(grouped_d) == length(betas_for_cat))
      stopifnot(word(names(betas_for_cat), sep = '\\)', 2)[2:nrow(grouped_d)] == grouped_d$col_to_group_by[2:nrow(grouped_d)])
      # put into a single dataframe and compute the weighted mean. 
      grouped_d$beta = betas_for_cat
      weighted_mean = weighted.mean(grouped_d$beta, grouped_d$count)
      val = val + weighted_mean
    }
    stopifnot(!is.na(val))
    
    # Now we have to feed the value through the link function to compute the rates. 
    # For the stop model, this is the exponential function. 
    # For all other models, this is the logistic function
    link_function = ifelse(variable == 'is_stopped', exp, boot::inv.logit)
    # finally, add in race coefficients. 
    rates = rbind(rates, 
                  data.frame(variable = variable, 
                             White = link_function(val), 
                             Black = link_function(val + model_coefficients['C(driver_race)Black']), 
                             Hispanic = link_function(val + model_coefficients['C(driver_race)Hispanic'])))
  }
  print(rates)
  print(xtable(rates), file = output_fn, include.rownames=FALSE, digits = 3)
  
}


