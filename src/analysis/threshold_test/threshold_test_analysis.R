
# This script contains extra analysis code to inspect the threshold test models.

threshold_ci <- function(post, obs){
  # Create data frame with the threshold and its 95% credible interval for each race
  stopifnot(levels(obs$driver_race) == c('White', 'Black', 'Hispanic'))
  df    <- as.data.frame(cbind(levels(obs$driver_race), round(colMeans(post$thresholds)*100,1)))
  ci    <- apply(post$thresholds, 2, function(x) quantile(x, probs = c(0.025, 0.975)) ) 
  df$V3 <- paste0('(', round(ci[1,]*100,1), ', ', round(ci[2,]*100,1), ')')
  white_black_diff = post$thresholds[,which(levels(obs$driver_race) == 'White')] - 
    post$thresholds[,which(levels(obs$driver_race) == 'Black')]
  white_hispanic_diff = post$thresholds[,which(levels(obs$driver_race) == 'White')] - 
    post$thresholds[,which(levels(obs$driver_race) == 'Hispanic')]
  #message("95% CI for white minus black threshold")
  #print(quantile(white_black_diff, probs = c(.025, .975)))
  #message("95% CI for white minus Hispanic threshold")
  #print(quantile(white_hispanic_diff, probs = c(.025, .975)))
  names(df) <- c('Race', 'Threshold', '95% Credible Interval')
  return(df)  
}

fit_summary <- function(fit, post, obs, state_name){
  # generate a summary of the fit. 
  # We don't actually use this directly in the paper, but useful for checking model convergence. 
  stopifnot(levels(obs$driver_race) == c('White', 'Black', 'Hispanic'))
  t          <- threshold_ci(post,obs)
  t_white    <- paste(t[1,2], t[1,3])
  t_black    <- paste(t[2,2], t[2,3])
  t_hispanic <- paste(t[3,2], t[3,3])
  
  s     <- rstan::summary(fit)
  rhat  <- max(s$summary[,'Rhat'], na.rm = TRUE)
  n_eff <- round(min(s$summary[,'n_eff']))
  
  params = rstan::get_sampler_params(fit, inc_warmup = FALSE)
  for(i in 1:length(params)){ # for some reason divergent is inconsistently named. 
    if(!('n_divergent__'  %in% colnames(params[[i]]))){
      params[[i]] = as.data.frame(params[[i]])
      params[[i]][, 'n_divergent__'] = params[[i]][, 'divergent__']
    }
  }
  n_divergent = sum(sapply(params, function(x) sum(x[,'n_divergent__'])))
  
  obs$pred_hit_rate = colMeans(post$hit_rate)
  rms_error = round(with(obs,100*sqrt(weighted.mean((hit_rate-pred_hit_rate)^2, num_stops))),1)
  row <- data.frame(State=state_name, White=t_white, Black=t_black, Hispanic=t_hispanic, Rhat=rhat, N_div=n_divergent, N_eff=n_eff, RMS=rms_error) 
  return(row)
}


get_model_results_all_states <- function(model_name){
  d = data.frame()
  for(i in 1:length(GOOD_THRESHOLD_TEST_DATA)){
    path = paste0(threshold_test_output_folder, sprintf('%s_%s.RData', tolower(GOOD_THRESHOLD_TEST_DATA[i]), model_name))
    print(sprintf('Loading %s...', GOOD_THRESHOLD_TEST_DATA[i]))
    print(path)
    load(path)
    model_result = fit_summary(fit, post, obs, GOOD_THRESHOLD_TEST_DATA[i])
    d = rbind(d, model_result)
  }
  return(d)
}

##### 
# calculate aggregate thresholds

# Code to compute aggregate thresholds and CIs across all states. 
# For each posterior sample (we have 12,500) we want to compute the weighted threshold 
# across all counties. We weight by number of stops in the county (for all drivers).
# This yields a vector of 12,500 statistics. Then we compute the mean + CI of that. 
# This should be exactly the same as the computation for a single state, but extended to work for all states.

compute_national_aggregate_thresholds = function(states_to_use){
  all_post_ti = NULL
  all_obs = NULL
  races_to_use = c('White', 'Black', 'Hispanic')
  # First, loop over all states and load in the t_i samples + population of each county. 
  for(s in states_to_use){
    message(sprintf("loading threshold test results for %s", s))
    path = paste0(threshold_test_output_folder, sprintf('%s_%s.RData', tolower(s), THRESHOLD_TEST_MODEL_NAME))
    load(path)
    
    # filter out locations which do not have thresholds for all races. 
    # because they lack searches for at least one race.
    # (want to make sure we're comparing apples to apples -- same set of locations for all races.)
    complete_locations <- obs %>% 
      group_by(location_variable) %>% 
      summarise(count=n()) %>% 
      filter(count == length(races_to_use))
    complete_location_idxs <- sort(which(obs$location_variable %in% complete_locations$location_variable))
    message(sprintf("%i / %i locations have all races.", 
                    length(unique(complete_locations$location_variable)), 
                    length(unique(obs$location_variable))))
    stopifnot(obs[complete_location_idxs,] %>% group_by(location_variable) %>% summarise(n = length(unique(driver_race))) %>% select(n) %>% unique() == length(races_to_use))
    # filter in obs
    stops_per_county = obs %>% group_by(location_variable) %>% summarise(total_stops = sum(num_stops))
    obs <- obs %>% 
      filter(location_variable %in% complete_locations$location_variable) %>% 
      left_join(stops_per_county, by = 'location_variable')
    
    # filter in posterior samples
    samples <- post$t_i[,complete_location_idxs]
    
    # Make sure shapes match. 
    stopifnot(nrow(obs) == dim(samples)[2])
    stopifnot(length(complete_location_idxs) == nrow(obs))
    
    all_obs = rbind(all_obs, obs)
    all_post_ti = cbind(all_post_ti, samples)
  }
  
  # Now we have a giant matrix of posterior samples (all_post_ti)
  # each column of which corresponds to one race in one county. 
  # We want the weighted sum of columns. 
  message(sprintf("Computing aggregate thresholds using %s", paste(states_to_use, collapse = ',')))
  posterior_samples_by_race = NULL
  for(race in races_to_use){
    weighting = (all_obs$driver_race == race) * (all_obs$total_stops)
    weighting = weighting / sum(weighting) 
    weighted_statistics = 0
    # compute weighted statistics by computing weighted sum of columns. 
    for(j in 1:length(weighting)){
      weighted_statistics = weighted_statistics + weighting[j] * all_post_ti[,j]
    }
    posterior_samples_by_race = rbind(posterior_samples_by_race, weighted_statistics)
    message(sprintf("Race: %s. Threshold %2.3f", 
                    race, 
                    mean(weighted_statistics)))
    print(quantile(weighted_statistics, c(.025, .975)))
  }
  white_black_difference = posterior_samples_by_race[which(races_to_use == 'White'),] - 
    posterior_samples_by_race[which(races_to_use == 'Black'),]
  white_hispanic_difference = posterior_samples_by_race[which(races_to_use == 'White'),] - 
    posterior_samples_by_race[which(races_to_use == 'Hispanic'),]
  message(sprintf("White-black difference: mean %2.3f", 
                  mean(white_black_difference)))
  print(quantile(white_black_difference, c(.025, .975)))
  
  message(sprintf("White-Hispanic difference: mean %2.3f", 
                  mean(white_hispanic_difference)))
  print(quantile(white_hispanic_difference, c(.025, .975)))
}

