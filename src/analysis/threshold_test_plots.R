# This script produces a scatter plot (Figure 4b in the paper) representing the 
# county-level threshold estimates coming from the threshold model.

make_threshold_scatterplot = function(states_to_use, model_name){
  message("\n\nRecreating thresholds figure.\n")

  d <- data.frame()
  # loop over all states and collect the race-location specific thresholds in each state. 
  for(i in 1:length(states_to_use)){
    model_path <- paste0(threshold_test_output_folder, sprintf('%s_%s.RData', tolower(states_to_use[i]), model_name))
    print(sprintf('Loading %s...', states_to_use[i]))
    print(model_path)
    load(model_path)
    obs$t_rd <- colMeans(post$t_i) # posterior mean of threshold for each race-location group. 
    obs$state <- (states_to_use[i])
    obs$driver_race = paste(as.character(obs$driver_race), 'drivers')
    d <- rbind(d, obs)
  }
  
  # save data for figure. 
  data_for_figure = d %>% 
    rename(inferred_threshold = t_rd)

  # Plots the department thresholds for each race 
  df <- d %>%
    filter(driver_race == 'White drivers') %>%
    right_join(d %>% filter(driver_race != 'White drivers'), by=c('location_variable', 'state'))
  # make sure we're only plotting one threshold for each minority group - location. 
  stopifnot(sum(duplicated(df %>% filter(driver_race.y == 'Black drivers') %>% select(state, location_variable))) == 0)
  stopifnot(sum(duplicated(df %>% filter(driver_race.y == 'Hispanic drivers') %>% select(state, location_variable))) == 0)
  mx_t <- max(d$t_rd)
  mx   <- ceiling(mx_t/0.05)*0.05 + .05
  
  # Make the plot and save the data. 
  fn = 'fig4b_threshold_test'
  data.frame(x=df$t_rd.x, y=df$t_rd.y, n=df$num_stops.y, minority=df$driver_race.y) %>%
    scatter_circles(fn=fn, var='threshold', lim=c(0, mx))
  write_csv(data_for_figure, paste0(data_for_figures_folder, fn, '.csv'))
}


rate_ppc <- function(rate_to_plot, ylim=0.03, model_name = 'flat', states = tolower(GOOD_THRESHOLD_TEST_DATA)) {
  # plot the rate posterior predictive check for all states. rate_to_plot should be either search_rate or hit_rate. 
  message(sprintf("Making PPC plot for %s", rate_to_plot))
  stopifnot(rate_to_plot %in% c('search_rate', 'hit_rate'))
  all_obs  <- data.frame()

  # loop over all states and collect the race-location specific thresholds data in each state. 
  for(i in 1:length(states)){
    state = states[i]
    model_path <- paste0(threshold_test_output_folder, sprintf('%s_%s.RData', state, model_name))
    print(sprintf('Loading %s model from path %s', state, model_path))
    load(model_path)
    if(rate_to_plot == 'search_rate'){
      obs$predicted_rate <- colMeans(post$search_rate)
      obs$true_rate = obs$search_rate
    }else{
      obs$predicted_rate <- colMeans(post$hit_rate)
      obs$true_rate = obs$hit_rate
    }
    all_obs  <- rbind(all_obs, obs)
  }
  english_label = gsub('_', ' ', rate_to_plot)
  rms_error = with(all_obs,100*sqrt(weighted.mean((predicted_rate-true_rate)^2, num_stops)))
  message(sprintf("RMS error for %s is %2.3f", rate_to_plot, rms_error))
  p <- ggplot(data=all_obs, aes(x=predicted_rate, y=predicted_rate-true_rate)) +
    geom_point(aes(size=num_stops, color=driver_race), alpha = 0.8) + scale_size_area(max_size=10) +
    scale_x_continuous(sprintf('\nPredicted %s', english_label), labels=scales::percent)+
    scale_y_continuous(sprintf('%s prediction error\n', Hmisc::capitalize(english_label)), labels=scales::percent, limits=c(-ylim, ylim)) +
    geom_abline(slope=0, intercept=0, linetype='dashed') +
    scale_color_manual(values=c('blue','black','red')) +
    guides(size=FALSE) + 
    our_theme() + 
    theme(legend.position=c(.9, .85), 
          panel.margin=unit(0.5, "lines"), 
          plot.margin=unit(c(.1, .2, .1, .1), units = 'in'))
  p
  ggsave(
    sprintf(
      '%s/%s%s_ppc.pdf', figures_folder,
      ifelse(model_name==THRESHOLD_TEST_MODEL_NAME, '', paste0(model_name, '_')),
      rate_to_plot),
    p,
    width=10,
    height=4)
}
