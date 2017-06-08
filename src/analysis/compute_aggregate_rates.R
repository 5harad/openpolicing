# loop over all states for which we have good data and compute the aggregate rate by simply summing up.
compute_aggregate_rate = function(var, states){
  message(sprintf("\n\nComputing statistics for %s, aggregated across all states", var))
  full_d = NULL
  stopifnot(var %in% c('search_conducted', 'consent_search_conducted', 'is_arrested', 
                       'cited_speeding_only', 'contraband_found'))
  for(state in states){
    d = load_aggregate_data(state, cols_to_group_by = c('driver_race', 'stop_date'))
    if(var == 'search_conducted'){
      d$pos_count = d$n_searches
      d$total_count = d$n_stops_with_search_data
    }
    if(var == 'consent_search_conducted'){
      d$pos_count = d$n_consent_searches
      d$total_count = d$n_stops_with_consent_search_data
    }
    if(var == 'is_arrested'){
      d$pos_count = d$n_arrests
      d$total_count = d$n_stops_with_arrest_data
    }
    if(var == 'cited_speeding_only'){
      d$pos_count = d$n_citations_for_speeding
      d$total_count = d$n_citations_for_speeding + d$n_warnings_for_speeding
    }
    if(var == 'contraband_found'){
      d$pos_count = d$n_hits
      d$total_count = d$n_searches_with_contraband_data
    }
    d = d %>%
      mutate(year = year(stop_date)) %>% 
      group_by(driver_race, year) %>%
      summarise(pos_count = sum(pos_count), 
                total_count = sum(total_count),
                state = first(state))
    #filter out years with no data. 
    years_with_no_data = d %>% group_by(year) %>% filter(sum(pos_count) == 0)
    if(nrow(years_with_no_data) > 0){print(sprintf("Warning: lacking annual data for %s.", state))}
    d = filter(d, !(year %in% years_with_no_data$year))
    d = group_by(d, driver_race) %>%
      summarise(pos_count = sum(pos_count), 
                total_count = sum(total_count),
                state = first(state))
    
    #quick sanity checks. 
    stopifnot(sum(d$pos_count) > 0)
    stopifnot(sum(d$total_count) > 0)
    stopifnot(nrow(d) == 3)
    full_d = rbind(full_d, d)
  }
  stopifnot(unique(full_d$state) == states)
  print(full_d %>% 
          group_by(driver_race) %>% 
          summarise(pos_count = sum(pos_count), 
                    total_count = sum(total_count), 
                    rate = pos_count / total_count, 
                    n_states = length(unique(state)),
                    states = paste(sort(state), collapse = ',')) %>% 
          as.data.frame())
  message(sprintf("Aggregate across all races: %2.1f%%", 100 * sum(full_d$pos_count) / sum(full_d$total_count)))
}

message("Computing aggregate rates across all states!")
compute_aggregate_rate('search_conducted', GOOD_SEARCH_CONDUCTED_DATA)
compute_aggregate_rate('consent_search_conducted', GOOD_CONSENT_DATA)
compute_aggregate_rate('is_arrested', GOOD_ARREST_DATA)
compute_aggregate_rate('cited_speeding_only', GOOD_SPEEDING_CITATION_DATA)
compute_aggregate_rate('contraband_found', GOOD_CONTRABAND_FOUND_DATA) 



