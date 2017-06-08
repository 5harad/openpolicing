# This script creates the table(s) summarizing the availabitily of data for each
# state. This corresponds to tables 1 and S2 in the paper. A dot means that at
# least 70% of the rows in that state have a value for that field.
# In the paper, the header is multi-row.

# If all, summarize all states and rows in the data set.
# Otherwise, summarize only the states and stops used in the analysis.
summary_table <- function(all, output_fn) {
  # dictionary to map state abbreviations to full state names
  state_dict <- read_csv("resources/states_data.csv")
  # short-hand for bullet sign
  b <- '$\\bullet$'
  # set cut-off
  t <- 0.7
  # collect what states to process
  states <- FINAL_STATE_LIST
  if(all) {
    states <- c(states, STATES_PROCESSED_BUT_NOT_USED_IN_PAPER)
  }
  # data.frame to collect results
  res <- NULL
  # iterate over states
  for (state in sort(states)) {
    print(sprintf("Doing %s", state))
    # read in data, don't filter any data if 'all'
    row <- state %>%
      read_state(perform_data_checks=F, filters=!all) %>% 
      mutate(year=year(stop_date)) %>%
      # summarize by state
      group_by(state) %>%
      summarize( 
        n_stops      = n(),
        time_range   = paste(min(year, na.rm=T), min(2016, max(year, na.rm=T)), sep='-'),
        date         = ifelse(mean((day(stop_date) != 1) & !is.na(stop_date)) > t, b, ''), # no date if there are a lot of days set to 1 (indicates aggregate data) or a lot of missing days 
        time         = ifelse(mean(!is.na(stop_time  )) > t, b, ''),
        location     = ifelse(mean(!is.na(county_fips)) > t, b,
                       # exception for states with districts instead of counties
                       ifelse(state %in% c('RI','IL','NC'), b, '')),
        race         = ifelse(mean(!is.na(driver_race     )) > t, b, ''),
        gender       = ifelse(mean(!is.na(driver_gender   )) > t, b, ''),
        age          = ifelse(mean(!is.na(driver_age      )) > t, b, ''),
        violation    = ifelse(mean(!is.na(violation       )) > t, b, ''),
        search       = ifelse(mean(!is.na(search_conducted)) > t, b, ''),
        # only assess search_type and contraband data when a search was actually conducted
        search_type  = ifelse(mean(!is.na(search_type[search_conducted %in% c(TRUE)])) > t, b, ''),
        search_type  = ifelse(is.na(search_type), '', search_type), # if no searches conducted, no search_type data
        contraband  = ifelse(mean(!is.na(contraband_found[search_conducted %in% c(TRUE)])) > t, b, ''),
        contraband  = ifelse(is.na(contraband), '', contraband), # if no searches conducted, no contraband data
        stop_outcome = ifelse(mean(!is.na(stop_outcome), na.rm=T) > t, b, '')
      )
    # collect results
    res <- rbind(res, row)
  }
  res <- as.data.frame(res)
  # add whether the state is used in the analysis
  if(all) {
    res <- res %>% mutate(used=ifelse(state %in% FINAL_STATE_LIST, '+', ''))
  }
  # map state names
  res <- res %>%
    mutate(state=plyr::mapvalues(state, state_dict$state, state_dict$state_name)) %>%
    arrange(state)
  # add total count
  n_tot <- sum(res$n_stops)
  res <- rbind(res, c("\\textbf{Total}", n_tot, rep('', 13)))
  # format numbers
  res <- res %>% mutate(n_stops=format(as.numeric(n_stops), big.mark=','))
  # rewrite total stops
  res$n_stops[nrow(res)] <- sprintf("\\textbf{%s}", res$n_stops[nrow(res)])
  # add row number
  rownames(res) <- 1:nrow(res)
  rownames(res)[nrow(res)] <- ''
  if(all) {
    # reorder columns
    res <- res %>% select(state, used, 2:14)
  } else {
    # remove race
    res <- res %>% select(-race)
  }
  # format column names
  res = rename(res, stops = n_stops)
  colnames(res) = str_to_title(gsub('_', ' ', colnames(res)))
  # make latex table
  tab <- xtable(res, align=ifelse(all, 'llcrcccccccccccc', 'llrccccccccccc'))
  # write to output file
  fileConn <- file(output_fn)
  tab %>%
    print(sanitize.text.function=function(x){x}) %>%
    writeLines(fileConn)
  close(fileConn)
}
