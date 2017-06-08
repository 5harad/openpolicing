# This script produces a map (Fig. 1 in the paper) of the US, representing
# the status of the data for each state.
make_data_availability_map <- function(){
  message("Making Figure 1: state data availability map.")
  # Read the shapefile
  states_shape <- readOGR(dsn="resources/shapes/states/", layer='cb_2015_us_state_20m')

  # Get the names for each polygon
  states_map <- data.frame(nid=1:52, state_name=states_shape$NAME)
  
  # Value map
  map_keys = c('Good data', 'Bad data', 'No data', 'No response')
  map_vals = c('Necessary data received', 'Insufficient data received', 'No data received', 'No data received')
  map_lvls = c('Necessary data received', 'Insufficient data received', 'No data received')
  
  # Read data for figure
  data_for_figure = read_csv("resources/states_data.csv")
  
  # Cast the shapefile as data frame
  d <- states_shape %>%
    fortify() %>%
    mutate(nid=as.numeric(as.character(id))+1) %>%
    # Remove non-mainland states
    filter(!nid %in% c(14,39,42)) %>%
    # Add human-readable names
    left_join(states_map, by='nid') %>%
    left_join(data_for_figure %>%
                mutate(status=factor(map(status, map_keys, map_vals), levels=map_lvls))
              , by='state_name') %>%
    filter(!is.na(status))
  
  # Make the plot
  p <- ggplot(d) +
    geom_polygon(aes(x=long, y=lat, group=group, fill=status), colour='black', size=0.2) +
    scale_fill_manual(values=c('#74a9cf', 'lightgrey', 'white'), labels=map_lvls) +
    our_theme() +
    # some extra theme stuff to make the map more clear
    theme(
      panel.border=element_blank(),
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      axis.ticks=element_blank(),
      axis.text.y=element_blank(),
      axis.text.x=element_blank(),
      legend.title=element_blank(),
      legend.text=element_text(size=10),
      legend.position=c(0.88, 0.14),
      plot.margin=unit(c(0.0, 0.0, 0.0, 0.0), "cm")
    )
  
  # Save the plot
  fn = 'fig1_states_data_map'
  ggsave(paste0(figures_folder, fn, '.pdf'), p, height=5, width=8, dpi=500)
  write_csv(data_for_figure, paste0(data_for_figures_folder, fn, '.csv'))
}