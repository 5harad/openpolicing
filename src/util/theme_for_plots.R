# This script contains the shared themes for making figures in the paper. 

our_theme <- function() {
  # Set the base size
  theme_bw(base_size=15) +
    theme(
      # Remove the title
      plot.title=element_blank(),
      # Make the background white
      panel.background=element_rect(fill='white', colour='white'),
      panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
      # Minimize margins
      plot.margin=unit(c(0.2, 0.2, 0.2, 0.2), "cm"),
      panel.margin=unit(0.25, "lines"),
      # Tiny space between axis labels and tick labels
      axis.title.x=element_text(margin=ggplot2::margin(t=6.0)),
      axis.title.y=element_text(margin=ggplot2::margin(r=6.0)),
      # Simplify the legend
      legend.key=element_blank(),
      legend.background=element_rect(fill='transparent'),
      legend.title=element_blank()
    )
}

# Extra rule for thicker lines
update_geom_defaults("line", list(size=1))

# Function to make a minority vs white scatter plot
# Arguments: 
# df: the data: a data.frame with (x, y, n, minority). x is the value for whites in one location, y is the value for minorities, n sizes the point, 
# and minority denotes which minority group. 
# fn: the filename to save the figure. 
# var: the name of the variable being plotted. 
scatter_circles <- function(df, fn, var, lim=c(0,NA), use_percent=TRUE, max_size=15, breaks=waiver()) {
  p <- ggplot(df) +
    geom_point(aes(x=x, y=y, size=n), shape=1, alpha=0.6) +
    facet_grid(.~minority) +
    geom_abline(slope=1, intercept=0, linetype='dashed') +
    scale_size_area(max_size=max_size) +
    guides(size=FALSE, color=FALSE) +
    our_theme()
  if(use_percent){
    p = p + 
      scale_x_continuous(paste('White'   , var), limits=lim, labels = scales::percent, expand=c(0,0), breaks = breaks) +
      scale_y_continuous(paste('Minority', var), limits=lim, labels = scales::percent, expand=c(0,0), breaks = breaks)
  }
  else{
    p = p + 
      scale_x_continuous(paste('White'   , var), limits=lim, expand=c(0,0), breaks = breaks) +
      scale_y_continuous(paste('Minority', var), limits=lim, expand=c(0,0), breaks = breaks)
  }
  # save the graph
  if(!is.null(fn)){
  	ggsave(sprintf('%s/%s.pdf', figures_folder, fn), p, height=4, width=8, dpi=500)
  }
  # return the plot object
  return(p)
}
