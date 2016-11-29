faceted_heatmap <- function(df, title) {
# Takes in a dataframe and returns a plot object containing heatmaps
# faceted by crime type.
#
# Args:
#   df: dataframe to plot heatmaps for
#   title: Title to be used for the graph object generated
# Returns:
#   g: Graph object to be plotted

  sf.long <- c(-122.55, -122.355)
  sf.lat <- c(37.705, 37.84)
  sf_map <- suppressMessages(get_map(location = c(mean(sf.long), mean(sf.lat)), zoom = 12, maptype = "roadmap"))
  g <- ggmap(sf_map) + stat_density2d(data = df, aes(x = X, y = Y,fill = ..level.., alpha = ..level..), size = .01, bins = 16, geom = 'polygon', show.legend = FALSE) +
    scale_fill_gradient(low = "green", high = "red") + labs(x = 'Longitude', y = 'Latitude') + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    facet_wrap(~Category) + ggtitle(title)
  return(g)
}
