# main plot function
# the map has a main map, a Caribbean map and ~25 inset countries plotted as squares
map_function <- function(data_map, fill_name, map = world_map, not_inset = TRUE, legend = FALSE, plot_title = NA) {
  data_map$mean <- data_map[, fill_name]
  if (not_inset) {
    # plot main map and Caribbean map
    dataset <- merge(map, data_map, all = TRUE) %>% arrange(order)
    p <- ggplot(dataset, aes(x = long, y = lat, group = group, fill = mean)) +
      geom_polygon() +
      geom_path(size = 0.1, colour = "black") +
      coord_equal() +
      theme_void() +
      my_fill
    if (length(unique(data_map$iso)) == 200) {
      p <- p + ggtitle(paste(genders[unique(data_map$sex)], unique(data_map$year))) +
        theme(plot.title = element_text(hjust = 0.5))
    }

    if (!is.na(plot_title)) p <- p + ggtitle(plot_title)

    if (legend) {
      p <- p + theme(
        legend.background = element_rect(fill = "white", colour = NA),
        legend.position = c(0.06, 0.1),
        legend.justification = c("left", "bottom"),
        legend.title = element_blank()
      )
    } else {
      p <- p + theme(legend.position = "none")
    }
  } else {
    # plot inset countries as squares
    ncols <- 4
    nrows <- ceiling(length(inset_countries) / ncols)
    dx <- 11
    alpha <- 0.6
    marg <- 0.2
    data.inset <- data.frame(iso = insets, country = inset_countries) %>%
      arrange(country) %>%
      mutate(i = 1:length(inset_countries) - 1)
    data.inset$i <- ifelse(data.inset$i >= 20, data.inset$i + 1, data.inset$i)
    data.inset$x <- floor(data.inset$i / nrows) * dx
    data.inset$y <- nrows - data.inset$i %% nrows
    data.inset$colour <- data_map$mean[match(data.inset$iso, data_map$iso)]
    p <- ggplot(data.inset, aes(fill = colour)) +
      geom_rect(aes(xmin = x, xmax = x + alpha, ymin = y, ymax = y + alpha), colour = "black", size = 0.2) +
      geom_text(aes(x = x + alpha + marg, y = y + 0.5 * alpha, label = country), hjust = 0) +
      xlim(c(0, max(data.inset$x) + dx * 0.6)) +
      my_fill +
      coord_equal() +
      theme_void() +
      theme(legend.position = "none")
  }
  return(p)
}
