#' Bubble plot of number of studies by region, year and type
#'
#' @param meta data frame of study metadata; must have columns: Region, mid_year, survey_type, id_study
#' @param sizing_f control the size of the pies: default = 1.1
#' @param legend_sizes the values for the pies in the legend for size: default = c(1,10,20)
#'
#' @return ggplot object
#' @export
#'
#' @examples
#' bubble_plot(glucose_meta)
#' bubble_plot(anthro_meta, sizing_f = 0.6, max_leg_size = 25) # eg because there are more studies

bubble_plot <- function(meta, sizing_f = 1.2, legend_sizes = c(1,10,20)) {
    # set region orders
    region_order <- rev(c("Eastern Europe", "Central Europe",
                          "Southwestern Europe", "Northwestern Europe", "High-income English-speaking countries",
                          "Southern Latin America", "Central Latin America", "Andean Latin America", "The Caribbean",
                          "Polynesia and Micronesia", "Melanesia",
                          "East Asia and the Pacific", "Southeast Asia",
                          "South Asia",
                          "Central Asia", "Middle East and north Africa",
                          "East Africa", "Central and southern Africa", "West Africa", "Other sub-Saharan Africa"))
    meta$Region <- factor(meta$Region, levels = region_order)

    # count number studies
    met <- meta %>%
        group_by(Region, mid_year, survey_type) %>%
        summarise(value = n()) %>%
        ungroup() %>%
        spread(survey_type, value) %>%
        mutate(
            Community = ifelse(is.na(Community), 0, Community),
            Subnational = ifelse(is.na(Subnational), 0, Subnational),
            National = ifelse(is.na(National), 0, National)
        ) %>%
        left_join(
            tot <- meta %>%
                group_by(Region, mid_year) %>%
                summarise(total = n()) %>%
                ungroup()
        ) %>%
        suppressMessages()

    # legend position
    leg_pos <- -1.5
    leg_texts <- data.frame(
        x = start_year + c(0, 2.5, 5, 11, 16, 21) + 5,
        y = leg_pos,
        label = c(as.character(legend_sizes), 'Community', 'Subnational', 'National')
    )

    # colour scheme
    cols <- c("National" = "#0085FF", "Subnational" = "#FFD600", "Community" = "#FF0099")

    p <- ggplot() + theme_bw() +
        geom_scatterpie(data = met,
                        aes(x = mid_year, y = as.numeric(Region), r = sqrt(total/100) * sizing_f),
                        cols = c('National','Subnational','Community'), colour = NA) +
        scale_y_continuous(breaks = 1:length(region_order),
                           expand = c(0,0),
                           labels = region_order, name = NULL,
                           minor_breaks = c()) +
        scale_x_continuous(minor_breaks = seq(start_year, end_year),
                           expand = c(0,0)) +
        scale_fill_manual(values = cols) +
        theme(axis.title.x = element_blank(),
              text = element_text(size = 12),
              legend.position = 'nones',
              legend.title = element_blank(),
              panel.grid = element_line(colour = grey(0.9), linewidth = 0.5)) +
        coord_equal(clip = 'off',
                    xlim = c(start_year - 0.5, end_year + 0.5),
                    ylim = c(0.5, 20.5)) +
        # add legends - pies: using data frame did not work so had to do individually
        geom_scatterpie(data = met[1,], aes(x = leg_texts$x[1], y = leg_pos, r = sqrt(legend_sizes[1]/100) * sizing_f), cols = 'total', fill = 'black', colour = NA) +
        geom_scatterpie(data = met[1,], aes(x = leg_texts$x[2], y = leg_pos, r = sqrt(legend_sizes[2]/100) * sizing_f), cols = 'total', fill = 'black', colour = NA) +
        geom_scatterpie(data = met[1,], aes(x = leg_texts$x[3], y = leg_pos, r = sqrt(legend_sizes[3]/100) * sizing_f), cols = 'total', fill = 'black', colour = NA) +
        geom_scatterpie(data = met[1,], aes(x = leg_texts$x[4], y = leg_pos, r = sqrt(10/100) * sizing_f), cols = 'total', fill = cols['Community'], colour = NA) +
        geom_scatterpie(data = met[1,], aes(x = leg_texts$x[5], y = leg_pos, r = sqrt(10/100) * sizing_f), cols = 'total', fill = cols['Subnational'], colour = NA) +
        geom_scatterpie(data = met[1,], aes(x = leg_texts$x[6], y = leg_pos, r = sqrt(10/100) * sizing_f), cols = 'total', fill = cols['National'], colour = NA) +
        # add legends - texts
        geom_text(data = leg_texts, aes(x = x + 0.8, y = leg_pos, label = label), hjust = 0, size = 3)

    return(p)
}