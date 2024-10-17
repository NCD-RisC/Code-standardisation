#' Bubble plot of number of studies by region, year and type
#'
#' @param meta data frame of study metadata; must have columns: Region, mid_year, survey_type, id_study
#' @param sizing_f control the size of the pies: default = 1.1
#' @param legend_sizes the values for the pies in the legend for size: default = c(1,10,20)
#' @param year_range the range of year to be plotted: default = NULL, i.e. plot all years present in meta
#'
#' @return ggplot object
#' @export
#'
#' @examples
#' bubble_plot(glucose_meta)
#' bubble_plot(glucose_meta, year_range = c(1990, NA))
#' bubble_plot(glucose_meta, year_range = c(1990, 2020))
#' bubble_plot(anthro_meta, sizing_f = 0.6, max_leg_size = 25) # eg because there are more studies

bubble_plot <- function(meta, sizing_f = 1.2, legend_sizes = c(1,10,20), year_range = NULL) {

    if (is.null(year_range)) {
        start_year <- min(meta$mid_year)
        end_year <- max(meta$mid_year)
    } else {
        start_year <- year_range[1]
        end_year <- year_range[2]
        if (is.na(start_year)) start_year <- min(meta$mid_year)
        if (is.na(end_year)) end_year <- max(meta$mid_year)
    }

    # set region orders
    region_order <- rev(c("High-income English-speaking countries", "Northwestern Europe", "Southwestern Europe",
                          "Central Europe", "Eastern Europe",
                          "Southern Latin America", "Central Latin America", "Andean Latin America", "The Caribbean",
                          "East Asia and the Pacific", "Southeast Asia",
                          "South Asia",
                          "Central Asia", "Middle East and north Africa",
                          "Polynesia and Micronesia", "Melanesia",
                          "East Africa", "West Africa", "Central and southern Africa", "Other sub-Saharan Africa"))
    meta$Region <- factor(meta$Region, levels = region_order)

    # count number studies
    met <- meta %>%
        filter(mid_year >= start_year & mid_year <= end_year) %>%
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
        mutate(Region_x = as.numeric(Region)) %>%
        suppressMessages()

    # legend position
    leg_pos <- -1.5
    leg_texts0 <- data.frame(
        x = start_year + c(0, 2.5, 5, 11, 16, 21) + 5,
        y = leg_pos,
        label = c(as.character(legend_sizes), 'Community', 'Subnational', 'National'),
        r = c(legend_sizes, rep(legend_sizes[2], 3))
    )
    leg_texts <- leg_texts0 %>% spread(label, r)
    leg_texts[is.na(leg_texts)] <- 0
    leg_texts$total <- leg_texts0$r

    # colour scheme
    cols <- c("National" = "#0085FF", "Subnational" = "#FFD600", "Community" = "#FF0099",
              rep('black', 3))
    names(cols)[4:6] <- as.character(legend_sizes)

    p <- ggplot() + theme_bw() +
        geom_scatterpie(data = met,
                        aes(x = mid_year, y = Region_x, r = sqrt(total/100) * sizing_f),
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
              axis.text = element_text(colour = 'black'),
              legend.position = 'nones',
              legend.title = element_blank(),
              panel.grid = element_line(colour = grey(0.9), linewidth = 0.5)) +
        coord_equal(clip = 'off',
                    xlim = c(start_year - 0.5, end_year + 0.5),
                    ylim = c(0.5, 20.5)) +
        # add legends - pies
        # using the same function 'geom_scatterpie' so that the sizes match the plot
        geom_scatterpie(data = leg_texts, aes(x = x, y = y, r = sqrt(total/100) * sizing_f), cols = c('National','Subnational','Community',as.character(legend_sizes)), colour = NA) +
        # add legends - texts
        geom_text(data = leg_texts0, aes(x = x + 0.8, y = leg_pos, label = label), hjust = 0, size = 3)

    return(p)
}
