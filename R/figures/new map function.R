library(tidyverse)
library(ggridges)
library(scales)

# 'Mapping function
#'
#' plot a map with density and small countries as dots inside the map
#' @param data_map The data frame with the value to colour the map in variable 'colour_val'
#' @param world_map The shape file for map
#' @param scale_range The min and max of values for the colouring variable, decided in function if NULL
#' @param colour_scale Colour scheme used for plotting, decided in function if NULL
#' @param type The type of variable being plotted, which decides the colour scale (unless provided) and formatting of legend, title and inset density chart
#' @param plot_title Add plot title if provided
#' @param plot_type default 'standalone'; if 'composite' then font sizes are made larger
#' @return A ggplot grob object: can be used in egg/ggpubr ggarrange
#' @examples
#' p <- map_function(data %>% filter(sex == 'Women' & year = 2022),
#'                   world_map,
#'                   c(min(data$colour_val[data$year == 2022]), max(data$colour_val[data$year == 2022])),
#'                   plot_title = 'Women 2022')
#' @export

map_function <- function(data_map, world_map, scale_range = NULL, colour_scale = NULL, type = 'level1', plot_title = NULL, plot_type = 'standalone') {

    # remove two islands in the Indian Ocean that clash with density plot
    world_map <- world_map %>% filter(!iso %in% c('ATF','HMD'))

    if (is.null(scale_range)) {
        minqt <- min(data_map$colour_val)
        maxqt <- max(data_map$colour_val)
    } else {
        minqt <- scale_range[1]
        maxqt <- scale_range[2]
    }

    ## embedded colour scales; or provide bespoke scales
    if (is.null(colour_scale)) {
        colour_ramps <- switch(
            type,

            level1 = c("#240A00", "#4A1400", "#8B1A1A", "#CD3700", "#E69667", "#FFF5CF", "#fffae8"),

            level2 = c("#040029", "#30286A", "#43459C", "#3C58BF", "#2E6CD4", "#3183E4", "#439DEE", "#7ABBEB", "#B9D9E4", "#FEF7DB", "#fffae8"),

            change1 = c("#002E12", "#277D4D", "#64BC45", "#9CEA4D", "#C3FF8D", "#DBFEDF",
                        "#FFFDF5",
                        "#FCD7C7", "#F59069", "#DE481A", "#B51B00", "#830001", "#54000B"),

            change2 = c("#54000B", "#830001", "#BA0000", "#A71E15", "#B53A15", "#FBD7C5",
                        "#FFFDF5",
                        "#CFFEB3", "#6CD354", "#36A165", "#2A8B47", "#1D7D1F", "#002E12"),

            pp1 = c("#456A76", "#70A1B0", "#96D0E1", "#C2E6F0", "#E3F0F1",
                    "#FCF8F3",
                    "#FAF2D7", "#FAEEB7", "#F7DE70", "#D6B732", "#796409"),

            pp2 = rev(c("#456A76", "#70A1B0", "#96D0E1", "#C2E6F0", "#E3F0F1",
                        "#FCF8F3",
                        "#FAF2D7", "#FAEEB7", "#F7DE70", "#D6B732", "#796409"))
        )
        colour_ramps <- colorRampPalette(colour_ramps)
    }

    if (grepl('change', type)) {
        label_func <- function(x) x * 100
        scales <- get_change_scale(type, colour_ramps, minqt, maxqt, label_func)
        my_fill <- scales[[1]]
        my_colour <- scales[[2]]
    } else {
        if (grepl('pp', type)) {
            vls <- c(0, 0.05, 0.1, 0.2, 0.25, 0.5, 0.75, 0.8, 0.9, 0.95, 1)  # needs to be of odd length
            colour_scale <- colour_ramps(length(vls))
            minqt <- 0
            maxqt <- 1
            label_func <- waiver()
        } else {
            colour_scale <- rev(colour_ramps(24))
            vls <- NULL
            label_func <- percent
        }

        my_fill <- scale_fill_gradientn(
            limits = c(minqt, maxqt),
            labels = label_func,
            colours = colour_scale,
            values = vls,
            na.value = "grey"
        )
        my_colour <- scale_colour_gradientn(
            limits = c(minqt, maxqt),
            labels = label_func,
            colours = colour_scale,
            values = vls,
            na.value = "grey"
        )
    }

    # get data for countries as dots
    dots <- c("BMU", "COM", "CPV", "MDV", "MUS", "STP", "SYC", "BHS")
    pac1 <- c("FJI", "FSM", "TUV", "MHL", "VUT", "SLB", "NRU", "PLW")
    pac2 <- c("WSM", "TKL", "ASM", "KIR", "TON", "NIU",  "COK", "PYF")
    dots <- c(dots, pac1, pac2)
    dot_map <- world_map %>%
        filter((iso %in% setdiff(dots,'KIR') & grepl('\\.1$', group)) |   # keep the first island when multiple exist
               (group == 'KIR.5')) %>%   # KIR.5 is the main island for Kiribati, which appear on the other side of the map
        group_by(iso) %>%
        summarise(long = mean(long),
                  lat = mean(lat))
    # move countries so that they don't overlap
    dot_map$long[dot_map$iso == "ASM"] <- -169
    dot_map$long[dot_map$iso == "WSM"] <- -173
    dot_map$long[dot_map$iso == "BHS"] <- -77

    # get data for Caribbean islands as dots and line
    caribbeans <- c("ATG", "BRB", "DMA", "GRD", "KNA", "LCA", "TTO", "VCT")
    cari_line_start <- world_map %>%
        filter(iso %in% caribbeans & grepl('\\.1$', group)) %>%
        group_by(iso) %>%
        summarise(long = mean(long),
                  lat = mean(lat)) %>%
        arrange(-lat)
    centre_long <- -65
    centre_lat <- 15
    radius <- 18
    angle_start <- 15
    angle_end <- 115
    angles <- seq(angle_start, angle_end, length.out = length(caribbeans)) - 85
    longs <- centre_long + radius * cos(angles/180*pi)
    lats <- centre_lat - radius * sin(angles/180*pi)

    cari_map <- data.frame(iso = cari_line_start$iso, long = longs, lat = lats)
    cari_line <- cari_map %>% rename(long_end = long, lat_end = lat) %>% left_join(cari_line_start)

    # other line countries
    oth <- c("MNE", "BRN", "BHR")
    oth_line_start <- world_map %>%
        filter(iso %in% oth & grepl('\\.1$', group)) %>%
        group_by(iso) %>%
        summarise(long = mean(long),
                  lat = mean(lat))
    longs <- c(19, 112, 57)
    lats <- c(35, 8, 15)
    oth_map <- data.frame(iso = oth, long = longs, lat = lats)
    oth_line <- oth_map %>% rename(long_end = long, lat_end = lat) %>% left_join(oth_line_start)


    # plot map with dots
    dataset <- merge(world_map, data_map, all = TRUE) %>% arrange(order)
    p <- ggplot(dataset, aes(x = long, y = lat, text = iso)) +
        geom_polygon(aes(group = group, fill = colour_val)) +
        geom_path(aes(group = group), linewidth = 0.01, colour = "black") +
        geom_segment(data = bind_rows(cari_line, oth_line) %>% left_join(data_map),
                     aes(xend = long_end, yend = lat_end, group = iso), linewidth = 0.1) +
        geom_point(data = bind_rows(dot_map, cari_map, oth_map) %>% left_join(data_map),
                   aes(fill = colour_val), shape = 21, size = 1.8, colour = 'black', stroke = 0.001) +
        coord_equal(clip = 'off') +
        theme_void() +
        ylim(c(-56.5,84)) +
        my_fill +
        theme(
              legend.justification = c("left", "bottom"),
              legend.frame = element_rect(colour = 'black', linewidth = 0.2),
              legend.ticks = element_line(colour = 'black', linewidth = 0.2),
              legend.margin = margin(t = 0),
              legend.spacing.x = unit(0, 'cm'),
              legend.position = 'inside'
              )

    if (!is.null(plot_title)) p <- p + ggtitle(plot_title)

    density <- ggplot(data_map, aes(x = colour_val, y = 2, fill = after_stat(x))) +
        geom_density_ridges_gradient(colour="black", linewidth=0.1) +
        my_fill + theme_minimal() +
        scale_x_continuous(expand = c(0, 0), labels = label_func, limits = c(minqt, maxqt)) +
        scale_y_continuous(expand = expansion(mult = c(0, 0), add = c(0, 0.01))) +
        theme(legend.position = "none",
              plot.margin = margin(),
              panel.grid = element_blank(),
              axis.line.x = element_line(colour = 'black', linewidth = 0.1),
              axis.ticks.x = element_line(linewidth = 0.2),
              axis.text.y = element_blank(), axis.title = element_blank())

    if (plot_type == 'composite') {
        # larger font sizes when maps are used as part of a composite figure
        p <- p +
            guides(fill = guide_colourbar(barheight = 4.5, barwidth = 0.9, title = 'Percentage\npoint change')) +
            theme(
                plot.title = element_text(size = 12),
                legend.ticks.length = unit(0.06, 'cm'),
                legend.text = element_text(size = 10)
            )

        if (grepl('change',type)) {
            p <- p + theme(legend.title = element_text(size = 10),
                           legend.position.inside = c(0.15, 0.05))
        } else {
            p <- p + theme(legend.title = element_blank(),
                           legend.position.inside = c(0.18, 0.07))
        }

        density <- density + theme(
            axis.ticks.length.x = unit(0.06, 'cm'),
            axis.text.x = element_text(size = 10, colour = 'black')
        )

        # location and size of the density plot
        loc <- c(50,120,-65,-15)

    } else {
        p <- p +
            guides(fill = guide_colourbar(barheight = 3, barwidth = 0.6, title = 'Percentage\npoint change')) +
            theme(
                plot.title = element_text(size = 8),
                legend.ticks.length = unit(0.05, 'cm'),
                legend.text = element_text(size = 5)
            )

        if (grepl('change',type)) {
            p <- p + theme(legend.title = element_text(size = 5),
                           legend.position.inside = c(0.17, 0.1))
        } else {
            p <- p + theme(legend.title = element_blank(),
                           legend.position.inside = c(0.2, 0.1))
        }

        density <- density + theme(
            axis.ticks.length.x = unit(0.05, 'cm'),
            axis.text.x = element_text(size = 5, colour = 'black')
        )

        # location and size of the density plot
        loc <- c(55,115,-62,-12)
    }


    # add density plot in Indian Ocean
    if (grepl('pp', type)) {
        loc[1] <- loc[1] - 10
        loc[2] <- loc[2] - 10
    }
    p <- p + annotation_custom(ggplotGrob(density), xmin = loc[1],xmax = loc[2],ymin = loc[3],ymax = loc[4])

    return(p)
}


get_change_scale <- function(var, colour_ramps, minqt, maxqt, label_func) {
    n <- 2001
    if (grepl('level1', var)) {
        a = 0.45
        x1  <- 1-rev(rescale(8^seq(.1,.9,length=100*a)))
        x2  <- rescale(8^seq(.1,.9,length=100))[-1]
        vls0 <- c(x1*a, a+x2)
        vls0 <- c(vls0, 1+a + 1+a-rev(vls0)[-1])
        pt_col <- colour_ramps(n)
    } else {
        a = 0.45
        x1  <- 1-rev(rescale(8^seq(.1,.9,length=100*a)))
        x2  <- rescale(8^seq(.1,.9,length=100))[-1]
        vls0 <- c(x1*a, a+x2)
        vls0 <- c(vls0, 1+a + 1+a-rev(vls0)[-1])
        pt_col <- colour_ramps(n)
    }

    if (maxqt > abs(minqt)) {
        highlim <- (n-1)/2
        lowlim  <- ceiling((n-1)/2*(abs(minqt)/maxqt))
    } else {
        lowlim <- (n-1)/2
        highlim  <- ceiling((n-1)/2*(maxqt/abs(minqt)))
    }

    vls <- approx(vls0, n=n)$y

    if (minqt < 0) {
        upperpart <- seq((n-1)/2, (n-1)/2 + highlim)
        lowerpart <- seq((n-1)/2 - lowlim, (n-1)/2)
        pt_col  <- pt_col[c(lowerpart, upperpart[-1])]

        vls1    <- rescale(vls[lowerpart]) * (lowlim / (highlim + lowlim - 1))
        vls2    <- rescale(vls[upperpart]) * (highlim / (highlim + lowlim - 1)) + (lowlim / (highlim + lowlim - 1))
        vls     <- rescale(c(vls1, vls2[-1]))
    } else {
        # use a slightly off white colour (minqt = 0.01) for the low limit if the low limit is larger than zero (in the case of diagnosis)
        lowlim  <- ceiling((n-1)/2*(0.01/maxqt))
        pt_col  <- pt_col[seq((n-1)/2 + lowlim, (n-1)/2 + highlim)]
        vls     <- rescale(vls[seq((n-1)/2 + lowlim, (n-1)/2 + highlim)])
    }

    fill = scale_fill_gradientn(
        limits  = c(minqt, maxqt),
        labels  = label_func,
        colours = pt_col,
        values  = vls,
        na.value = "grey")
    colour = scale_colour_gradientn(
        limits  = c(minqt, maxqt),
        labels  = label_func,
        colours = pt_col,
        values  = vls,
        na.value = "grey")

    return(list(fill = fill, colour = colour))
}
