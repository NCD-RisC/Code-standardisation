library(ggplot2)
library(dplyr)
library(ggridges)
library(grid)

countrylist <- read.csv('../data/country-list-2023-new.csv')
source('../R/figures/region_sregion_palette.R')
source('../R/figures/map_function.R')
blank <- grid.rect(gp = gpar(col = NA, fill = NA))

world_map <- readRDS('../data/world_map.RDS')

data <- readRDS('../data/example_map_data.RDS')

# assign variable of interest to 'colour_val'
data$colour_val <- data$htn140
scale_range <- range(data$colour_val)
title <- c('female' = 'Women', 'male' = 'Men')

ps <- list()
for (sx in c('female','male')) ps[[sx]] <- map_function(data %>% filter(sex == sx & year == 2019),
                                                        world_map,
                                                        scale_range,
                                                        colour_scale = NULL,
                                                        type = 'level1',
                                                        plot_title = title[sx],
                                                        plot_type = 'standalone')

# "type" uses built-in colour schemes including:
# level1 (large value is bad), level2 (large value is good), change1 (increase is bad), change2 (increase is good), pp1 (large value for increase), pp2 (large value for decrease)
# 'change' in "type" name will force the colour scheme to be centred at a middle colour
# the default colour scheme for 'change' de-emphasise the small absolute changes: tune the behaviour by changing 'vls' vector in 'get_change_scale' function

# "plot_type" fine tunes the legend size
# 'composite' would make the legend titles smaller

# further tuning of the small-country circle sizes, legend sizes, density plot location and sizes and font sizes may be necessary depending on how large the plotting area is


pdf('example maps.pdf', height = 6, width = 7)
ggpubr::ggarrange(plotlist = ps, ncol = 1)
dev.off()

