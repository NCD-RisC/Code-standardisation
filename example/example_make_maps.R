library(ggplot2)
library(dplyr)
library(ggridges)
library(grid)

countrylist <- read.csv('../data/country-list-2023-new.csv')
source('../R/figures/region_sregion_palette.R')
source('../R/figures/map_function.R')
blank <- grid.rect(gp = gpar(col = NA, fill = NA))

world_map <- readRDS('../data/world_map.RDS')


### Example of a continuous map ###
data <- readRDS('../data/example_map_data.RDS')

# assign variable of interest to 'colour_val'
data$colour_val <- data$htn140
title <- c('female' = 'Women', 'male' = 'Men')

print(range(data$colour_val))
scale_breaks <- c(min(data$colour_val),0.2,0.35,0.4,0.45,0.5,0.55,0.6,max(data$colour_val))   # does not need to be uniform

ps <- list()
for (sx in c('female','male')) ps[[sx]] <- map_function(data %>% filter(sex == sx & year == 2019),
                                                        world_map,
                                                        scale_breaks,
                                                        type = 'level1',
                                                        plot_title = title[sx],
                                                        plot_type = 'standard')

# "type" uses built-in colour schemes including:
# level1 (large value is bad), level2 (large value is good), change1 (increase is bad), change2 (increase is good), pp1 (large value for increase), pp2 (large value for decrease)
# 'change' in "type" name will force the colour scheme to be centred at a middle colour
# the default colour scheme for 'change' de-emphasise the small absolute changes: tune the behaviour by changing 'vls' vector in 'get_change_scale' function

# "plot_type" fine tunes the legend size
# 'composite' would make the legend titles smaller

# further tuning of the small-country circle sizes, legend sizes, density plot location and sizes and font sizes may be necessary depending on how large the plotting area is


pdf('example continuous maps.pdf', height = 6, width = 7)
ggpubr::ggarrange(plotlist = ps, ncol = 1)
dev.off()



### Example of a categorical map ###
meta <- read.csv('../data/example_metadata.csv')
data <- data.frame(table(meta$iso))
names(data) <- c('iso','colour_val')

# add zeros
data <- data %>% bind_rows(tibble(iso = setdiff(unique(countrylist$iso), data$iso), colour_val = 0))

maxqt <- max(data$colour_val)

# categories should start from zero and have length = 9 to use the default colour scheme
p <- map_function_categorical(data, world_map, type = 'source_number', scale_breaks = c(0, 1, 2, 5, 10, 20, 30, 40, maxqt+1)) %>% suppressMessages()

pdf('example data source map.pdf', height = 3.2, width = 8)
p
dev.off()
