library(tidyverse)
library(scatterpie)

source('../R/figures/bubble_plot.R')

countrylist <- read.csv("../data/country-list-2023-new.csv")

meta <- read.csv("../data/example_metadata.csv")

start_year <- 1980

d <- meta %>%
    left_join(countrylist %>% select(iso, Region)) %>%
    select(id_study,mid_year,survey_type,Region) %>%
    mutate(mid_year = ifelse(mid_year>=start_year-3 & mid_year<start_year & survey_type == 'National', start_year, mid_year),
           mid_year = ifelse(mid_year==2023, 2022, mid_year))

pdf('example bubble plot.pdf', width = 12, height = 5.5)
# bubble_plot(d, sizing_f = 1.4, legend_sizes = c(1,10,15), year_range = c(1990,2022))
bubble_plot(d, legend_sizes = c(1,10,15))
dev.off()
