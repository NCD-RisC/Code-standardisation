library(tidyverse)
library(grid)

date <- format(Sys.Date(), '%Y%m%d')

setwd("S:/Projects/HeightProject/Original dataset/Glucose/glucose_Model Runs/Figures/Hex map/")
read_crude <- TRUE
source('../utils/read data - both sexes.R')
source('S:/Projects/HeightProject/Templates/Code-standardisation/R/figures/region_sregion_palette.R')
countrylist <- read.csv('S:/Projects/HeightProject/Original dataset/Covariates/country-list-2023-new.csv')

# get number of studies
studies <- read.csv('S:/Projects/HeightProject/Original dataset/Glucose/glucose_Model Runs/prepare-data/model_ready_glucose_data_WITHOUT_new_CCDRFS-v2.csv') %>% 
  select(iso, id_study, survey_type) %>% 
  unique() %>% 
  group_by(iso) %>% 
  summarise(total = n(), national = sum(survey_type == 'National')) %>%
  mutate(text = paste0('(', total, ',', national,')'))

# change Oceania to Pacific island nations
names(sregion_col)[which(names(sregion_col) == 'Oceania')] <- 'Pacific island nations'
countrylist$Superregion[which(countrylist$Superregion == 'Oceania')] <- 'Pacific island nations'

mets <- c('Age-standardised')
met_labels <- c('Age-standardised' = 'Age-standardised\n','Crude' = '\nCrude ')

blank <- grid.rect(gp = gpar(col = NA, fill = NA))

# read country locations
d <- read.csv('../utils/hex-map-coordv6.csv')

d <- merge(d, countrylist)
d$island[is.na(d$island)] <- 0

# scale the coords so that x and y are equally sized
d$x <- d$x * 1.5
d$y <- d$y * sqrt(3)/2

# calculate hexagon corner coordinates
dxs <- c(-0.5, 0.5, 1, 0.5, -0.5, -1)
dys <- c(1, 1, 0, -1, -1, 0) * sqrt(3)/2
xs <- matrix(unlist(lapply(dxs, function(i) d$x + i)), ncol = 6)
ys <- matrix(unlist(lapply(dys, function(i) d$y + i)), ncol = 6)
xys <- cbind(xs, ys)
colnames(xys) <- paste0(rep(c('x', 'y'), each = 6), seq(1:6))
d <- cbind(d, xys)

# reshape
dy_plot <- d %>%
  select(iso, y1:y6) %>%
  gather(coord, y, y1:y6)
d_plot <- d %>%
  select(Superregion, Region, iso, island, x1:x6) %>%
  gather(coord, x, x1:x6) %>%
  select(-coord)
d_plot$y <- dy_plot$y

legs <- list()
annotates_lines <- list()
for (met in mets) {
  if (met == 'Crude') {
    testdata <- crude_18plus
  } else {
    testdata <- prevalence_18plus
  }
  met_label <- met_labels[met]

  # plot data
  # NAs in iso are global results
  testdata <- testdata %>%
    mutate(mean = prevalence * 100,
           l = l * 100,
           u = u * 100) %>%
    left_join(countrylist %>% rename(country = Country)) %>%
    select(-country, -prevalence) %>%
    filter(year >= 1990 & year <= 2022)

  # draw test data for each country and store in as a list in a tibble
  annotates_lines[[met]] <- d %>%
    # renaming necessary; otherwise filter statement inside mutate will confuse with variable name in other dataframe
    rename(iso0 = iso) %>%
    select(iso0, x, y) %>%
    rowwise() %>%
    mutate(grob = list(
      ggplotGrob(ggplot(data = testdata %>% filter(iso == iso0), aes(year, mean)) +
                   # thin horizontal line at zero
                   geom_hline(yintercept = 0, linewidth = 0.4, linetype = 'solid') +
                   # uncertainty
                   geom_ribbon(aes(ymin = l, ymax = u, fill = Superregion), colour = NA, alpha = 0.2) +
                   # trend lines
                   geom_line(aes(colour = Superregion), linewidth = 0.85) +
                   theme_classic() +
                   scale_colour_manual(values = sregion_col) +
                   scale_fill_manual(values = sregion_col) +
                   scale_x_continuous(limits = c(1990, 2022), breaks = seq(1990,2020,by=10), expand = c(0,0)) +
                   # range is manually decided - need to update for different data
                   scale_y_continuous(limits = c(0, 65), breaks = seq(0,60,by=10), expand = c(0,0)) +
                   theme(axis.title = element_blank(),
                         axis.text = element_blank(),
                         axis.line.x = element_line(colour = NA),
                         axis.line.y = element_line(linewidth = 0.4),
                         # axis.ticks.x = element_blank(),
                         axis.ticks.y = element_line(linewidth = 0.4, colour = 'black'),
                         axis.ticks.length = unit(0.75, "mm"),
                         panel.grid = element_blank(),
                         legend.position = 'none',
                         plot.background = element_blank(),
                         panel.background = element_blank(),
                         plot.margin = unit(c(0,0,0,0), "mm")) ))) %>%
    rename(iso = iso0) %>%
    suppressMessages()

  # side length of an inscribed square inside the hexagon
  # used to get locations for texts for ISO
  # 1.05 is a relaxation factor depending on preference
  a <- 1/(1+sqrt(3)/3) * 1.05

  # Define legends
  # legend for inset plot: axis titles, tick numbers
  # for (sx in sexes) {
    leg <- ggplotGrob(
      ggplot(testdata %>% filter(is.na(iso)), aes(year, mean)) + geom_blank() + theme_classic() +
        geom_hline(yintercept = 0, linewidth = 0.8, linetype = 'solid') +
        geom_ribbon(aes(ymin = l, ymax = u), fill = 'grey90', colour = NA) +
        geom_line(colour = 'black') +
        scale_x_continuous(name = '', limits = c(1990, 2022), breaks = seq(1990,2020,by=10), expand = c(0,0)) +
        scale_y_continuous(name = paste0(met_label, 'diabetes\nprevalence (%)'), limits = c(0, 65), breaks = seq(0,60,by=10), expand = c(0,0)) +
        theme(panel.grid = element_blank(),
              axis.line.x = element_line(colour = NA),
              axis.line.y = element_line(linewidth = 0.8),
              axis.ticks = element_line(linewidth = 0.8),
              axis.ticks.length.y = unit(1.2, "mm"),
              axis.text = element_text(size = 11, colour = 'black'),
              axis.text.x = element_text(angle = 45, hjust = 1),
              axis.title.y = element_text(size = 11, margin = margin(r = 8)),
              axis.title.x = element_blank()) +
        geom_text(data = testdata[1,], label = 'World', size = 4, hjust = 0.5, x = 2006, y = 60)
    )
    legs[[paste(met)]] <- leg
  # }
}

# legend for regional colour
reg_leg <- ggpubr::get_legend(
  ggplot(d, aes(x,y, colour = Superregion)) +
    geom_line() +
    scale_colour_manual(values = sregion_col) +
    guides(colour = guide_legend(ncol = 2, title = NULL, override.aes = list(linewidth = 1))) +
    theme(legend.key = element_rect(fill = NA),
          legend.key.width = unit(0.6, 'cm'),
          legend.key.spacing.x = unit(2, 'cm'),
          legend.key.spacing.y = unit(-0.15, 'cm'))
  )

# legend location (bottom left corner)
leg_loc <- c(5.1, 11.5)
regleg_loc <- c(2.5, -16.5)

for (met in mets) {
  pdf(paste0('Hex map - 18+ ', met, ' diabetes prevalence_bothsexes_',date,'.pdf'), height = 11, width = 17)

  # for (sx in sexes) {
    
    # add number of studies
    d$studies <- NA
    d$studies <- studies$text[match(d$iso, studies$iso)]
    d$studies[is.na(d$studies)] <- '(0,0)'
    
    p <- ggplot(d_plot, aes(x, y)) + theme_void() + coord_equal() + theme(legend.position = 'none') +
      annotate(geom = 'text', x = 2.5, y = 15.5, label = 'Both sexes', fontface = 'bold', hjust = 0, size = 5.5) +
      # defining plotting area
      scale_x_continuous(limits = c(2.1, 45.9), expand = c(0, 0)) +
      scale_y_continuous(limits = c(-13.3, 16.5), expand = c(0, 0)) +
      # add insets for each country - check function purrr::pmap
      pmap(annotates_lines[[met]],
           function(iso, grob, x, y) annotation_custom(
             grob = grob, xmin = x - a, xmax = x + a, ymin = y - a, ymax = y + a)) +
      # add legends
      annotation_custom(grob = legs[[paste(met)]],
                        xmin = leg_loc[1], xmax = leg_loc[1] + 7,
                        ymin = leg_loc[2] * sqrt(3)/2, ymax = (leg_loc[2] + 6) * sqrt(3)/2) +
      annotation_custom(grob = reg_leg,
                        xmin = regleg_loc[1] * 1.5, xmax = (regleg_loc[1] + 10) * 1.5,
                        ymin = regleg_loc[2] * sqrt(3)/2, ymax = (regleg_loc[2] + 7) * sqrt(3)/2) +
      # add country ISOs last so they are on top
      geom_text(data = d, aes(y = y + a, label = iso), colour = 'black', size = 2.8, vjust = 0.5) +
      # add number of studies
      geom_text(data = d, aes(y = y + a/1.9, label = studies), colour = 'black', size = 2.6, vjust = 0.5)

    plot(p)
  # }
  dev.off()

}
