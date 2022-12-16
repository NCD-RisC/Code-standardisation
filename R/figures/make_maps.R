library(tidyverse)
library(rgdal)
library(gridExtra)
library(grid)

date <- format(Sys.Date(), "%Y%m%d")

data <- readRDS("example_data.RDS")
countrylist <- unique(data %>% select(iso, Country))
world_map <- readRDS("world_map.RDS")

source("Map colour scales.R")
genders <- c("male" = "Men", "female" = "Women")

source("map_function.R")

# Plotting
caribbean <- c("ATG", "BHS", "BRB", "CUB", "DMA", "DOM", "GRD", "HTI", "JAM", "KNA", "LCA", "PRI", "TTO", "VCT")
insets <- c("ASM", "BHR", "BMU", "BRN", "COK", "COM", "CPV", "FJI", "FSM", "KIR", "MDV", "MHL", "MNE", "MUS", "NIU", "NRU", "PLW", "PYF", "SLB", "STP", "SYC", "TKL", "TON", "TUV", "VUT", "WSM")
inset_countries <- countrylist$Country[match(insets, countrylist$iso)]
inset_countries[which(inset_countries == "Cabo Verde")] <- "Cape Verde"
inset_countries[grep("Micronesia", inset_countries)] <- "Mirconesia, Federated States of"

maps <- list()
qt <- data$htn140
my_fill <- get_fill_scale("htn140", min(qt), max(qt), type = "levels") # to decide colour scale based on data

for (sx in c("female", "male")) {
  d_map <- data %>% subset(sex == sx)
  main_map <- map_function(d_map, "htn140", legend = TRUE, plot_title = genders[sx])
  caribbean_map <- map_function(
    d_map %>% subset(iso %in% caribbean), "htn140",
    world_map %>% subset(iso %in% caribbean)
  )
  inset_map <- map_function(d_map %>% subset(iso %in% insets), "htn140",
    map = NULL, not_inset = FALSE
  )
  maps[[sx]] <- arrangeGrob(main_map,
    arrangeGrob(caribbean_map, inset_map, nrow = 1, widths = c(1, 5)),
    ncol = 1, heights = c(5, 2)
  )
}

## Plot ##
pdf(paste0("Maps_", date, ".pdf"), height = 6, width = 20)
grid.arrange(
  textGrob("Hypertension", gp = gpar(cex = 1.3, fontface = "bold"), hjust = 0, x = unit(2, "cm")),
  arrangeGrob(maps[["female"]], maps[["male"]], nrow = 1),
  heights = c(5, 100)
)
dev.off()
