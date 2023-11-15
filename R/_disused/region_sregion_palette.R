library(RColorBrewer)

add.alpha <- function(col, alpha=1){
    if(missing(col))
        stop("Please provide a vector of colours.")
    apply(sapply(col, col2rgb)/255, 2,
          function(x)
              rgb(x[1], x[2], x[3], alpha=alpha))
}

## Regional palette ##
palette(brewer.pal(12,"Paired"))
palette(c(palette()[c(2,6,8,10,1,4,12,7,5)]));#dev.off()
palette(c(palette()[c(6,6,9,2,1,6,9,3,1,7,5,4,2,5,4,8,3,9,6,5,9)]))
colours     <- palette()
colours[1]  <- add.alpha(colours[1],  0.6)    # [1] "Andean Latin America"
colours[2]  <- add.alpha(colours[2],  0.4)    # [2] "Caribbean"
colours[6]  <- add.alpha(colours[6],  0.8)    # [6] "Central Latin America"
colours[11] <- add.alpha(colours[11], 0.6)    #[11] "High-income English-speaking countries"
colours[14] <- add.alpha(colours[14], 0.8)    #[14] "North Western Europe"
colours[5]  <- add.alpha(colours[5],  0.8)    # [5] "Central Europe"
colours[17] <- add.alpha("#FF8C00",   0.8)    #[17] "Southeast Asia"
colours[13] <- add.alpha(colours[13], 0.8)    #[13] "North Africa and Middle East"
colours[3]  <- add.alpha(colours[3],  0.6)    # [3] "Central and Southern Africa"
colours[21] <- add.alpha(colours[21], 0.4)    #[21] "West Africa"
colours[18] <- add.alpha(colours[18], 0.8)    #[18] "Southern Africa"
colours[12] <- add.alpha(colours[12], 0.8)    #[12] "Melanesia"

# translate colour with transparency to opaque equivalent
original_col <- col2rgb(colours, alpha=TRUE)
alpha        <- matrix(rep(original_col[4,]/255,3),nrow=3, byrow=TRUE)
opac_col     <- original_col[-4,]*alpha + (1-alpha)*255
region_col   <- rgb(opac_col[1,],opac_col[2,],opac_col[3,], maxColorValue=255)
# names(region_col) <- c("Andean Latin America","Caribbean","Central Africa","Central Asia","Central Europe","Central Latin America","East Africa","East Asia","Eastern Europe","High-income Asia Pacific","High-income English-speaking countries","Melanesia","Middle East and North Africa","North Western Europe","Polynesia and Micronesia","South Asia","South East Asia","Southern Africa","Southern and Latin America","South Western Europe","West Africa")
names(region_col) <- c("Andean Latin America","Caribbean","Central and Southern Africa","Central Asia","Central Europe","Central Latin America","East Africa","East Asia and the Pacific","Eastern Europe","South Asia", "High-income English-speaking countries","Melanesia","Middle East and North Africa","North Western Europe","Polynesia and Micronesia","South Asia","Southeast Asia","Other sub-Saharan Africa","Southern Latin America","South Western Europe","West Africa")


## Super-regional palette ##
palette(brewer.pal(12,"Paired"))
sregion_col <- c(palette()[c(2,6,8,12,1,4,10,5)])
names(sregion_col) <- c("Central and Eastern Europe","Central Asia, Middle East and North Africa","Southeast Asia, East Asia and the Pacific","South Asia","High-income western","Latin America and Caribbean","Oceania","Sub-Saharan Africa")

.region_order <- c("High-income English-speaking countries",
                      "North Western Europe",
                      "South Western Europe",
                      "Central Europe",
                      "Eastern Europe",
                      "Southern Latin America",
                      "Central Latin America",
                      "Andean Latin America",
                      "Caribbean",
                      "East Asia and the Pacific",
                      "Southeast Asia",
                      "South Asia",
                      "Central Asia",
                      "Middle East and North Africa",
                      "Polynesia and Micronesia",
                      "Melanesia",
                      "East Africa",
                      "Other sub-Saharan Africa",
                      "Central and Southern Africa",
                      "West Africa")

.sregion_order <- c("High-income western",
                   "Central and Eastern Europe",
                   "Latin America and Caribbean",
                   "Southeast Asia, East Asia and the Pacific",
                   "South Asia",
                   "Central Asia, Middle East and North Africa",
                   "Oceania",
                   "Sub-Saharan Africa")
