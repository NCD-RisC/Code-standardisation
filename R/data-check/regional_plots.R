## Regional plot by year
regional_plot_year <- function(df, group_var, age_low = NULL, age_high = NULL,
                               my_xlim = NULL, my_ylim = NULL,
                               file_name) {

  # Make sure that all required standardised variables are included
  std_vars <- c("group_var", "mean_var", "year_var", "se_var")
  if (!all(std_vars %in% colnames(df))) {
    stop("Some of the std variables (group_var, mean_var, year_var, se_var) are missing ")
  }

  # Get region names
  sregions <- as.character(sort(unique(df$group_var)))

  # Define age ranges
  if (is.null(age_low) | is.null(age_high)) {
    if (max(df$age) > 20) {
      age_low <- c(20, 30, 40, 50, 60)
      age_high <- c(30, 40, 50, 60, 70)
    } else {
      age_low <- c(5, 6, 7, 8, 9, 10, 12, 14, 16) ## Ages to use in plots
      age_high <- c(6, 7, 8, 9, 10, 12, 14, 16, 19)
    }
  }

  # Define axes limits
  if (is.null(my_xlim)) {
    my_xlim <- range(data$year_var)
  }
  if (is.null(my_ylim)) {
    my_ylim <- range(na.omit(c(data$mean_var - 2 * data$se_var, data$mean_var + 2 * data$se_var)))
  }

  # Initialize plot
  pdf(file_name)


  for (j in 1:length(sregions)) {
    print(sregions[j])
    par(mfrow = c(3, 3))
    for (i in 1:length(age_low)) {
      dat_plot <- subset(df, age >= age_low[i] & age < age_high[i] & df[, "group_var"] == sregions[j])
      coverage_col <- rep(0, nrow(dat_plot))
      coverage_col[dat_plot$survey_type == "National"] <- rainbow(50, alpha = 0.5)[30]
      coverage_col[dat_plot$survey_type == "Subnational"] <- rainbow(50, alpha = 0.5)[8]
      coverage_col[dat_plot$survey_type == "Community"] <- rainbow(50, alpha = 0.5)[46]
      scope.pch <- rep(16, nrow(dat_plot))
      scope.pch[dat_plot$urban_rural == "rural"] <- 15
      scope.pch[dat_plot$urban_rural == "urban"] <- 17

      plot(dat_plot$year_var, dat_plot[, "mean_var"],
        pch = scope.pch, col = coverage_col,
        xlim = my_xlim,
        ylim = my_ylim,
        cex = .6, main = paste(sregions[j], "\n",
          age_low[i], " <= year < ", age_high[i],
          sep = ""
        ), xlab = "", ylab = "", cex.main = .8
      )

      for (i in 1:nrow(dat_plot)) {
        lines(c(dat_plot$year_var[i], dat_plot$year_var[i]), c(
          (dat_plot[, "mean_var"][i] - 2 * dat_plot[, "se_var"][i]),
          (dat_plot[, "mean_var"][i] + 2 * dat_plot[, "se_var"][i])
        ), col = coverage_col[i])
      }
    }
  }

  dev.off()
}

## Regional plot by age
regional_plot_age <- function(df, group_var, year_low = NULL, year_high = NULL, my_xlim = c(5, 19),
                              my_ylim = NULL, file_name) {

  # Make sure that all required standardised variables are included
  std_vars <- c("group_var", "mean_var", "year_var", "se_var")
  if (!all(std_vars %in% colnames(data))) {
    stop("Some of the std variables (group_var, mean_var, year_var, se_var) are missing ")
  }

  # Define year ranges
  if (is.null(year_low) | is.null(year_high)) {
    vals <- round(seq(min(data$year_var), max(data$year_var), length = 5))
    year_low <- vals[seq(1, length(vals) - 1, 1)]
    year_high <- vals[seq(2, length(vals), 1)]
  }

  # Define axis limits
  if (is.null(my_ylim)) {
    my_ylim <- range(na.omit(c(data$mean_var - 2 * data$se_var, data$mean_var + 2 * data$se_var)))
  }

  # Get region names
  sregions <- as.character(sort(unique(df$group_var)))

  # Initialize plot
  pdf(file_name)

  for (j in 1:length(sregions)) {
    print(sregions[j])
    par(mfrow = c(3, 3))
    for (i in 1:length(year_low)) {
      dat_plot <- subset(df, year_var >= year_low[i] & year_var < year_high[i] & df[, "group_var"] == sregions[j])
      coverage_col <- rep(0, nrow(dat_plot))
      coverage_col[dat_plot$survey_type == "National"] <- rainbow(50, alpha = 0.5)[30]
      coverage_col[dat_plot$survey_type == "Subnational"] <- rainbow(50, alpha = 0.5)[8]
      coverage_col[dat_plot$survey_type == "Community"] <- rainbow(50, alpha = 0.5)[46]
      scope.pch <- rep(16, nrow(dat_plot))
      scope.pch[dat_plot$urban_rural == "rural"] <- 15
      scope.pch[dat_plot$urban_rural == "urban"] <- 17

      plot(dat_plot$age, dat_plot[, "mean_var"],
        pch = scope.pch, col = coverage_col,
        xlim = my_xlim,
        ylim = my_ylim,
        cex = .6, main = paste(sregions[j], "\n",
          year_low[i], " <= year < ", year_high[i],
          sep = ""
        ), xlab = "", ylab = "", cex.main = .8
      )

      for (i in 1:nrow(dat_plot)) {
        lines(c(dat_plot$age[i], dat_plot$age[i]), c(
          (dat_plot[, "mean_var"][i] - 2 * dat_plot[, "se_var"][i]),
          (dat_plot[, "mean_var"][i] + 2 * dat_plot[, "se_var"][i])
        ), col = coverage_col[i])
      }
    }
  }

  dev.off()
}
