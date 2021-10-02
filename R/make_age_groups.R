
make_age_groups <- function(age, age_design_min, age_design_max) {
  # mean age for open age groups
  mean_age_open_age_group <-
    data.frame(
      min_age = 65:100,
      mean_age = c(
        73.81, 74.52, 75.23, 75.95, 76.67,
        77.40, 78.14, 78.88, 79.62, 80.37, 81.11, 81.86, 82.61, 83.37, 84.14,
        84.91, 85.69, 86.48, 87.27, 88.07, 88.88, 89.69, 90.50, 91.33, 92.15,
        92.98, 93.81, 94.64, 95.47, 96.30, 97.12, 97.94, 98.75, 99.52, 100.15, 100.50
      )
    )

  # naive groupin
  age_group0 <- cut(age, c(18, seq(20, 80, by = 10), 200), right = FALSE)
  age_min <- c(18, seq(20, 80, by = 10))[as.numeric(age_group0)]
  age_max <- c(seq(20, 80, by = 10), 200)[as.numeric(age_group0)]

  # update the first and last age group according to age range
  age_min[which(age_min < age_design_min)] <- age_design_min[which(age_min < age_design_min)]
  age_max[which(age_max > age_design_max)] <- age_design_max[which(age_max > age_design_max)] + 1

  # single year age groups: use nominal age rather than xx.5 as mid-age
  age_max[which(age_max - age_min == 1)] <- age_min[which(age_max - age_min == 1)]

  # generate age group and mean age
  age_group <- paste0(age_min, "-", age_max)
  age_group <- gsub("-200", "\\+", age_group)
  age_mean <- rowMeans(cbind(age_min, age_max))
  # use pre-determined mean age for for open-ended age groups
  age_mean[which(age_max == 200)] <- mean_age_open_age_group$mean_age[match(age_min[which(age_max == 200)], mean_age_open_age_group$min_age)]

  return(data.frame(age_mean, age_group))
}
