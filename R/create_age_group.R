## create_age_groups ##
# agemin: if age_min_anthro and age_max_anthro are within a 10-year band agemin = age_min_anthro, else agemin = floor(age_clean).
# Exceptions: if age_clean < 20, agemin = age_clean; if age_clean > 80, agemin = 80
# agemax: if age_min_anthro and age_max_anthro are within a 10-year band agemin = age_max_anthro, else agemax = floor(age_clean) + 9.
# Exceptions: if age_clean < 20, agemax = age_clean
create_age_groups <- function(data) {
  data$agemin <- ifelse(data$age_clean < 20, data$age_clean, ifelse(data$age_clean >=
    20 & data$age_clean < 30 & data$age_min_anthro_F >= 20 & data$age_min_anthro_F <
    30 & data$sex == 2, data$age_min_anthro_F, ifelse(data$age_clean >= 30 &
    data$age_clean < 40 & data$age_min_anthro_F >= 30 & data$age_min_anthro_F <
    40 & data$sex == 2, data$age_min_anthro_F, ifelse(data$age_clean >= 40 &
    data$age_clean < 50 & data$age_min_anthro_F >= 40 & data$age_min_anthro_F <
    50 & data$sex == 2, data$age_min_anthro_F, ifelse(data$age_clean >= 50 &
    data$age_clean < 60 & data$age_min_anthro_F >= 50 & data$age_min_anthro_F <
    60 & data$sex == 2, data$age_min_anthro_F, ifelse(data$age_clean >= 60 &
    data$age_clean < 70 & data$age_min_anthro_F >= 60 & data$age_min_anthro_F <
    70 & data$sex == 2, data$age_min_anthro_F, ifelse(data$age_clean >= 70 &
    data$age_clean < 80 & data$age_min_anthro_F >= 70 & data$age_min_anthro_F <
    80 & data$sex == 2, data$age_min_anthro_F, ifelse(data$age_clean >= 80 &
    data$sex == 2, 80, ifelse(data$age_clean >= 20 & data$age_clean < 30 & data$age_min_anthro_M >=
    20 & data$age_min_anthro_M < 30 & data$sex == 1, data$age_min_anthro_M, ifelse(data$age_clean >=
    30 & data$age_clean < 40 & data$age_min_anthro_M >= 30 & data$age_min_anthro_M <
    40 & data$sex == 1, data$age_min_anthro_M, ifelse(data$age_clean >= 40 &
    data$age_clean < 50 & data$age_min_anthro_M >= 40 & data$age_min_anthro_M <
    50 & data$sex == 1, data$age_min_anthro_M, ifelse(data$age_clean >= 50 &
    data$age_clean < 60 & data$age_min_anthro_M >= 50 & data$age_min_anthro_M <
    60 & data$sex == 1, data$age_min_anthro_M, ifelse(data$age_clean >= 60 &
    data$age_clean < 70 & data$age_min_anthro_M >= 60 & data$age_min_anthro_M <
    70 & data$sex == 1, data$age_min_anthro_M, ifelse(data$age_clean >= 70 &
    data$age_clean < 80 & data$age_min_anthro_M >= 70 & data$age_min_anthro_M <
    80 & data$sex == 1, data$age_min_anthro_M, ifelse(data$age_clean >= 80 &
    data$sex == 1, 80, floor(data$age_clean / 10) * 10)))))))))))))))

  data$agemax <- ifelse(data$age_clean < 20, data$age_clean, ifelse(data$age_clean >=
    20 & data$age_clean < 30 & data$age_max_anthro_F >= 20 & data$age_max_anthro_F <
    30 & data$sex == 2, data$age_max_anthro_F, ifelse(data$age_clean >= 30 &
    data$age_clean < 40 & data$age_max_anthro_F >= 30 & data$age_max_anthro_F <
    40 & data$sex == 2, data$age_max_anthro_F, ifelse(data$age_clean >= 40 &
    data$age_clean < 50 & data$age_max_anthro_F >= 40 & data$age_max_anthro_F <
    50 & data$sex == 2, data$age_max_anthro_F, ifelse(data$age_clean >= 50 &
    data$age_clean < 60 & data$age_max_anthro_F >= 50 & data$age_max_anthro_F <
    60 & data$sex == 2, data$age_max_anthro_F, ifelse(data$age_clean >= 60 &
    data$age_clean < 70 & data$age_max_anthro_F >= 60 & data$age_max_anthro_F <
    70 & data$sex == 2, data$age_max_anthro_F, ifelse(data$age_clean >= 70 &
    data$age_clean < 80 & data$age_max_anthro_F >= 70 & data$age_max_anthro_F <
    80 & data$sex == 2, data$age_max_anthro_F, ifelse(data$age_clean >= 80 &
    data$sex == 2, data$age_max_anthro_F, ifelse(data$age_clean >= 20 & data$age_clean <
    30 & data$age_max_anthro_M >= 20 & data$age_max_anthro_M < 30 & data$sex ==
    1, data$age_max_anthro_M, ifelse(data$age_clean >= 30 & data$age_clean <
    40 & data$age_max_anthro_M >= 30 & data$age_max_anthro_M < 40 & data$sex ==
    1, data$age_max_anthro_M, ifelse(data$age_clean >= 40 & data$age_clean <
    50 & data$age_max_anthro_M >= 40 & data$age_max_anthro_M < 50 & data$sex ==
    1, data$age_max_anthro_M, ifelse(data$age_clean >= 50 & data$age_clean <
    60 & data$age_max_anthro_M >= 50 & data$age_max_anthro_M < 60 & data$sex ==
    1, data$age_max_anthro_M, ifelse(data$age_clean >= 60 & data$age_clean <
    70 & data$age_max_anthro_M >= 60 & data$age_max_anthro_M < 70 & data$sex ==
    1, data$age_max_anthro_M, ifelse(data$age_clean >= 70 & data$age_clean <
    80 & data$age_max_anthro_M >= 70 & data$age_max_anthro_M < 80 & data$sex ==
    1, data$age_max_anthro_M, ifelse(data$age_clean >= 80 & data$sex == 1, data$age_max_anthro_M,
    9 + floor(data$age_clean / 10) * 10
  )))))))))))))))

  data$age_group <- paste(data$agemin, "-", data$agemax, sep = "")

  data$age_mean <- ifelse(data$agemax - data$agemin == 0, data$agemax, ifelse(data$agemax ==
    200, 84.91, data$agemin + (data$agemax + 1 - data$agemin) / 2))

  return(data)
}
