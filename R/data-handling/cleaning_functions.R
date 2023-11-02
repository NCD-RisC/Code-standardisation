# Utility functions
library(dplyr)

# 'General cleaning function
#'
#' cleaning a specific variable in the data frame
#' @param data The data frame to be cleaned
#' @param variable The name of the variable to be cleaned
#' @return A index of the values in the variable that should be cleaned
#' @examples
#' cleaned_index <- clean_data(data, 'age');
#' cleaned_index <- clean_data(data, 'bmi');
#' @export
clean_data <- function(data, variable) {
  clean_list <- switch(variable,
                       sex    = clean_sex(data$sex),
                       age    = clean_age(data$age),

                       height1 = , height2 = , height3 = ,
                       height = clean_height(data$height, data$age),
                       weight1 = , weight2 = , weight3 = ,
                       weight = clean_weight(data$weight, data$age, data$is_pregnant),
                       bmi    = clean_bmi(data$bmi, data$age, data$is_pregnant),

                       waist1 = , waist2 = , waist3 = ,
                       waist = clean_waist(data[[variable]], data$age, data$is_pregnant),
                       hip1 = , hip2 = , hip3 = ,
                       hip = clean_hip(data[[variable]], data$age, data$is_pregnant),

                       whtr = clean_wth(data$whtr, data$age, data$is_pregnant),
                       whr = clean_whr(data$whr, data$age, data$is_pregnant),

                       is_pregnant = , is_pregnant_exam = ,
                       is_urban = clean_cat(data[[variable]], variable),

                       drug_diab = , drug_diab_insu = , drug_diab_pill = ,
                       self_diab = clean_cat(data[[variable]], variable),

                       fgl    = clean_continuous(data[[variable]], variable, 2, 30),
                       ppg    = clean_continuous(data[[variable]], variable, 2, 30),
                       hba1c  = clean_continuous(data[[variable]], variable, 3, 18),

                       drug_hyper = , drug_presc = , drug_bp = ,
                       self_hyper_12mos = , self_hyper_preg = ,
                       self_hyper = clean_cat(data[[variable]], variable),

                       sbp1 = , sbp2 = , sbp3 = , sbp4 = , sbp5 = , sbp6 = , sbp7 = , sbp8 = ,
                       sbp9 = , sbp10 = , sbp11 = , sbp12 = , sbp13 =,
                       sbp_avg = clean_continuous(data[[variable]], variable, 70, 270),

                       dbp1 = , dbp2 = , dbp3 = , dbp4 = , dbp5 = , dbp6 = , dbp7 = , dbp8 = ,
                       dbp9 = , dbp10 = , dbp11 = , dbp12 = , dbp13 =,
                       dbp_avg = clean_continuous(data[[variable]], variable, 30, 150),

                       drug_chol = , drug_chol_stat = , drug_chol_fibr = ,
                       self_chol = clean_cat(data[[variable]], variable),

                       tc  = clean_continuous(data[[variable]], variable, 1.75, 20),
                       ldl = clean_continuous(data[[variable]], variable, 0.5,  10),
                       hdl = clean_continuous(data[[variable]], variable, 0.4,   5),
                       trg = clean_continuous(data[[variable]], variable, 0.2,  20),

                       {print(paste('Variable', variable, 'is not expected')); c()}
  )
  return(clean_list)
}

clean_height <- function(height, age) {
  clean_list <- c()
  # Clean height according to age group #
  clean_list <- c(clean_list, which((age >= 5 & age < 10) & (height < 60 | height > 180)))
  clean_list <- c(clean_list, which((age >= 10 & age < 15) & (height < 80 | height > 200)))
  clean_list <- c(clean_list, which((age >= 15) & (height < 100 | height > 250)))
  clean_list <- unique(clean_list)
  print(paste("Number of height data recoded as NA:", length(clean_list), "of", sum(!is.na(height))))
  return(clean_list)
}

clean_weight <- function(weight, age, is_pregnant) {
  clean_list <- c()
  # Clean weight according to age group #
  clean_list <- c(clean_list, which((age >= 5 & age < 10) & (weight < 5 | weight > 90)))
  clean_list <- c(clean_list, which((age >= 10 & age < 15) & (weight < 8 | weight > 150)))
  clean_list <- c(clean_list, which((age >= 15) & (weight < 12 | weight > 300)))
  clean_list <- c(clean_list, which(is_pregnant == 1))
  clean_list <- unique(clean_list)
  print(paste("Number of weight data recoded as NA:", length(clean_list), "of", sum(!is.na(weight))))
  return(clean_list)
}

clean_bmi <- function(bmi, age, is_pregnant) {
  clean_list <- c()
  # Clean bmi according to age group #
  clean_list <- c(clean_list, which((age >= 5 & age < 10) & (bmi < 6 | bmi > 40)))
  clean_list <- c(clean_list, which((age >= 10 & age < 15) & (bmi < 8 | bmi > 60)))
  clean_list <- c(clean_list, which((age >= 15) & (bmi < 10 | bmi > 80)))
  clean_list <- c(clean_list, which(is_pregnant == 1))
  clean_list <- unique(clean_list)
  print(paste("Number of bmi data recoded as NA:", length(clean_list), "of", sum(!is.na(bmi))))
  return(clean_list)
}

clean_waist <- function(waist, age, is_pregnant) {
  clean_list <- c()
  # Clean waist according to age group #
  clean_list <- c(clean_list, which((age >= 5 & age < 10) & (waist < 20 | waist > 150)))
  clean_list <- c(clean_list, which((age >= 10 & age < 15) & (waist < 20 | waist > 200)))
  clean_list <- c(clean_list, which((age >= 15) & (waist < 30 | waist > 300)))
  clean_list <- c(clean_list, which(is_pregnant == 1))
  clean_list <- unique(clean_list)
  print(paste("Number of waist data recoded as NA:", length(clean_list), "of", sum(!is.na(waist))))
  return(clean_list)
}

clean_wth <- function(wth, age, is_pregnant) {
  clean_list <- c()
  # Clean wth according to age group #
  clean_list <- c(clean_list, which((age >= 5 & age < 15) & (wth < 0.2 | wth > 1.5)))
  clean_list <- c(clean_list, which((age >= 15) & (wth < 0.2 | wth > 2.0)))
  clean_list <- c(clean_list, which(is_pregnant == 1))
  clean_list <- unique(clean_list)
  print(paste("Number of waist-to-height ratio data recoded as NA:", length(clean_list), "of", sum(!is.na(wth))))
  return(clean_list)
}


clean_hip <- function(hip, age, is_pregnant) {
  clean_list <- c()
  # Clean hip according to age group #
  clean_list <- c(clean_list, which((age >= 5 & age < 10) & (hip < 30 | hip > 180)))
  clean_list <- c(clean_list, which((age >= 10 & age < 15) & (hip < 30 | hip > 200)))
  clean_list <- c(clean_list, which((age >= 15) & (hip < 40 | hip > 300)))
  clean_list <- c(clean_list, which(is_pregnant == 1))
  clean_list <- unique(clean_list)
  print(paste("Number of hip data recoded as NA:", length(clean_list), "of", sum(!is.na(hip))))
  return(clean_list)
}


clean_whr <- function(whr, age, is_pregnant) {
  clean_list <- c()
  # Clean whr according to age group #
  clean_list <- c(clean_list, which((age >= 5 & age < 15) & (whr < 0.4 | whr > 1.8)))
  clean_list <- c(clean_list, which((age >= 15) & (whr < 0.4 | whr > 2.0)))
  clean_list <- c(clean_list, which(is_pregnant == 1))
  clean_list <- unique(clean_list)
  print(paste("Number of wait-to-hip ratio data recoded as NA:", length(clean_list), "of", sum(!is.na(whr))))
  return(clean_list)
}

clean_sbp <- function(sbp, var_name) {
  clean_list <- which(sbp < 70 | sbp > 270)
  print(paste("Number of", var_name, "data recoded as NA:", length(clean_list), "of", sum(!is.na(sbp))))
  return(clean_list)
}

clean_dbp <- function(dbp, var_name) {
  clean_list <- which(dbp < 30 | dbp > 150)
  print(paste("Number of", var_name, "data recoded as NA:", length(clean_list), "of", sum(!is.na(dbp))))
  return(clean_list)
}

clean_sex <- function(sex) {
  clean_list <- which(sex != 1 & sex != 2)
  print(paste("Number of sex data recoded as NA:", length(clean_list), "of", sum(!is.na(sex))))
  return(clean_list)
}

clean_age <- function(age) {
  clean_list <- which(age < 0 | age > 120)
  print(paste("Number of age data recoded as NA:", length(clean_list), "of", sum(!is.na(age))))
  return(clean_list)
}

clean_continuous <- function(var, var_name, minv, maxv) {
  clean_list <- which(var < minv | var > maxv)
  print(paste("Number of", var_name, "data recoded as NA:", length(clean_list), "of", sum(!is.na(var))))
  return(clean_list)
}

clean_cat <- function(var, var_name) {
  clean_list <- which(var != 0 & var != 1)
  print(paste("Number of", var_name, "data recoded as NA:", length(clean_list), "of", sum(!is.na(var))))
  return(clean_list)
}

clean_fgl <- function(fgl) {
  clean_list <- which(fgl < 2 | fgl > 30)
  print(paste("Number of FPG data recoded as NA:", length(clean_list), "of", sum(!is.na(fgl))))
  return(clean_list)
}

clean_hba1c <- function(hba1c) {
  clean_list <- which(hba1c < 3 | hba1c > 18)
  print(paste("Number of HbA1c data recoded as NA:", length(clean_list), "of", sum(!is.na(hba1c))))
  return(clean_list)
}
