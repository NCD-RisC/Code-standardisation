## clean_anthro ## 
## Cleans variables ("var") according to the criteria specified within the function
## Anthroprometric variables are cleaned using age-specific plausible ranges
clean_anthro <- function(data, var, bmi_only_surveyids = NULL) {
    
    if (var == "sex") {
        # If sex is not 1 or 2, set it as NA (preexisting NA values, remain NA)
        message("Cleaning sex variable")
        print(paste("Number of subjects recoded as NA:", length(which(!data$sex %in% c(1, 2)))))
        data$sex[which(!data$sex %in% c(1, 2))] = NA
    }
    if (var == "age") {
        # If age is not included within the range [age_min_anthro, age_max_anthro], set
        # age to NA
        message("Cleaning age variable")
        data$age_clean <- ifelse((data$age < data$age_min_anthro_F | data$age > data$age_max_anthro_F) & 
            data$sex == 2, NA, ifelse((data$age < data$age_min_anthro_M | data$age > 
            data$age_max_anthro_M) & data$sex == 1, NA, data$age))
        print(paste("Number of subjects recoded as NA:", sum(is.na(data$age_clean)) - sum(is.na(data$age))))
        
    }
    if (var == "pregnant") {
        if (!"is_pregnant" %in% names(data)) {
          message("No pregnancy variable")
        } else {
          message("Cleaning pregnant")
          # Pregnant for men (=1) spread across surveys --> all men & preg are considered as men
          preg_old <- data$is_pregnant
          data$is_pregnant <- ifelse(data$is_pregnant == 1 & data$sex == 2 & (data$age_clean >= 10 & data$age_clean <= 49), 
                                     1, 0) # Only 1 if female, and pregnant, and age [10-49] # Males, or too young/too old females are set to zero.
          data$is_pregnant <- ifelse(is.na(data$is_pregnant), 0, data$is_pregnant) # if it's NA, set to 0. This was updated Dec-18
          print(paste("Number of subjects recoded as 0:", sum(data$is_pregnant == 0) - length(which(preg_old == 0))))
        }
    }
    if (var == "height") {
        message("Cleaning height variable")
        # Clean height according to age group #
        data$height_clean <- data$height
        data[which((data$age_clean >= 5 & data$age_clean < 10) & (data$height < 60 | data$height > 180)), "height_clean"] <- NA
        data[which((data$age_clean >= 10 & data$age_clean < 15) & (data$height < 80 | data$height > 200)), "height_clean"] <- NA
        data[which((data$age_clean >= 15) & (data$height < 100 | data$height > 250)), "height_clean"] <- NA
        print(paste("Number of subjects recoded as NA:", sum(is.na(data$height_clean)) - sum(is.na(data$height))))
        
    }
    if (var == "weight") {
        message("Cleaning weight variable")
        # Clean weight according to age group #
        data$weight_clean <- data$weight
        data[which((data$age_clean >= 5 & data$age_clean < 10) & (data$weight < 5 | data$weight > 90)), "weight_clean"] <- NA
        data[which((data$age_clean >= 10 & data$age_clean < 15) & (data$weight < 8 | data$weight > 150)), "weight_clean"] <- NA
        data[which((data$age_clean >= 15) & (data$weight < 12 | data$weight > 300)), "weight_clean"] <- NA
        data$weight_clean[which(data$is_pregnant == 1)] <- NA # clean for pregnant
        print(paste("Number of subjects recoded as NA:", sum(is.na(data$weight_clean)) - sum(is.na(data$weight))))
    }
    if (var == "waist") {
        message("Cleaning waist variable")
        data$waist_clean <- ifelse(data$waist < 30 | data$waist > 200, NA, data$waist)
        data$waist_clean[which(data$is_pregnant == 1)] <- NA  # clean for pregnant
        print(paste("Number of subjects recoded as NA:", sum(is.na(data$waist_clean)) - sum(is.na(data$waist))))
    }
    if (var == "hip") {
        message("Cleaning hip variable")
        data$hip_clean <- ifelse(data$hip < 45 | data$hip > 300, NA, data$hip)
        data$hip_clean[which(data$is_pregnant == 1)] <- NA  # clean for pregnant
        print(paste("Number of subjects recoded as NA:", sum(is.na(data$hip_clean)) - sum(is.na(data$hip))))
    }
    if (var == "whr") {
        message("Cleaning waist-hip-ratio variable")
        data$whr_clean <- ifelse(is.na(data$waist_clean) | is.na(data$hip_clean), NA, data$whr)
        data$whr_clean <- ifelse(data$whr_clean < 0.4 | data$whr_clean > 1.8, NA, data$whr_clean)
        data$whr_clean[which(data$is_pregnant == 1)] <- NA  # clean for pregnant
        print(paste("Number of subjects recoded as NA:", sum(is.na(data$whr_clean)) - sum(is.na(data$whr))))
    }
    if (var == "bmi") {
        # Clean BMI according to age group # 
        message("Cleaning BMI variable")
        data$bmi_clean <- data$bmi
        data[which((data$age_clean >= 5 & data$age_clean < 10) & (data$bmi < 6 | data$bmi > 40)), "bmi_clean"] <- NA
        data[which((data$age_clean >= 10 & data$age_clean < 15) & (data$bmi < 8 | data$bmi > 60)), "bmi_clean"] <- NA
        data[which((data$age_clean >= 15) & (data$bmi < 10 | data$bmi > 80)), "bmi_clean"] <- NA
        data$bmi_clean[which(data$is_pregnant == 1)] <- NA  # clean for pregnant
        ## Reclean for height and weight
        if (length(setdiff(c("height_clean", "weight_clean"), names(data)) > 0)) { # 
            stop("Variables height_clean or weight_clean are missing")
        }
        if (!is.null(bmi_only_surveyids)) { 
            data$bmi_clean[data$id_study %in% bmi_only_surveyids] <- data$bmi[data$id_study %in% bmi_only_surveyids]
            data$bmi_clean <- ifelse((is.na(data$height_clean) | is.na(data$weight_clean)) & 
                                         (!data$id_study %in% bmi_only_surveyids), NA, data$bmi_clean)
        } else {
            data$bmi_clean <- ifelse(is.na(data$height_clean) | is.na(data$weight_clean), NA, data$bmi_clean)
        }
        print(paste("Number of subjects recoded as NA:", sum(is.na(data$bmi_clean)) - sum(is.na(data$bmi))))
    }
    
    return(data)
}