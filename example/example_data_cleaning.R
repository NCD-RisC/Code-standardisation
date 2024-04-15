# NCD-RisC
# Example of use of data cleaning functions
# January 2024

source('../R/data-handling/cleaning_functions_data.R')

library(dplyr)

data <- read.csv('../data/USA NHANES 2017-2018.csv')

# test unit and plasma conversion for FPG
data$fgl <- data$fgl / 0.0556
data$unit_gl <- 'mg/dL'
data$is_plasma <- 0

# test implausible pp
data$sbp1[123] <- 70
data$dbp1[123] <- 120

# create secondary variables
data$whr <- data$waist / data$hip
data$whtr <- data$waist / data$height
data$bmi <- data$weight / data$height^2 * 10000

# rename hba1c
data <- rename(data, hba1c = ha1c)
data <- rename(data, unit_hba1c = unit_ha1c)

# data cleaning
# for BP, cleaning pulse pressure first
bp_vars <- grep('^sbp|^dbp', names(data), value = TRUE)
bp_var_index <- unique(gsub('sbp|dbp', '', bp_vars))
sbp_missing_code <- c(0,777,888,994,995,996,999)
dbp_missing_code <- c(0,222,666,888,994,995,996,999,1000,9999)
for (i in bp_var_index) {

    message(paste0('Cleaning variable "sbp', i, '", "dbp', i, '":'))

    # remove code for missing data
    # sbp
    var <- data[[paste0('sbp', i)]]
    clean_list <- which(var %in% sbp_missing_code)
    cat(paste0('  ', length(clean_list), ' of ', sum(!is.na(var)), ' sbp', i, ' were removed for having a code for missing data\n'))
    data[clean_list, paste0('sbp', i)] <- NA
    # dbp
    var <- data[[paste0('dbp', i)]]
    clean_list <- which(var %in% dbp_missing_code)
    cat(paste0('  ', length(clean_list), ' of ', sum(!is.na(var)), ' dbp', i, ' were removed for having a code for missing data\n'))
    data[clean_list, paste0('dbp', i)] <- NA

    # calculate pulse pressure
    data[paste0('pp', i)] <- data[paste0('sbp', i)] - data[paste0('dbp', i)]

    # remove data if pulse pressure < 10 mmHg
    var <- data[paste0('pp', i)]
    clean_list <- which(var < 10)
    cat(paste0('  ', length(clean_list), ' of ', sum(!is.na(var)), ' pp', i, ' <10 mmHg were removed (sbp and dbp were removed too)\n'))
    data[clean_list, paste0('pp',  i)] <- NA
    data[clean_list, paste0('sbp', i)] <- NA
    data[clean_list, paste0('dbp', i)] <- NA
}

# the variable list should be specified according to need
vars <- c('sex','age','is_pregnant','is_fasting','is_urban','self_diab','drug_diab_insu','drug_diab_pill','drug_diab','height','weight','bmi','waist','hip','whtr','whr','fgl','hba1c','tc','hdl','ldl','trg', bp_vars)
for (v in vars) {

    if (!v %in% names(data)) {
        message(paste0('Variable "', v, '" not in data frame'))
        next
    }

    v_clean <- clean_data(data, v)     # clean_data function returns a list of positions with implausible values

    if (v %in% c('sex','age','is_pregnant','is_fasting','is_urban','self_diab','drug_diab_insu','drug_diab_pill','drug_diab')) {
        # for non biomarkers, replace original variables with cleaned one
        # bp variables already have _clean appended
        data[v] <- v_clean
    } else {
        # otherwise create a v_clean variable
        data[paste0(v, '_clean')] <- v_clean
    }

}

# additional cleaning needed, dependent on the risk factors

# remove weight and bmi data for pregnant women
# but keep height data
data$weight_clean[which(data$is_pregnant == 1)] <- NA
data$bmi_clean[which(data$is_pregnant == 1)] <- NA

# note: pregnant women are removed entirely from waist, BP and glucose analyses, ie by row not by variable
# data <- filter(data, is_pregnant == 0 | is.na(is_pregnant))

# cleaning composite variables when components are removed
bmi_only_studies <- c()  # need to be determined at study level
data$bmi_clean[(is.na(data$height_clean) | is.na(data$weight_clean)) & !data$id_study %in% bmi_only_studies] <- NA
data$whtr_clean[is.na(data$waist_clean) | is.na(data$height_clean)] <- NA
data$whr_clean[is.na(data$waist_clean) | is.na(data$hip_clean)] <- NA

# cleaning FPG for fasting status
data$fasting_accept <- NA
data$fasting_accept[which(data$fasting_time<=24 & data$fasting_time>=6)] <- 1
data$fasting_accept[which(data$fasting_time>24 | data$fasting_time<6)]   <- 0
data$fasting_accept[which(is.na(data$fasting_time) & data$is_fasting==1)] <- 1
data$fasting_accept[which(is.na(data$fasting_time) & data$is_fasting==0)] <- 0
data$fasting_accept[which(is.na(data$fasting_time) & is.na(data$is_fasting))] <- NA
print(table(data$fasting_accept, data$id_study, exclude = NULL))

data$fgl_clean[which(data$fasting_accept==0)] <- NA

