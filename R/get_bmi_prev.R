## get bmi_prev ## 
# Gives 1 if an individual falls in a given BMI category, else 0 (subjects with NA values in height_data were removed)
get_bmi_prev = function(bmi_data) {
    
    # prevalences
    bmi_data$prev_bmi12 <- ifelse(bmi_data$bmi_clean < 12, 1,0)
    bmi_data$prev_bmi12_15 <- ifelse(bmi_data$bmi_clean >= 12 & bmi_data$bmi_clean < 15, 1, 0)
    bmi_data$prev_bmi15_185 <- ifelse(bmi_data$bmi_clean >= 15 & bmi_data$bmi_clean < 18.5, 1, 0)
    bmi_data$prev_bmi185_20 <- ifelse(bmi_data$bmi_clean >= 18.5 & bmi_data$bmi_clean < 20, 1, 0)
    bmi_data$prev_bmi20_25 <- ifelse(bmi_data$bmi_clean >= 20 & bmi_data$bmi_clean < 25, 1, 0)
    bmi_data$prev_bmi25_30 <- ifelse(bmi_data$bmi_clean >= 25 & bmi_data$bmi_clean < 30, 1, 0)
    bmi_data$prev_bmi30_35 <- ifelse(bmi_data$bmi_clean >= 30 & bmi_data$bmi_clean < 35, 1, 0)
    bmi_data$prev_bmi35_40 <- ifelse(bmi_data$bmi_clean >= 35 & bmi_data$bmi_clean < 40, 1, 0)
    bmi_data$prev_bmi40 <- ifelse(bmi_data$bmi_clean >= 40, 1, 0)
    bmi_data$prev_bmi25 <- ifelse(bmi_data$bmi_clean >= 25, 1, 0)
    bmi_data$prev_bmi30 <- ifelse(bmi_data$bmi_clean >= 30, 1, 0)
    bmi_data$prev_bmi22 <- ifelse(bmi_data$bmi_clean >= 22, 1, 0)
    bmi_data$prev_bmi23 <- ifelse(bmi_data$bmi_clean >= 23, 1, 0)
    bmi_data$prev_bmi24 <- ifelse(bmi_data$bmi_clean >= 24, 1, 0)
    bmi_data$prev_bmi27 <- ifelse(bmi_data$bmi_clean >= 27, 1, 0)
    bmi_data$prev_bmi275 <- ifelse(bmi_data$bmi_clean >= 27.5, 1, 0)
    bmi_data$prev_bmi28 <- ifelse(bmi_data$bmi_clean >= 28, 1, 0)
    bmi_data$prev_bmi_185_25 <- ifelse(bmi_data$bmi_clean < 25 & bmi_data$bmi_clean >= 18.5, 1, 0)
    bmi_data$prev_bmi_l185 <- ifelse(bmi_data$bmi_clean < 18.5, 1, 0)
    bmi_data$prev_bmi_l16 <- ifelse(bmi_data$bmi_clean < 16, 1, 0)
    bmi_data$prev_bmi_16_17 <- ifelse(bmi_data$bmi_clean < 17 & bmi_data$bmi_clean >= 16, 1, 0)
    bmi_data$prev_bmi_l17 <- ifelse(bmi_data$bmi_clean < 17, 1, 0)
    bmi_data$prev_bmi_l25 <- ifelse(bmi_data$bmi_clean < 25, 1, 0)
    bmi_data$prev_bmi_30_40 <- ifelse(bmi_data$bmi_clean < 40 & bmi_data$bmi_clean >= 30, 1, 0)
    bmi_data$prev_bmi_17_185 <- ifelse(bmi_data$bmi_clean < 18.5 & bmi_data$bmi_clean >= 17, 1,0)
    
    ## Adolescent cut-offs 
    bmi_cutoff <- read.csv("utilities/child_adolescent_bmi_cutoffs.csv")
    bmi_rows <- split(bmi_cutoff, f = seq(nrow(bmi_cutoff)))
    
    idx_neg2sd <- unlist(lapply(bmi_rows, bmi_adol, bmi_cat = "bmi_neg2sd", bmi_data = bmi_data)) 
    idx_neg1sd <- unlist(lapply(bmi_rows, bmi_adol, bmi_cat = "bmi_neg1sd", bmi_data = bmi_data))
    idx_sd <- unlist(lapply(bmi_rows, bmi_adol, bmi_cat = "bmi_sd", bmi_data = bmi_data))
    idx_2sd <- unlist(lapply(bmi_rows, bmi_adol, bmi_cat = "bmi_2sd", bmi_data = bmi_data))
    
    bmi_data$prev_bmi_neg2sd <- 0  
    bmi_data$prev_bmi_neg2sd[idx_neg2sd] <- 1 # Works even in idx_neg2sd = integer(0)

    bmi_data$prev_bmi_neg1sd <- 0 
    bmi_data$prev_bmi_neg1sd[idx_neg1sd] <- 1
    
    bmi_data$prev_bmi_1sd <- 0 
    bmi_data$prev_bmi_1sd[idx_sd] <- 1
    
    bmi_data$prev_bmi_2sd <- 0 
    bmi_data$prev_bmi_2sd[idx_2sd] <- 1
    
    return(bmi_data)
}