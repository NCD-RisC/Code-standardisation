
#### Calculating metrics for summarising ####

## get bmi_prev ##
# Gives 1 if an individual falls in a given BMI category, else 0 (subjects with NA values in height_data were removed)
get_bmi_prev = function(bmi_data, adol_bmi_cutoffs = 'child_adolescent_bmi_cutoffs.csv') {

    # prevalences
    bmi_data$prev_bmi_12 <- ifelse(bmi_data$bmi_clean < 12, 1,0)
    bmi_data$prev_bmi_12_15 <- ifelse(bmi_data$bmi_clean >= 12 & bmi_data$bmi_clean < 15, 1, 0)
    bmi_data$prev_bmi_15_185 <- ifelse(bmi_data$bmi_clean >= 15 & bmi_data$bmi_clean < 18.5, 1, 0)
    bmi_data$prev_bmi_185_20 <- ifelse(bmi_data$bmi_clean >= 18.5 & bmi_data$bmi_clean < 20, 1, 0)
    bmi_data$prev_bmi_20_25 <- ifelse(bmi_data$bmi_clean >= 20 & bmi_data$bmi_clean < 25, 1, 0)
    bmi_data$prev_bmi_25_30 <- ifelse(bmi_data$bmi_clean >= 25 & bmi_data$bmi_clean < 30, 1, 0)
    bmi_data$prev_bmi_30_35 <- ifelse(bmi_data$bmi_clean >= 30 & bmi_data$bmi_clean < 35, 1, 0)
    bmi_data$prev_bmi_35_40 <- ifelse(bmi_data$bmi_clean >= 35 & bmi_data$bmi_clean < 40, 1, 0)
    bmi_data$prev_bmi_40 <- ifelse(bmi_data$bmi_clean >= 40, 1, 0)
    bmi_data$prev_bmi_25 <- ifelse(bmi_data$bmi_clean >= 25, 1, 0)
    bmi_data$prev_bmi_30 <- ifelse(bmi_data$bmi_clean >= 30, 1, 0)
    bmi_data$prev_bmi_22 <- ifelse(bmi_data$bmi_clean >= 22, 1, 0)
    bmi_data$prev_bmi_23 <- ifelse(bmi_data$bmi_clean >= 23, 1, 0)
    bmi_data$prev_bmi_24 <- ifelse(bmi_data$bmi_clean >= 24, 1, 0)
    bmi_data$prev_bmi_27 <- ifelse(bmi_data$bmi_clean >= 27, 1, 0)
    bmi_data$prev_bmi_275 <- ifelse(bmi_data$bmi_clean >= 27.5, 1, 0)
    bmi_data$prev_bmi_28 <- ifelse(bmi_data$bmi_clean >= 28, 1, 0)
    bmi_data$prev_bmi_185_25 <- ifelse(bmi_data$bmi_clean < 25 & bmi_data$bmi_clean >= 18.5, 1, 0)
    bmi_data$prev_bmi_l185 <- ifelse(bmi_data$bmi_clean < 18.5, 1, 0)
    bmi_data$prev_bmi_l16 <- ifelse(bmi_data$bmi_clean < 16, 1, 0)
    bmi_data$prev_bmi_16_17 <- ifelse(bmi_data$bmi_clean < 17 & bmi_data$bmi_clean >= 16, 1, 0)
    bmi_data$prev_bmi_l17 <- ifelse(bmi_data$bmi_clean < 17, 1, 0)
    bmi_data$prev_bmi_l25 <- ifelse(bmi_data$bmi_clean < 25, 1, 0)
    bmi_data$prev_bmi_30_40 <- ifelse(bmi_data$bmi_clean < 40 & bmi_data$bmi_clean >= 30, 1, 0)
    bmi_data$prev_bmi_17_185 <- ifelse(bmi_data$bmi_clean < 18.5 & bmi_data$bmi_clean >= 17, 1,0)

    ## Adolescent cut-offs
    bmi_cutoff <- read.csv(adol_bmi_cutoffs)
    bmi_rows <- split(bmi_cutoff, f = seq(nrow(bmi_cutoff)))

    idx_neg2sd <- unlist(lapply(bmi_rows, bmi_adol, bmi_cat = "bmi_neg2sd", bmi_data = bmi_data))
    idx_neg1sd <- unlist(lapply(bmi_rows, bmi_adol, bmi_cat = "bmi_neg1sd", bmi_data = bmi_data))
    idx_sd <- unlist(lapply(bmi_rows, bmi_adol, bmi_cat = "bmi_sd", bmi_data = bmi_data))
    idx_2sd <- unlist(lapply(bmi_rows, bmi_adol, bmi_cat = "bmi_2sd", bmi_data = bmi_data))

    bmi_data$prev_bmi_neg2sd <- 0
    bmi_data$prev_bmi_neg2sd[idx_neg2sd] <- 1 # Works even in idx_neg2sd = integer(0)
    bmi_data$prev_bmi_neg2sd[is.na(bmi_data$bmi_clean)] <- NA

    bmi_data$prev_bmi_neg1sd <- 0
    bmi_data$prev_bmi_neg1sd[idx_neg1sd] <- 1
    bmi_data$prev_bmi_neg1sd[is.na(bmi_data$bmi_clean)] <- NA

    bmi_data$prev_bmi_1sd <- 0
    bmi_data$prev_bmi_1sd[idx_sd] <- 1
    bmi_data$prev_bmi_1sd[is.na(bmi_data$bmi_clean)] <- NA

    bmi_data$prev_bmi_2sd <- 0
    bmi_data$prev_bmi_2sd[idx_2sd] <- 1
    bmi_data$prev_bmi_2sd[is.na(bmi_data$bmi_clean)] <- NA

    return(bmi_data)
}

## get_height_prev ##
# Gives 1 if an individual falls in a given height category, else 0 (subjects with NA values in height_data were removed)
get_height_prev = function(height_data) {

    height_data$prev_height_120 <- ifelse(height_data$height_clean < 120, 1, 0)
    height_data$prev_height_120_140 <- ifelse(height_data$height_clean >= 120 & height_data$height_clean < 140, 1, 0)
    height_data$prev_height_140_145 <- ifelse(height_data$height_clean >= 140 & height_data$height_clean < 145, 1, 0)
    height_data$prev_height_145_150 <- ifelse(height_data$height_clean >= 145 & height_data$height_clean < 150, 1, 0)
    height_data$prev_height_150_155 <- ifelse(height_data$height_clean >= 150 & height_data$height_clean < 155, 1, 0)
    height_data$prev_height_155_160 <- ifelse(height_data$height_clean >= 155 & height_data$height_clean < 160, 1, 0)
    height_data$prev_height_160_165 <- ifelse(height_data$height_clean >= 160 & height_data$height_clean < 165, 1, 0)
    height_data$prev_height_165_170 <- ifelse(height_data$height_clean >= 165 & height_data$height_clean < 170, 1, 0)
    height_data$prev_height_170_175 <- ifelse(height_data$height_clean >= 170 & height_data$height_clean < 175, 1, 0)
    height_data$prev_height_175_180 <- ifelse(height_data$height_clean >= 175 & height_data$height_clean < 180, 1, 0)
    height_data$prev_height_180_185 <- ifelse(height_data$height_clean >= 180 & height_data$height_clean < 185, 1, 0)
    height_data$prev_height_185_190 <- ifelse(height_data$height_clean >= 185 & height_data$height_clean < 190, 1, 0)
    height_data$prev_height_190_195 <- ifelse(height_data$height_clean >= 190 & height_data$height_clean < 195, 1, 0)
    height_data$prev_height_195_200 <- ifelse(height_data$height_clean >= 195 & height_data$height_clean < 200, 1, 0)
    height_data$prev_height_200 <- ifelse(height_data$height_clean >= 200, 1, 0)

    return(height_data)
}

# Gets prevalence of bmi for a given bmi category (bmi_cat), and for a given age_sex group (bmi_line)
bmi_adol <- function(bmi_line, bmi_cat, bmi_data) {
    bmi_data$sex <- ifelse(bmi_data$sex == 'female', 2, 1)
    if (grepl("neg", bmi_cat)) {
        res <- which(bmi_data$age_mean == bmi_line$age & bmi_data$sex == bmi_line$sex &
                         bmi_data$bmi_clean < bmi_line[, bmi_cat])
    } else {
        res <- which(bmi_data$age_mean == bmi_line$age & bmi_data$sex == bmi_line$sex &
                         bmi_data$bmi_clean > bmi_line[, bmi_cat])
    }
    return(res)
}
