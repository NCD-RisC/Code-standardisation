## bmi_adol ## 
# Gets prevalence of bmi for a given bmi category (bmi_cat), and for a given age_sex group (bmi_line)
bmi_adol <- function(bmi_line, bmi_cat, bmi_data) {
    if (grepl("neg", bmi_cat)) {
        res <- which(bmi_data$age_mean == bmi_line$age & bmi_data$sex == bmi_line$sex & 
                         bmi_data$bmi_clean < bmi_line[, bmi_cat])
    } else {
        res <- which(bmi_data$age_mean == bmi_line$age & bmi_data$sex == bmi_line$sex & 
                         bmi_data$bmi_clean > bmi_line[, bmi_cat])
    }
    return(res)
}

