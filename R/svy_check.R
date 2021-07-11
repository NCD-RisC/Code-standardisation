## svy_check ## 
## Checks whether in tmp[, "var"] there is a mixed of subjects with reported values
# and subjects with missing values for "var"
svy_check <- function(tmp, var = "samplewt_anthro") {
    
    if (any(is.na(tmp[, var])) & !all(is.na(tmp[, var]))) {
        
        if (is.numeric(tmp[, var]) & var == "samplewt_anthro") {
            ans <- c(sum(is.na(tmp[, var])), 
                     round(100*sum(is.na(tmp[, var]))/nrow(tmp), 2),
                     range(na.omit(tmp[, var])), 
                     paste(tmp$names[(is.na(tmp[, var]))], collapse = "//"))
            names(ans) <- c("N_NA","%_NA", "min_sw", "max_sw", "NA_idx")
            return(ans)
            
        } else {
            ans <- c(sum(is.na(tmp[, var])), 
                     round(100*sum(is.na(tmp[, var]))/nrow(tmp), 2),
                     paste(tmp$names[(is.na(tmp[, var]))], collapse = "//"))
            names(ans) <- c("N_NA", "%_NA", "NA_idx")
            return(ans)
        }
    } else {
        return(NULL)
    }
}