## clean_svydesign ## 
## This function stratifies the data based in: id_study, or id_study//age_mean//sex, or id_study//age_mean//sex//is_urban;
# It then checks whether within a given study there is a mix of subjects with & without (NA) values 
# for any survey design variable: psu, stratum, sample weight (each variable is explored separately). 
# These studies where there is a mixed of subjects with & without (i.e. NA) values for
# the survey design variables are problematic because the svydesign() function can't deal 
# with NA values. Therefore, subjects with NA values within these "problematic studies" must be removed. 
clean_svydesign <- function (data, by = "survey") {
    
    if(grepl("S", rownames(data))[1] == FALSE) { # Make sure we have rownames
        rownames(data) <- paste("S", 1:nrow(data), sep = "")
        data$names <- rownames(data)
    }
    
    if (by == "age_gender") {
        sw_prob <- ddply(data,.(id_study, age_mean, sex), function(tmp) svy_check(tmp))
        psu_prob <- ddply(data,.(id_study, age_mean, sex), function(tmp) svy_check(tmp, var = "psu"))
        stratum_prob <- ddply(data,.(id_study, age_mean, sex), function(tmp) svy_check(tmp, var = "stratum"))
        
    } else if (by == "urban"){
        sw_prob <- ddply(data,.(id_study, age_mean, sex, is_urban), function(tmp) svy_check(tmp))
        psu_prob <- ddply(data,.(id_study, age_mean, sex, is_urban), function(tmp) svy_check(tmp, var = "psu"))
        stratum_prob <- ddply(data,.(id_study, age_mean, sex, is_urban), function(tmp) svy_check(tmp, var = "stratum"))
       
    } else { # Do not stratifiy
        sw_prob <- ddply(data,.(id_study), function(tmp) svy_check(tmp))
        psu_prob <- ddply(data,.(id_study), function(tmp) svy_check(tmp, var = "psu"))
        stratum_prob <- ddply(data,.(id_study), function(tmp) svy_check(tmp, var = "stratum"))
    }
    
    if (ncol(sw_prob) > 4) {
        sw_prob <- sw_prob[, setdiff(colnames(sw_prob), c("min_sw", "max_sw"))]
    }
    
    all_prob <- rbind(sw_prob, psu_prob, stratum_prob)
    
    if (nrow(all_prob) == 0) {
        message("None of the subjects was dropped due to survey design")
        return(data)
    }
    indx <- unlist(strsplit(all_prob$NA_idx, "//"))
    if (sum(as.numeric(all_prob$N_NA)) != length(indx)) {
        stop ("Problem in code")
    }
    indx_wanted <- setdiff(rownames(data), indx)

    to_print <- paste("Number of subjects removed due to survey design:", length(indx))
    data <- data[indx_wanted, ]
    
    return(data)
    
}