## Functions used in general cleaning ##

# clean_numerical
# clean_categorical
# clean_data
# print_cleaned: deprecated
# clean_svydesign: NOTE taken from anthro_cleaning, need to go through
# clean_single_psu_ssa

## clean_numerical ##
# to clean a variable with continuous values: calls 'clean_data' function
# variable: a variable from a data frame
# plausible_range: a pair of numerical values c(min, max)
# where: rows where cleaning should be applied; applying to all rows if unspecified
# known_na_codes: values that are known to be code for missing value, which will be excluded from printing out of % cleaned
clean_numerical <- function(variable, plausible_range, where = NULL, known_na_codes = NULL, print = TRUE) {
    clean_data(variable, plausible_range, variable_type = "continuous", where, known_na_codes, print)
}

## clean_caterical ##
# to clean a variable with categorical values: calls 'clean_data' function
clean_categorical <- function(variable, plausible_values, where = NULL, known_na_codes = NULL, print = TRUE) {
    clean_data(variable, plausible_values, variable_type = "categorical", where, known_na_codes, print)
}

## clean_data ##
# to clean a variable with variable_type (continuous or categorical) values
# and print out a summary with % cleaned by id_study
clean_data <- function(variable, plausible_range, variable_type, where, known_na_codes, print = TRUE) {
    if (is.null(where)) {
        where <- 1:nrow(data)
    }
    
    cleaned_column <- data[where, variable]
    studies <- as.character(unique(data$id_study[where]))
    
    # remove values before cleaning/printing where unit_var == EXCLUDE (data were excluded)
    unit_var <- paste0("unit_", variable)                    # get unit variable
    unit_var <- gsub("fgl", "gl", unit_var)                  # fgl unit is called unit_gl
    unit_var <- gsub("(s|d)bp[0-9](*|_avg)", "bp", unit_var) # all bp variable shares unit_bp
    unit_d   <- data[where, unit_var]
    cleaned_column[which(unit_d == 'EXCLUDE')] <- NA
    
    # remove negative values before cleaning/printing
    negatives <- which(cleaned_column < 0)
    cleaned_column[negatives] <- NA
    
    # clean other known NA codes before cleaning/printing
    if (!is.null(known_na_codes)) {
        cleaned_column[which(cleaned_column%in%known_na_codes)] <- NA 
    }
    
    # cleaning implausible values and output a log
    all_nonNA <- which(!is.na(cleaned_column))
    if (variable_type == "continuous") {
        implausible <- which(cleaned_column > plausible_range[2] | cleaned_column < plausible_range[1])
    } else if (variable_type == "categorical") {
        implausible <- which(!(cleaned_column %in% c(plausible_range, NA)))   
    }
    # printing log
    if (print) {
        if (length(implausible) == 0) {
            print("No records cleaned")
        } else {
            out_table <- table(studies[implausible])/table(studies[all_nonNA])*100
            print(paste0("percentage of implausible values cleaned for ", variable, ''))
            print(sort(round(out_table[which(out_table[]!=0)], 2), decreasing = TRUE))   # printing largest % first
            cat('', sep="\n")
            
            cleaned_column[implausible] <- NA
        }
    }
    
    ##merge with other rows if only a subset is cleaned
    res <- data[variable]
    res[where] <- cleaned_column
    
    return(res)
}

# deprecated: incorporated into above
# print_cleaned <- function(var) {
#     print(paste("Percentage Cleaned (No. of cleaned/No. of non-NAs):",var,"(%)"))
#     if (length(clnList)==0) {
#         print("No records cleaned")
#     } else {
#         cln.table <- table(data[clnList,]$id_study)/table(data[!is.na(data[var]),]$id_study)*100
#         print(sort(round(cln.table[which(cln.table!=Inf&cln.table>0)],2), decreasing=TRUE))
#     }
# }

## convert_data ##
# convert data units
# column: variable in a data frame
# where: rows where the conversion should be applied: usually from a 'which' statement
# conversion_factor: as the name suggests
convert_data <- function(column, where, conversion_factor) {
    column[where] <- column[where] * conversion_factor
    return(column)
}

## clean_svydesign ## 
# this function stratifies the data based in: id_study, or id_study//age_mean//sex, or id_study//age_mean//sex//is_urban;
# tt then checks whether within a given study there is a mix of subjects with & without (NA) values 
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

## clean_single_psu_ssa ##
# cleaning single PSUs by study-sex-age (ssa) group
# single PSUs cause errors in svymean function call later
library(plyr)
clean_single_psu_ssa <- function(tmp){
    psu.count 	<- ddply(tmp[!is.na(tmp$psu),],.( id_study, age_mean, sex),function(tmp)length(unique(tmp$psu)))
    single.psu <- psu.count[psu.count$V1==1 & psu.count$id_study%in%c(as.character(psu_only),as.character(psu_wt)),] # single stratum, single psu 
    drop.singlepsu <- which(paste(tmp$id_study, tmp$age_mean, tmp$sex, sep="_")%in%paste(single.psu$id_study, single.psu$age_mean, single.psu$sex, sep="_"))
    print(length(drop.singlepsu))
    print(single.psu)
    
    if(length(drop.singlepsu)>0) {
        tmp <- tmp[-drop.singlepsu,]
    }
    return(tmp)
}