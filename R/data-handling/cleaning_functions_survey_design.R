## Functions used for cleaning survey design variables ##
require(plyr)  # do not load to avoid conflict
ddply <- plyr::ddply

# clean_svydesign: NOTE taken from anthro_cleaning, need to go through
# svy_check
# clean_single_psu_ssa

## clean_svydesign ##
# this function stratifies the data based in: id_study, or id_study//age_mean//sex, or id_study//age_mean//sex//is_urban;
# then checks whether within a given study there is a mix of subjects with & without (NA) values
# for any survey design variable: psu, stratum, sample weight (each variable is explored separately).
# These studies where there is a mixed of subjects with & without (i.e. NA) values for
# the survey design variables are problematic because the svydesign() function can't deal
# with NA values. Therefore, subjects with NA values within these "problematic studies" must be removed.
clean_svydesign <- function(data, by = "survey") {
  if (grepl("S", rownames(data))[1] == FALSE) { # Make sure we have rownames
    rownames(data) <- paste("S", 1:nrow(data), sep = "")
    data$names <- rownames(data)
  }

  if (by == "age_gender") {
    sw_prob <- ddply(data, .(id_study, age_mean, sex), function(tmp) svy_check(tmp))
    psu_prob <- ddply(data, .(id_study, age_mean, sex), function(tmp) svy_check(tmp, var = "psu"))
    stratum_prob <- ddply(data, .(id_study, age_mean, sex), function(tmp) svy_check(tmp, var = "stratum"))
  } else if (by == "urban") {
    sw_prob <- ddply(data, .(id_study, age_mean, sex, is_urban), function(tmp) svy_check(tmp))
    psu_prob <- ddply(data, .(id_study, age_mean, sex, is_urban), function(tmp) svy_check(tmp, var = "psu"))
    stratum_prob <- ddply(data, .(id_study, age_mean, sex, is_urban), function(tmp) svy_check(tmp, var = "stratum"))
  } else { # Do not stratifiy
    sw_prob <- ddply(data, .(id_study), function(tmp) svy_check(tmp))
    psu_prob <- ddply(data, .(id_study), function(tmp) svy_check(tmp, var = "psu"))
    stratum_prob <- ddply(data, .(id_study), function(tmp) svy_check(tmp, var = "stratum"))
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
    stop("Problem in code")
  }
  indx_wanted <- setdiff(rownames(data), indx)

  to_print <- paste("Number of subjects removed due to survey design:", length(indx))
  data <- data[indx_wanted, ]

  return(data)
}


## svy_check ##
## Checks whether in tmp[, "var"] there is a mixed of subjects with reported values
# and subjects with missing values for "var"
svy_check <- function(tmp, var = "samplewt_anthro") {
    if (any(is.na(tmp[, var])) & !all(is.na(tmp[, var]))) {
        if (is.numeric(tmp[, var]) & var == "samplewt_anthro") {
            ans <- c(
                sum(is.na(tmp[, var])),
                round(100 * sum(is.na(tmp[, var])) / nrow(tmp), 2),
                range(na.omit(tmp[, var])),
                paste(tmp$names[(is.na(tmp[, var]))], collapse = "//")
            )
            names(ans) <- c("N_NA", "%_NA", "min_sw", "max_sw", "NA_idx")
            return(ans)
        } else {
            ans <- c(
                sum(is.na(tmp[, var])),
                round(100 * sum(is.na(tmp[, var])) / nrow(tmp), 2),
                paste(tmp$names[(is.na(tmp[, var]))], collapse = "//")
            )
            names(ans) <- c("N_NA", "%_NA", "NA_idx")
            return(ans)
        }
    } else {
        return(NULL)
    }
}

## clean_single_psu_ssa ##
# cleaning single PSUs by study-sex-age (ssa) group
# single PSUs cause errors in svymean function call later
clean_single_psu_ssa <- function(tmp) {
    psu.count <- ddply(tmp[!is.na(tmp$psu), ], .(id_study, age_mean, sex), function(tmp) length(unique(tmp$psu)))
    single.psu <- psu.count[psu.count$V1 == 1 & psu.count$id_study %in% c(as.character(psu_only), as.character(psu_wt)), ] # single stratum, single psu
    drop.singlepsu <- which(paste(tmp$id_study, tmp$age_mean, tmp$sex, sep = "_") %in% paste(single.psu$id_study, single.psu$age_mean, single.psu$sex, sep = "_"))
    print(length(drop.singlepsu))
    print(single.psu)

    if (length(drop.singlepsu) > 0) {
        tmp <- tmp[-drop.singlepsu, ]
    }
    return(tmp)
}
