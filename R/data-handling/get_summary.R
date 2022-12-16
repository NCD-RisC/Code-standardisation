## get_summary_bmi ##
# For each study (id_study//sex//age) it calculates the mean of bmi (as continuous variable)
# and the mean of each prevalence group, across all subjects included within that
# study. This means that the SE for prevalences, are calcualted as for means

get_summary <- function(tmp, study = NULL) { #

  if (any(grepl("prev_bmi", colnames(tmp)))) {
    vars <- "bmi"
  } else if (any(grepl("height", colnames(tmp)))) {
    vars <- "height"
  } else {
    vars <- c("waist", "hip", "whr")
  }
  # print(paste(tmp$id_study[1], tmp$sex[1], tmp$age_mean[1]))

  ## Set options ##
  options(survey.lonely.psu = "adjust", survey.adjust.domain.lonely = TRUE)

  ## Check survey design variables ##
  if (any(is.na(tmp$samplewt_anthro))) { # If sample weights are missing set them to 1 (NA values not allowed in svydesign())
    if (all(is.na(tmp$samplewt_anthro)) == FALSE) { # Sample weights should be all NA, or all not NA
      stop("Sample weights are missing ONLY in some subjects")
    }
    tmp$samplewt_anthro <- 1
    res_wt <- TRUE #
  }
  if (any(is.na(tmp$psu))) {
    if (all(is.na(tmp$psu)) == FALSE) { # psu should be all NA, or all not NA
      stop("psu values are missing ONLY in some subjects")
    }
    res_psu <- FALSE # psu missing
  } else {
    res_psu <- TRUE # psu available
  }
  if (any(is.na(tmp$stratum))) {
    if (all(is.na(tmp$stratum)) == FALSE) { # stratum should be all NA, or all not NA
      stop("stratum values are missing ONLY in some subjects")
    }
    res_stratum <- FALSE # stratum missing
  } else {
    res_stratum <- TRUE # stratum available
  }

  ## Clean here for single_psu_ssa
  # Number of different psu (excluding NAs). If the study has only one psu and
  # stratum is not reported, it should be dropped. Else the function svydesign gives an error
  if (length(unique(na.omit(tmp$psu))) == 1) {
    res_psu <- FALSE # psu not available
    tmp$psu <- NA
    message("Study psu updated to NA because of single psu")
  }
  num.valid <- function(x) sum(!is.na(x) & x != -1) # sums values which are different from NA or -1 (valid subjects)

  results <- NULL
  for (var in vars) {
    var_clean <- paste(var, "clean", sep = "_")
    n_var <- num.valid(tmp[, var_clean])

    if (n_var > 1) {
      if (sum(res_psu, res_stratum) == 0) { # psu and stratum missing (if sw are not available, they're all 1)
        dsub <- svydesign(id = ~1, strata = NULL, weights = ~samplewt_anthro, data = tmp)
        # print("id = ~1, strata = NULL, weights = ~samplewt_anthro")
      } else if ((sum(res_psu, res_stratum) == 2)) { # we have psu and stratum
        dsub <- svydesign(id = ~psu, strata = ~stratum, weights = ~samplewt_anthro, data = tmp, nest = TRUE)
        # print("id = ~psu, strata = ~stratum, weights = ~samplewt_anthro")
      } else if (res_psu & !res_stratum) { # we have psu but not stratum
        dsub <- svydesign(id = ~psu, strata = NULL, weights = ~samplewt_anthro, data = tmp, nest = TRUE)
        # print("id = ~psu, strata = ~NULL, weights = ~samplewt_anthro")
      } else if (!res_psu & res_stratum) {
        dsub <- svydesign(id = ~1, strata = ~stratum, weights = ~samplewt_anthro, data = tmp, nest = TRUE)
        # print("id = ~1, strata = ~stratum, weights = ~samplewt_anthro")
      }

      var_m <- data.frame(svymean(~ tmp[, var_clean], dsub, na.rm = TRUE)) # gives mean and standard error
      var_sd <- as.numeric(sqrt(coef(svyvar(~ tmp[, var_clean], dsub, na.rm = TRUE)))) # gives only one value
      var_cols <- paste(c("N", "mean", "se", "sd"), var, sep = "_")

      # BMI and height also have prevalence
      if (var %in% c("bmi", "height")) {
        colnames(tmp) <- gsub(paste0(var, "_"), var, colnames(tmp))
        colnames(tmp) <- gsub(var, paste0(var, "_"), colnames(tmp))

        prev_names <- grep("prev_", colnames(tmp), value = TRUE)
        res <- as.data.frame(matrix(ncol = (length(prev_names) * 2 + 4), nrow = 1)) # empty df
        prev_cols <- unlist(lapply(prev_names, function(x) c(paste("se", x, sep = "_"), x)))
        colnames(res) <- c(var_cols, prev_cols)

        res[, var_cols] <- c(n_var, var_m[1, 1], var_m[1, 2], var_sd)

        for (prev in prev_names) {
          ans <- data.frame(svymean(~ tmp[, prev], dsub, na.rm = TRUE))
          res[, prev] <- ans[1, 1]
          res[, paste("se", prev, sep = "_")] <- ans[1, 2]
        }
      } else {
        # Waist does not have prevalence
        res <- as.data.frame(cbind(n_var, var_m[1, 1], var_m[1, 2], var_sd))
        colnames(res) <- var_cols
      }
    } else {
      # message(paste("Study excluded for", var, "as n <= 1"))
      res <- NULL
    }
    if (!is.null(res)) {
      if (is.null(results)) {
        results <- res
      } else {
        results <- data.frame(results, res)
      }
    }
  }
  return(results)
}
