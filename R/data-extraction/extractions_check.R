## Rosie May 2023
## Bin Jan 2024: added condition for checking age range; added checks for value constraints for BP and waist/hip

### Script to check extractions before saving

library(insight)

std_names_list <- read.csv("../data/standard_variable_names.csv")
# we use ha1c in extraction but hba1c downstream - rename hba1c variables to ha1c for these checks
std_names_list$Name[std_names_list$Name == "device_hba1c"] <- "device_ha1c"
std_names_list$Name[std_names_list$Name == "unit_hba1c"] <- "unit_ha1c"
std_names_list$Name[std_names_list$Name == "hba1c"] <- "ha1c"
numeric_var_list <- as.character(std_names_list$Name[which(std_names_list$Type == "numeric")])

country <- read.csv("../data/country-list-2023-new.csv")

check_extraction <- function(dataset) {
  print <- function(x, col = "white", indent = 0) print_color(paste0(strrep(" ", indent), paste(x, collapse = ", "), "\n"), color = col)

  stud_id <- unique(as.character(dataset$id_study))

  if (sum(names(dataset) == 'id_study') == 0) {
    stop("File missing id_study")
  }

  for (i in stud_id) {
    dat <- subset(dataset, dataset$id_study == i)

    print(paste("CHECKING ",i), "yellow")

    if (any(unique(dat$iso) != substr(i, 1, 3))) {
      print("CHECK - inconsistent ISO with id_study:", "br_red")
      print(paste(i, "vs", paste(unique(dat$iso), collapse = ', ')))
    }

    if (any(!unique(dat$iso) %in% country$iso)) {
      print("CHECK - ISO not in country list:", "br_red")
      print(paste(i, "vs", paste(unique(dat$iso), collapse = ', ')))
    } else {
      if (any(!unique(dat$country) %in% country$Country[country$iso %in% unique(dat$iso)])) {
        print("CHECK - country name not in country list:", "br_red")
        print(paste(unique(dat$country), collapse = ', '))
      }
    }

    if (any(unique(dat$mid_year) != as.numeric(substr(i, 5, 8)))) {
      print("CHECK - inconsistent mid_year with id_study:", "br_red")
      print(paste(i, "vs", paste(unique(dat$mid_year), collapse = ', ')))
    }

    if (any(unique(dat$survey_short) != substr(i, 10, nchar(i)))) {
      print("CHECK - inconsistent survey_short with id_study:", "br_red")
      print(paste(i, "vs", paste(unique(dat$survey_short), collapse = ', ')))
    }

    if (any(!unique(dat$survey_type) %in% c("National", "Subnational", "Community", "mixed", "Mixed"))) {
      print("CHECK - non-standard coding for survey_type:", "br_red")
      print(paste(unique(dat$survey_type), collapse = ', '))
    }

    if (any(!unique(dat$urban_rural) %in% c("urban", "rural", "both", "mixed", "Mixed"))) {
      print("CHECK - non-standard coding for urban_rural:", "br_red")
      print(paste(unique(dat$urban_rural), collapse = ', '))

    }

    if (any(grepl("\\.1", names(dat)))) {
      print("CHECK - variables with '.1' in names indicating duplicated columns in extraction", "br_red")
      print(grep("\\.1", names(dat), value = TRUE))
    }

    classes <- lapply(dat[, names(dat) %in% numeric_var_list], class)
    non_numeric_list <- names(classes[!classes %in% c("numeric", "integer", "logical")])
    if (length(non_numeric_list) > 0) {
      print("CHECK - numerical variables in non-numeric formats:", "br_red")
      print(non_numeric_list)
    }
    if(any(!dat$sex %in% c(1,2) & !is.na(dat$sex))){
      print("CHECK - sex miscoding", "br_red")
    }
    if(any(is.na(dat$sex))){
      if(length(unique(dat$sex)) ==2){
        print("CHECK - only one value for sex and NA: is sex coded correctly?")
      }
    }

    if(any(is.na(dat[,grepl('age_min|age_max',names(dat))]))){
      if(length(unique(dat$sex)) ==2){
        print("CAUTION - age_min and age_max variables have NA in them: please verify this is intended", "br_violet")
      }
    }

    #check there are ages in data
    if(sum(!is.na(dat$age)) == 0){
      if (!'age_mean' %in% names(dat) | ('age_mean' %in% names(dat) & sum(!is.na(dat$age_mean)) == 0)) {
        print("CHECK - age missing for all", "br_red")
      }
    }

    # check height data is not in metre
    if('height' %in% names(dat) & sum(!is.na(dat$height)) > 0 ){
      if (mean(dat$height, na.rm=TRUE)<80) print("CHECK - mean value for height too low: is it extracted in metre or inch?", "br_red")
    } else if ('height1' %in% names(dat) & sum(!is.na(dat$height1)) > 0) {
      if (mean(dat$height1, na.rm=TRUE)<80) print("CHECK - mean value for height too low: is it extracted in metre or inch?", "br_red")
    }

    # check sex if height data available
    if(any(dat$sex %in% c(1) &  dat$sex %in% c(2))){
      if (any(grepl("height", names(dat)))) {
        if (!"height" %in% names(dat)) {
          dat$height <- dat$height1
        }
        m_height <- mean(dat$height[which(dat$height>0 & dat$sex == 1)])
        f_height <- mean(dat$height[which(dat$height>0 & dat$sex == 2)])
        if (m_height <= f_height) {
          print("CHECK - male height smaller than female height: is sex coded correctly?", "br_red")
          base::print(tapply(dat$height, dat$sex, mean, na.rm=TRUE))
        }
      }
    }

    # check variable value constraints, allowing for multiple measures
    # in each pair c(A,B), check if A > B on average
    check_list <- list(c("sbp","dbp"), c("hip","waist"))
    for (pair in check_list) {
      vars1 <- sort(grep(paste0("^",pair[1],"(([0-9]+$)|($)|(_avg$))"), names(dat), value=TRUE))
      vars2 <- sort(grep(paste0("^",pair[2],"(([0-9]+$)|($)|(_avg$))"), names(dat), value=TRUE))

      if (!identical(gsub(pair[1],"", vars1), gsub(pair[2],"", vars2))) {
        print("CHECK - unexpected variable name patterns: are variables named correctly?", "br_red")
        print(vars1, indent = 2)
        print(vars2, indent = 2)
        next
      }

      if (length(vars1) == 0) next

      failed_list <- c()
      for (i in 1:length(vars1)) {
        v1 <- dat[[vars1[i]]]
        v2 <- dat[[vars2[i]]]
        if (sum(!is.na(v1)) == 0 & sum(!is.na(v2)) == 0) next
        if ((sum(!is.na(v1)) * sum(!is.na(v2)) == 0)) {
          if (pair[1] != 'hip') {
            print(paste0("CHECK - only one of ", vars1[i], " and ", vars2[i], " exists in extraction", "br_red"))
          }
          next
        }
        if (mean(v1[v1>0], na.rm=TRUE) <= mean(v2[v2>0], na.rm=TRUE)) {
          failed_list <- c(failed_list, paste(vars1[i], '<=', vars2[i]))
        }
      }

      if (length(failed_list)>0) {
        print("CHECK - the following variables have unexpected relationships (the former should always > the latter):", "br_red")
        print(failed_list, indent = 2)
      }

    }

    ## Check if urban_rural is 0 or 1 and urban_rural isn't "both"

    if(unique(dat$iso) == "TKL" & !(unique(dat$urban_rural) == "both")){
       print("CHECK - iso is TKL which has perurb=0, so we should have urban_rural = both", "br_red")
    } else if ((unique(dat$iso) == "NRU" | unique(dat$iso) == "BMU" | unique(dat$iso) == "BMU" | (unique(dat$iso) == "SGP" & unique(dat$mid_year) > 2001)) & !(unique(dat$urban_rural) == "both")){
          print("CHECK - perurb=1 for survey and country-year, so we should have urban_rural = both", "br_red")
       }

    ## TODO: check if 0/1 variables are coded as 0/1
    ## TODO: check unit variables: we can add correct units to the standard variable list - start from just the anthro/lipids/glucose variables
    #        eg anthro has to be metric (and cm instead of m), biomarkers have to be either mmol/L or mg/dL (I think we allow mg/dl too), a1c in % or mmol/mol

    classes <- subset(classes, classes %in% c("numeric", "integer"))
    classes <- classes[!grepl('^age_m|^is_ldl|^is_multi|_year|is_plasma', names(classes))] #remove metadata columns
    col <- data.frame(sum = colSums(dat[names(classes)], na.rm = TRUE), var = names(classes))
    col <- subset(col, !col$sum %in% c(0, 1)) # remove as likely NA or metadata with 0/1 coding
    if (any(duplicated(col$sum)) == TRUE) {
      print("CAUTION - columns with the same sum - was the same variable extracted twice?", "br_violet")
      print("Look out for measurement columns below:", indent = 2)  # to downgrade message to grey if only samplewt variables
      dup_sum <- col$sum[duplicated(col$sum)]
      print(col$var[col$sum %in% dup_sum], indent = 2)
    }

    nonstd_name <- setdiff(names(dat)[!names(dat) %in% std_names_list$Name & !grepl("\\.1", names(dat))], "mod_time")
    if (length(nonstd_name) > 0) {
      print("CHECK - non-standard variable names found:", "br_red")
      print(nonstd_name, indent = 2)
    }
    #col01 <- setdiff(grep("is_|self|averaged", colnames(dat), value = T),grep("_age|_year|standard", colnames(dat), value = T))
    #for(i in col01) {
    #  if(any(!dat[,i] %in% c(0,1)))
    #   print(i)
    #}

    print("DONE", "yellow")
  }
}

