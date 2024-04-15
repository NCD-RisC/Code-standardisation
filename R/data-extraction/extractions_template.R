## EXTRACTION TEMPLATE
## BIN ZHOU, 5 DEC 2017: created template
## BIN ZHOU, 8 JAN 2018: added "averaged_properly"
## RODRIGO, 04 SEPT 2018: added "head_house_education", "edu_father", "edu_mother", "ethnicity", "socioeconomic_status"
## Kate, 4 July 2022: changed "device_gl='Lab'" to"device_gl='NA'"
## Rosie July 2022 - added school age inclusion criteria and hb1c units
## Kate 16 August 2022 - added comment on extracting device metadata
## Victor 24 August 2022 - added comment on don't know for diagnosis
## Rachel 17 April 2023 - added fasting insulin
## Archie 27 September 2023 - edited comments for diabetes medications
## BZ 12 March 2024 - added hscrp and changed self_liver to string

# [NAME OF CREATOR]
# [DATE OF CREATION]
# Extraction of [NAME OF SURVEY]
# REMOVE UNNECESSARY COMMENTS AFTER COMPLETION
# CHECK VARIABLE NAMES IF UNCLEAR: https://docs.google.com/spreadsheets/d/1EE5uiRhAfLOE4JXHOu6sP0iHTIQ1P8CUh1DrnGMW3R8/edit?usp=drive_web

library(foreign)  # OR OTHER LIBRARY AS NEEDED (EG "readstata13", "sas7bdat", "Hmisc", "xlsx")
setwd("S:/Projects/HeightProject/Original dataset/Data/Surveys/COUNTRY_FOLDER/SURVEY_FOLDER")
data <- read.spss("SURVEY FILE", to.data.frame=TRUE, use.value.labels=FALSE)  # OR OTHER FUNCTIONS (EG "read.dta", "read.dta13", "read.xlsx", "read.csv")

# IF NAMES OF DATAFRAME HAS CAPITALISED LETTERS
names(data) <- tolower(names(data))

# OTHER PREPROCESSING STEPS AS NEEDED #

## SCHOOL based studies ONLY ##
## include other age groups if sample size are greater than the cut-off of "15% of average number of observations of target age groups".
#EG target = 7-10
((nrow(data[which(data$age>=7 &data$age<11),]))/4)*0.15 # cut-off (15% of mean sample size of target ages)
floor(table(data$age)) #check if any additional ages should be included


attach(data)
# USE SURVEY YEAR AS FILE NAME, EG d02 FOR MID_YEAR OF 2002
d02.info <- data.frame(id_study = "FIN_2002_Oulu45", iso = "FIN", country = "Finland",
                start_year = 2001, end_year = 2003, mid_year = 2002,
                survey = "Oulu 45 Study", survey_short = "Oulu45",
                survey_type = "Community", urban_rural = "urban",
                age_min_anthro_M = 55, age_max_anthro_M = 58,
                age_min_anthro_F = 55, age_max_anthro_F = 58,
                age_min_chol_M = 55, age_max_chol_M = 58,                       # REMOVE ROWS IF RELEVANT RF IS UNAVAILABLE
                age_min_chol_F = 55, age_max_chol_F = 58,
                age_min_bp_M = 55, age_max_bp_M = 58,
                age_min_bp_F = 55, age_max_bp_F = 58,
                age_min_glu_M = 55, age_max_glu_M = 58,
                age_min_glu_F = 55, age_max_glu_F = 58,
                age_min_kidney_M = 55, age_max_kidney_M = 58,
                age_min_kidney_F = 55, age_max_kidney_F = 58,
                age_min_liver_M = 55, age_max_liver_M = 58,
                age_min_liver_F = 55, age_max_liver_F = 58,
                age_min_othblood_M = 55, age_max_othblood_M = 58,   # for anemia and inflamatory markers
                age_min_othblood_F = 55, age_max_othblood_F = 58,

                is_plasma = 1,
                device_gl = NA, device_ha1c = NA, # either "Lab: device-name" or "Portable: device-name"
                device_lipids = NA, # either "Lab: device-name" or "Portable: device-name"
                method_tc = NA, # enzymatic, extraction, etc
                method_hdl_sep, # Heparin-Manganese, Dextran sulphate-Magnesium, polyethylene glycol, etc
                method_hdl_quan, # enzymatic etc
                is_ldl_calc = 0, is_ldl_fasting = 1,
                is_ldl_standard = NA, # not standardised, CDC/CRMLN, WHO, etc
                device_bp = "Omron", is_multi_cuff = 1, is_multi_bp = 1, averaged_properly = NA,
                drug_hyper_definition = NA,
                    # drug_hyper_definition = "questionnaire" for studies used questionnaires/self-report / "confirmed list" for studies used list of drugs and confirmed the purpose / "unconfirmed list" for studies that did not confirm purpose
                    # use averaged_properly only when multi BP was measured but only average was available to us (set value to 0), and the averages were not taken according to our protocol, e.g. they've recorded the mean of two measurements (instead of the later measurement)
                is_crea_idms = NA, # yes/no (1/0)
                crea_method = NA, # "jaffe", "enzymatic", "hplc", "other: specify"
                formula_egfr = NA # "mdrd", "ckd-epi", "cockcroft-gault", "other: specify"
                )

d02 <- data.frame(id = id_oulu45,                                               # USE "1:nrow(data)" IF UNAVAILABLE
                age = age_baseline, sex = sex, birth_y = year_of_birth, birth_m = month_of_birth,
                psu = NA, stratum = NA,
                samplewt_anthro = NA, samplewt_wh = NA, samplewt_smoke = NA,    # samplewt_smoke FOR INTERVIEW WEIGHTS IF AVAILABLE
                samplewt_bp = NA, samplewt_glu = NA, samplewt_chol = NA,        # samplewt_ppg = NA,  # samplewt_ppg FOR OGTT WEIGHTS IF AVAILABLE
                samplewt_kidney = NA, samplewt_liver = NA, samplewt_blood = NA,
                height = height, unit_height = "cm",
                weight = weight, unit_weight = "kg",
                waist = waist, unit_waist = "cm",
                hip = hip, unit_hip = "cm",
                is_urban = NA, edu = education_basic, occupation = NA,
                head_house_education = NA, edu_father = NA, edu_mother = NA,
                ethnicity = NA, income = NA, socioeconomic_status = NA,
                is_pregnant = NA,
                fgl = fasting_glucose, unit_gl = "mmol/L",
                ha1c = NA, unit_ha1c = "%", #must be as % or mmol/mol
                ppg = postprandial_glucose_2h, unit_ppg = "mmol/L",
                fasting_time = fasting_duration, # is_fasting = NA,		        # EXTRACT WHAT IS AVAILABLE
                self_diab = diagnosed_dm,
                drug_diab = NA, drug_diab_pill = use_of_glucose_lowering, drug_diab_insu = use_of_insulin, # EXTRACT ALL WHEN AVAILABLE TO CHECK FOR INCONSISTENCIES, AND CHECK DRUG_DIAB ISN'T ACTUALLY EITHER PILLS OR INSU
                self_diab_gp = NA,      # diagnosis history from GP record
                # self_diab_typ1, self_diab_typ2, self_diab_age, self_diab_year
                # drug_diab_class
                tc = fs_chol_tot, unit_tc = "mmol/L",
                ldl = fs_ldl, unit_ldl = "mmol/L", hdl = fs_hdl, unit_hdl = "mmol/L", trg  =  fs_trigly, unit_trg = "mmol/L",
                self_chol = NA,
                # self_chol_age, self_chol_year
                drug_chol = use_of_lipid_modifying,
                # drug_chol_stat = NA, drug_chol_fibr = NA,                     # USE OF STATIN AND FIBRATE, EXTRACT WHEN AVAILABLE
                sbp1 = syst_1, sbp2 = syst_2,                                   # ADD MORE (EG sbp3/dbp3) AS NEEDED
                dbp1 = diast_1, dbp2 = diast_2,
                # sbp_avg = NA, dbp_avg = NA,           # USE WHEN AND ONLY WHEN INDIVIDUAL BP MEASUREMENTS ARE UNAVAILABLE (NEED CONFIRMATION FROM COLLABORATOR)
                self_hyper = diagnosed_with_hypertension, # MAKE SURE IT IS NOT BASED ON SURVEY MEASUREMENTS
                self_hyper_preg = NA,    # diagnosis history of hypertension during pregnancy
                self_hyper_gp = NA,      # diagnosis history from GP record
                # self_hyper_age, self_hyper_year,
                drug_hyper = use_of_antihypertencive_drug,
                # drug_hyper_class

                smoker = current_smoking, smoke_ever = NA, smoke_num_curr = NA,
                drinker = NA, drink_ever = NA,
                # add other smoking/alcohol variables as needed

                self_cvd = NA,
                # self_cvd_age, self_cvd_year
                # add other disease history as needed (see variable list)

                crea = NA, unit_crea = NA,
                albumin = NA, unit_albumin = NA,
                egfr = NA, unit_egfr = NA,
                urine_crea = NA, unit_urine_crea = NA,
                albuminuria = NA, unit_albuminuria = NA,
                proteinuria = NA, unit_proteinuria = NA,
                uric_acid = NA, unit_uric_acid = NA,
                self_kidney = NA,
                # self_kidney_age, self_kidney_year
                drug_kidney = NA,
                # drug_kidney_class

                ast = NA, unit_ast = NA, alt = NA, unit_alt = NA,
                ggt = NA, unit_ggt = NA, alp = NA, unit_alp = NA,
                self_liver = NA,  # extract as string: specify the definition of liver disease in the study
                # self_liver_age, self_liver_year
                drug_liver = NA,
                # drug_liver_class
                hbsag = NA, ## 'yes' or 'no'
                hbsag_alt = NA, hbsag_alt_definition = NA,   # other definition including as continuous variable; please include unit in definition if relevant
                hcvab = NA, ## 'yes' or 'no'
                hbsag_alt = NA, hbsag_alt_definition = NA,   # as above

                hb = NA, unit_hb = NA,
                bilirubin = NA, unit_bilirubin = NA,
                ferritin = NA, unit_ferritin = NA,
                tibc = NA, unit_tibc = NA, mcv = NA, unit_mcv = NA,
                self_anemia = NA,
                # self_anemia_age, self_anemia_year

                crp = NA, unit_crp = NA,
                hscrp = NA, unit_hscrp = NA,
                fibrinogen = NA, unit_fibrinogen = NA,
                platelet = NA, unit_platelet = NA,
                ptime = NA, unit_ptime = NA,
                fast_insulin = NA, unit_insulin =NA

                # see standard variable list for additional measurement variables
                )
detach(data)
# ALWAYS "detach" AFTER "attach"

### RECODING AS NEEDED ###
### CHECK ESPECIALLY sex VARIABLE AND WHETHER 1/2 IS USED INSTEAD OF 1/0

### Always RECODE "don't know" as 0 for diagnosis

### COMPLETE SKIP PATTERNS AS NEEDED ###
### CHECK ESPECIALLY ALL self_xxx, drug_xxx AND smoking VARIABLES


### COMFIRM EVERYTHING IS OK BY RUNNING "summary(d02)"
### CHECK VARIABLE TYPES, VALUE RANGES AND MISSINGNESS

d02.final <- data.frame(d02.info, d02)
rm(d02, d02.info)

## CHECK EXTRACTIONS using script below; if multiple rounds run on all rounds after rbind
## Only is_study should be returned if extraction is correct
source("S:/Projects/HeightProject/Original dataset/Data/Surveys/__Extraction Template/Check extractions.R")
check_extraction(d02.final)

# Output
save(d02.final, file="Finland Oulu45 2001-2003.RData")  ## USE "COUNTRY SURVEY_NAME SURVEY_PERIODS" AS FILE NAME
# IF MULTIPLE YEARS, SAVE ALL SEPARATE YEARS AND THE COMBINED, EG:
# data <- rbind(d01, d02, d03)
# save(d01, d02, d03, data, file="Finland Oulu45 2001-2003-year.RData")
write.csv(d02.final, "Finland Oulu45 2001-2003.csv", row.names=FALSE)
write.csv(d02.final, "S:/Projects/HeightProject/Original dataset/Data/Surveys/Extracted Survey/COUNTRY SURVEY_NAME SURVEY_PERIODS.csv", row.names=FALSE)
