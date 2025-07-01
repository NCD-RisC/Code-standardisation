#adapted for diabetes initial runs AR April 2023
#Adapted by BZ for dual liklihood 3/7/23
#adapted by AR for Zeta naming and cross walk variance term 20/7/23
#adapted by AR for prior on zeta 24/07/23
# AR April 2024 includes multi study ids

################################################################################
# This code fits a Bayesian hierarchical model to metabolic risk factor data   #
# using MCMC. The model includes linear and non-linear components, an age      #
# model and a study-coverage covariate model. The hierarchy has four levels:   #
# country, region, 'superregion' and world, which allows borrowing of strength #
# from countries and years with data to inform estimates for countries and     #
# years without data.                                                          #
#                                                                              #
# This version builds on code used to fit the model to mean BMI data to        #
# estimate national, regional and global BMI trends for NCD Risk Factor        #
# Collaboration "Trends in adult body-mass index in 200 countries from 1975 to #
# 2014: a pooled analysis of 1698 population-based measurement studies with    #
# 19.2 million participants", Lancet 2016 (available for download at           #
# www.ncdrisc.org). Full details are provided in Finucane et al "Bayesian      #
# estimation of population-level trends in measures of health status",         #
# Statistical Science 2014, and in the appendix of Danaei et al "National,     #
# regional, and global trends in systolic blood pressure since 1980:           #
# systematic analysis of health examination surveys and epidemiological        #
# studies with 786 country-years and 5.4 million participants", Lancet 2011.   #
#                                                                              #
# The extension to the model allows estimation of trends to vary by age in a   #
# more flexible way, by introducing hierarchical linear time components in the #
# age model. The model was used in the publication "Worldwide trends in        #
# hypertension prevalence and progress in treatment and control from 1990 to   #
# 2019: a pooled analysis of 1,201 population-representative studies with      #
# 104 million participants", Lancet 2021.                                      #
################################################################################


################################################################################
# We have chosen to use a line width of more than 80 characters for some of    #
# the code, so that the whole of particular sections of the code can be seen   #
# at the same time. On our laptops and desktops the code displays most clearly #
# in Windows using Programmer's Notepad, and in Linux using gedit.             #
################################################################################




##### USER DEFINED INPUTS ######################################################
##### Users should set appropriate values for the objects below                #
################################################################################
#setwd('D:/Models/Model Variations/prev/OG/Dual likelihood/prior-on-z')
#seedVal             <- 1                                                                  # Assign each chain a unique seed value
#sex                 <- "female"                                                           # Set gender: must be either "male" or "female"

####################
name_var            <- "diabetes_prevalence"                                    # Choose label for model results: "diabetes_prevalence" or "diabetes_treatment"
variable            <- "prev_diabetes"                                          # Set variable name when components are not available
components          <- c("prev_treated_diabetes", "prev_diabetes_among_untreated")  # Set variable names for the components that make up the variable. 'components' = c(q,r) such that variable of interest = q + (1-q)*r
n_variable          <- "n3"                                                     # Set variable name for sample size of diabetes when components are not available
n_components        <- c("n1", "n2")                                            # Set variable names for sample sizes of treated and untreated diabetes when components are available
cwvar_probit_var    <- paste0("add_var_", name_var)                             # Set additional variance variable (arising from our use of regression models to estimate diabetes prevalence)
mod.no              <- 110                                                               # Set a unique model identifier: this can be any number or a character string
start.year          <- 1980                                                             # Set start year for analysis
end.year            <- 2022# Set end year for analysis
minimum.age         <- 18                                                              # Set minimum age of analysis (included)
maximum.age         <- 200                                                              # Set maximum age of analysis (not included)
knot1               <- 45                                                               # Position of first knot in cubic splines
middle.age          <- 55                                                               # Middle age in analysis
knot2               <- 60                                                               # Position of second knot in cubic splines
centring.year       <- 2001                                                            # Used to centre time
nLong               <- 75000                                                            # Set number of iterations for MCMC loop
epsilon             <- 0.001                                                            # Flat priors on the precision scale
freq.val            <- 200                                                              # Number of iterations per tuning step in the burn-in iterations
covariate.file.name <- "covariates-urbanisation-2021.csv"                                      # Set covariate filename
data.file.name      <- "model_ready_glucose_data.csv"                                   # Set data filename
country.file.name   <- "country-list-2023-new.csv"


outdirname <- paste0(name_var, "_", sex, "/")                                           # Set up output directory
dir.create(outdirname, showWarnings = FALSE)

##### PRELIMINARY CODE #########################################################
set.seed(seedVal)                                                                       # Sets random number seed using seedVal object
sex.val             <- switch(sex, male = 1, female = 2)                                # Sets a numerical variable for sex: 1 for male and 2 for female
nIts                <- nLong/10                                                         # Number of MCMC iterations after thinning
filename            <- paste0("Model",mod.no,"_",sex,"_",name_var,"_Seed_",seedVal)     # Sets a base filename used throughout the code
# The code uses the 'spam' library, which is a collection of functions for
# sparse matrix algebra
# https://cran.r-project.org/web/packages/spam/spam.pdf
library(spam)                                                                           # Loads spam library
options(spam.cholsymmetrycheck=FALSE,spam.structurebased=FALSE)                         # Sets options for spam library
library(zoo)                                                                            # Used for the 'index' function
library(TTR)                                                                            # Used for the 'TenYearWeightedAvg' function
source("1_functions.R")                            # Loads functions



##### INDEXING INPUT ###########################################################
##### Indexing and covariate file must be a csv file with the following columns#
##### iso: standard ISO3 three-letter codes                                    #
##### country: text strings, which must match country names in data file       #
##### region: text strings, which must match region names in data file         #
##### sregion: text strings, which must match superregion names in data file   #
##### data_year: integers                                                      #
##### perurb: numeric between 0 and 1 representing percent national population #
#####   living in urban areas                                               #
################################################################################
covar                   <- read.csv(covariate.file.name, header=TRUE)                               # Load covariate dataset
country.names           <- read.csv(country.file.name, header = TRUE)
covar                   <- merge(covar[,!names(covar) %in% c('country','region','sregion')],country.names,by=c("iso"),all.x=TRUE)
covar                   <- covar[order(covar$Region,covar$Country,covar$data_year),]                  # Sort covariate dataset by region, country and year
covar                   <- subset(covar, data_year >= start.year & data_year <= end.year)             # Subset this object by analysis period



##### DATA INPUT ###############################################################
##### The data are urban or rural place of residence-, age- & sex-stratified   #
##### summary statistics for each study. Data for studies that could not be    #
##### stratified by place of residence are age- & sex-stratified summary       #
##### statistics.                                                              #
#####                                                                          #
##### The data file must be a csv file with the following columns:             #
##### id_study: alphanumeric string (we suggest using only A-Z, 0-9 and _). We #
#####   suggest "iso3 code"_"data collection mid-year"_"survey name"           #
##### DATA CHARACTERISTICS                                                     #
##### sex: must be either "male" or "female", as the two sexes are modelled    #
#####   separately                                                             #
##### age: positive number >=18 for adult-only analyses; 'age' is the mid-age  #
#####   of the age group for this datapoint                                    #
##### mean_bmi: positive number                                                #
##### se_bmi: positive number                                                  #
##### STUDY CHARACTERISTICS                                                    #
##### mid_year: integer year, as required by the non-linear component of the   #
#####   model                                                                  #
##### survey_type: "National", "Subnational" or "Community"; used in the       #
#####   analysis to account for possible bias in non-national studies          #
##### urban_rural: "urban", "rural" or "both"; used in the analysis to account #
#####   for possible bias in urban-only or rural-only studies                  #
##### Country: country names, which must match indexing file                   #
##### Region: region names, which must match indexing file                     #
##### Superregion: superregion names, which must match indexing file           #
################################################################################
data                    <- read.csv(data.file.name, header = TRUE, stringsAsFactors = FALSE)  # Load dataset

data$flag        <- NA
data$flag[which(!is.na(data$prev_treated_diabetes) & !is.na(data$prev_diabetes_among_untreated))] <- 1 # When treated and untreated diabetes components are available
data$flag[which((is.na(data$prev_treated_diabetes) | is.na(data$prev_diabetes_among_untreated)) & !is.na(data$prev_diabetes))] <- 0 # When components are unavailable
data             <- data[which(data$flag %in% c(0,1)), ]                                               # Filter to just rows with available data that can be used in model

data$id_study           <- as.character(data$id_study)
data$mid_year           <- ifelse(data$mid_year >= (start.year - 3) & data$mid_year < start.year &
                                    data$survey_type == "National", start.year, ifelse(data$mid_year > (end.year) &
                                                                                         data$survey_type == "National", end.year,data$mid_year))                          # Include national studies from up to 3 years before the minimum data year

# get the n/p for total and the components
n_var <- gsub('prev', 'n', gsub('mean', 'n', variable))
data$number             <- round(data[,n_var])
data$prev               <- data[,variable]
n_comp_var <- gsub('prev', 'n', gsub('mean', 'n', components))
data$number_q             <- round(data[,n_comp_var[1]])
data$number_r             <- round(data[,n_comp_var[2]])
data$prev_q               <- data[,components[1]]
data$prev_r               <- data[,components[2]]

data$cwvar_probit <- data[,cwvar_probit_var]
data$flag <- data[,flag_var]

# Added by NP in case ISO is missing from data
if (!('iso' %in% names(data))){
  data<- merge(data, country.names, by = c("Country","Region","Superregion"))
}


data     <- data[,c("id_study","mid_year","iso","survey_type","urban_rural",
                    "sex","age","number","prev","number_q","prev_q","number_r","prev_r", "cwvar_probit","flag")] # Columns used in analysis
colnames(data)[colnames(data)=="mid_year"] <- "data_year"



data     <- subset(data, data_year>=start.year & data_year<=end.year &                                          # Subset to years within range
                     age >= minimum.age & age < maximum.age)                                 # Subset to ages within range
data     <- subset(data, (number >1 & !is.na(prev) | (number_q > 1 & !is.na(prev_q) & number_r > 1 & !is.na(prev_r))))                                                              # Subset to rows with data

subset   <- subset(data, sex==sex.val)     # Subset data for gender being modelled

### we get 6 missing values after the next step, i.e there are 6 data_years/isos that are different
## because HKG is now counted as CHN
##change this
subset[subset$iso == 'HKG',c('iso')] <- 'CHN'
subset   <- merge(subset,covar,by=c("data_year","iso"),all.x=TRUE) # Add country-region lookup info and perurb data
subset   <- subset[order(subset$Superregion,subset$Region,subset$Country,subset$data_year,subset$id_study),]

# subset number/prev variable for relevant rows: flag = 0 for studies with 1-piece info and flag = 1 for those with 2-piece info
flag     <- subset$flag 
number   <- subset$number[subset$flag == 0]
prev     <- subset$prev[subset$flag == 0]
number_q <- subset$number_q[subset$flag == 1]
prev_q   <- subset$prev_q[subset$flag == 1]
number_r <- subset$number_r[subset$flag == 1]
prev_r   <- subset$prev_r[subset$flag == 1]
y_P      <- round(prev * number)
y_Q      <- round(prev_q * number_q)
y_R      <- round(prev_r * number_r)

subset   <- subset[,c("iso", "id_study", "sex", "age", "cwvar_probit", #"flag",#"number", "prev", "number_q", "prev_q", "number_r", "prev_r", "flag",
                      "data_year", "survey_type", "urban_rural", "Country", "Region", "Superregion", "perurb")]
colnames(subset) <- c("iso","uid","sex.final","age", "cwvar_probit",#"flag",#"number","prev",
                      "data_year","coverage","scope","country","region","sregion", "perurb")



# Rename columns for analysis
attach(subset)

##### Derive variables for the age model #######################################
t                       <- (start.year:end.year) - centring.year                                                # Centred list of analysis years
time                    <- data_year - centring.year                                                            # Centred timepoints in data
age                     <- age - middle.age                                                                     # Age centred by middle age
age.knot1               <- age + middle.age - knot1; age.knot1[age < -middle.age + knot1] <- 0                  # Age using first knot
age.knot2               <- age + middle.age - knot2; age.knot2[age < -middle.age + knot2] <- 0                  # Age using second knot
ageMat                  <- cbind(age, age^2, age^3, age.knot1^3, age.knot2^3)                                   # Age matrix, mapping datapoints to ages
ageMat.prime            <- t(ageMat)                                                                            # Transposed age matrix
ng                      <- ncol(ageMat)

##### Calculate constants ######################################################
I                       <- nrow(subset)                                 # Number of survey-year-sex-age datapoints
J                       <- length(unique(covar$Country))                # Number of countries for which estimates are made (as opposed to number of countries in the dataset)
K                       <- length(unique(covar$Region))                 # Number of regions for which estimates are made
L                       <- length(unique(covar$Superregion))            # Number of superregions for which estimates are made
N                       <- length(unique(uid))                          # Number of studies in data
T                       <- length(t)                                    # Length of analysis period




##### RANDOM WALK CONSTANTS ####################################################
##### The following defines the penalty matrix and contraints used to capture  #
##### and differentiate between non-linear trends at country, region, and      #
##### superregion levels. This is described in Danaei et al Webappendix, p.5-7 #
##### Create penalty matrix ####################################################
##### See Rue and Held, p.110, and Danaei et al Webappendix, p.6
P                       <- matrix(0, T, T)
P[1, (1:3)]             <- c(1, -2, 1)
P[2, (1:4)]             <- c(-2, 5, -4, 1)
for(i in 3:(T-2)) {
    P[i, ((i-2):(i+2))] <- c(1, -4, 6, -4, 1)
    }
P[T-1, (T-3):T]         <- c(1, -4, 5, -2)
P[T, (T-2):T]           <- c(1, -2, 1)

##### Eigenanalysis for countries with no data #################################
##### Random walk with no data - see Danaei et al Webappendix p.15
eigenNoData             <- eigen(P)
eigenNoData$values[T-1] <- eigenNoData$values[T] <- Inf
EigenValGenInv          <- eigenNoData$val
EigenValGenInv[(T-1):T] <- 0
SigmaGenInvNoTheta      <- eigenNoData$vec %*% (EigenValGenInv * t(eigenNoData$vec))

##### Constraints for random walk ##############################################
##### See Danaei et al Webappendix p.15
##### Allows identifiabilty of linear random intercepts and slopes
A                       <- matrix(1, 2, T)
A[2,]                   <- t - mean(t)




##### MAPPING MATRICES AND VECTORS #############################################
################################################################################
##### Matrices with lists of study IDs, countries, regions and all possible    #
##### combinations of country & year, region & year and superregion & year for #
##### which estimates are made                                                 #
################################################################################
uid.match                   <- data.frame(name=unique(uid)[order(unique(uid))],number=1:N)                      # List of all study IDs
uid.match$coverage          <- coverage[match(uid.match$name,uid)]                                              # Adds coverage to list of all study IDs
country.match               <- data.frame(name=unique(covar$Country)[order(unique(covar$Country))], number=1:J)
region.match                <- data.frame(name=unique(covar$Region)[order(unique(covar$Region))], number=1:K)
sregion.match               <- data.frame(name=unique(covar$Superregion)[order(unique(covar$Superregion))], number=1:L)
country.time.match          <- data.frame(country = rep(as.character(sort(unique(covar$Country))),each=T), year = as.numeric(rep(start.year:end.year,J)), number = 1:(J*T))
region.time.match           <- data.frame(region = rep(as.character(sort(unique(covar$Region))),each=T), year = as.numeric(rep(start.year:end.year,K)), number = 1:(K*T))
sregion.time.match          <- data.frame(sregion = rep(as.character(sort(unique(covar$Superregion))),each=T), year = as.numeric(rep(start.year:end.year,L)), number = 1:(L*T))

##### Derive mapping matrices and vectors
which.uid                   <- match(uid,uid.match$name)                                                        # Identifies position of each datapoint's study ID in the ID list
which.country               <- match(country,country.match$name)                                                # Identifies position of each datapoint's country in the country list
which.region                <- match(region,region.match$name)                                                  # Identifies position of each datapoint's region in the region list
which.sregion               <- match(sregion,sregion.match$name)                                                # Identifies position of each datapoint's superregion in sregion (factor)
which.time                  <- time - min(time) + 1                                                                 # Identifies position of each datapoint's year in list of years
which.countryTime           <- match(paste(country,data_year),paste(country.time.match$country,country.time.match$year)) # Identifies position of each datapoint's country-year
which.regionTime            <- match(paste(region,data_year),paste(region.time.match$region,region.time.match$year))     # Identifies position of each datapoint's region-year
which.sregionTime           <- match(paste(sregion,data_year),paste(sregion.time.match$sregion,sregion.time.match$year)) # Identifies position of each datapoint's superregion-year
multipleRegionsInSregion    <- as.logical(!(table(covar$Region,covar$Superregion)[,(1:L)[colSums(table(covar$Region,covar$Superregion)>0)==1]])>0)
                                                                                                               # Identifies superregions comprising multiple regions
Z                           <- sum(multipleRegionsInSregion)                                                    # Number of regions in multi-region superregions
index_multiRinSR            <- which(multipleRegionsInSregion)                                                  # index of regions in multi-region superregions

country.table               <- table(index(which.country),which.country)                                        # Table identifying countries corresponding to each datapoint
StudyToCountry              <- StudyToCountry.time <- matrix(0, I, J)                                           # Matrix mapping datapoints to countries
StudyToCountry[,as.numeric(colnames(country.table))] <- country.table[,as.numeric(index(colnames(country.table)))]
                                                                                                                # Matrix mapping datapoints to countries
StudyToCountry.time         <- StudyToCountry * time                                                            # Matrix mapping datapoint-time combinations to countries
StudyToRegion               <- table(index(region),region)                                                      # Matrix mapping datapoints to regions
StudyToRegion.time          <- table(index(region),region) * time                                               # Matrix mapping datapoint-time combinations to regions
StudyToSregion              <- table(index(sregion),sregion)                                                    # Matrix mapping datapoints to superregions
StudyToSregion.time         <- table(index(sregion),sregion) * time                                             # Matrix mapping datapoint-time combinations to superregions
StudyToUid                  <- table(index(which.uid),which.uid)                                                # Matrix mapping datapoints to study IDs
StudyToUid.time             <- table(index(which.uid),which.uid) * time                                         # Matrix mapping datapoint-time combinations to study IDs




##### Create "F" matrix ########################################################
##### This matrix is described in the appendix of Danaei et al, p.12           #
##### It maps each linear and nonlinear component, covariates and the study-   #
##### specific random effects to the corresponding parameters in the theta     #
##### matrix                                                                   #
################################################################################
F <- cbind (
    rep(1, I),                                                                      # Column in F matrix corresponding to global intercept
    time,                                                                           # Column in F matrix corresponding to global slope
    StudyToSregion,                                                                 # Columns in F matrix corresponding to superregion intercepts
    StudyToSregion.time,                                                            # Columns in F matrix corresponding to superregion slopes
    StudyToRegion[,multipleRegionsInSregion],                                       # Columns in F matrix corresponding to region intercepts
    StudyToRegion.time[,multipleRegionsInSregion],                                  # Columns in F matrix corresponding to region slopes
    StudyToCountry,                                                                 # Columns in F matrix corresponding to country intercepts
    StudyToCountry.time,                                                            # Columns in F matrix corresponding to country slopes
    StudyToUid,                                                                     # Columns in F matrix corresponding to study random effects
    as.numeric(coverage=="Subnational"),                                            # Column in F matrix corresponding to subnational offset
    as.numeric(coverage=="Subnational")*time,                                       # Column in F matrix corresponding to subnational slope offset
    as.numeric(coverage=="Community"),                                              # Column in F matrix corresponding to community offset
    as.numeric(coverage=="Community")*time,                                         # Column in F matrix corresponding to community slope offset
    as.numeric(scope=="rural")*perurb,                                              # Column in F matrix corresponding to rural offset
    as.numeric(scope=="rural")*perurb*time,                                         # Column in F matrix corresponding to rural slope offset
    as.numeric(scope=="urban")*(1-perurb),                                          # Column in F matrix corresponding to urban offset
    as.numeric(scope=="urban")*(1-perurb)*time)                                     # Column in F matrix corresponding to urban slope offset

p                <- dim(F)[2] - (2+2*L+2*Z+2*J+N)                                   # Number of covariate parameters in model
F.prime          <- t(F)                                                            # Object derived from the F matrix for use below
F.spam           <- as.spam(F)                                                      # Object derived from the F matrix for use below
F.spam.prime     <- t(F.spam)                                                       # Object derived from the F matrix for use below
F.age            <- F*age                                                           # Object derived from the F matrix for use below
F.spam.age       <- as.spam(F.age)                                                  # Object derived from the F matrix for use below
F.age.prime      <- t(F.age)                                                        # Object derived from the F matrix for use below
F.spam.age.prime <- t(F.spam.age)                                                   # Object derived from the F matrix for use below




##### INITIAL VALUES ###########################################################
##### The following lines create empty vectors/matrices for model parameters   #
##### Parameter names in parentheses are used in Finucane et al & Danaei et al #
#####                                                                          #
##### NB. Those papers use 's' for subregion (equivalent to region here)       #
##### and 'r' for region (equivalent to superregion here)                      #
################################################################################
phi_s                   <- rep(NA, nIts)                # Empty vector for log variance for normal prior for superregion random intercepts (log kappa_a^r)
phi_r                   <- rep(NA, nIts)                # Empty vector for log variance for normal prior for region random intercepts (log kappa_a^s)
phi_c                   <- rep(NA, nIts)                # Empty vector for log variance for normal prior for country random intercepts (log kappa_a^c)
eta_s                   <- rep(NA, nIts)                # Empty vector for log variance for normal prior for superregion random slopes (log kappa_b^r)
eta_r                   <- rep(NA, nIts)                # Empty vector for log variance for normal prior for region random slopes (log kappa_b^s)
eta_c                   <- rep(NA, nIts)                # Empty vector for log variance for normal prior for country random slopes (log kappa_b^c)
phi_natl                <- rep(NA, nIts)                # Empty vector for log variance of ssre for national studies (log nu_"national")
phi_subn                <- rep(NA, nIts)                # Empty vector for log variance of ssre for subnational studies (log nu_s)
phi_comm                <- rep(NA, nIts)                # Empty vector for log variance of ssre for community studies (log nu_c)
tau                     <- rep(NA, nIts)                # Empty vector for log variance for within-study errors that differ between age groups (log tau)
theta_c                 <- rep(NA, nIts)                # Empty vector for log precision parameter for random walk at country level (log lambda_c)
theta_r                 <- rep(NA, nIts)                # Empty vector for log precision parameter for random walk at region level (log lambda_s)
theta_s                 <- rep(NA, nIts)                # Empty vector for log precision parameter for random walk at superregion level (log lambda_r)
theta_g                 <- rep(NA, nIts)                # Empty vector for log precision parameter for random walk at global level (log lambda_g)
deviance                <- rep(NA, nIts)                # Empty vector for deviance
gamma                   <- matrix(NA, nIts, 2*ng + 2*ng*L + 2*ng*Z + 2*ng*J)    # Empty matrix for age model parameters
sigma1_c                <- matrix(NA, nIts, ng)         # Empty matrix for variances for country-specific random intercept in spline coefficients (log c)
sigma2_c                <- matrix(NA, nIts, ng)         # Empty matrix for variances for country-specific random slope in spline coefficients (log omiga_c)
sigma1_r                <- matrix(NA, nIts, ng)         # Empty matrix for variances for region-specific random intercept in spline coefficients (log r)
sigma2_r                <- matrix(NA, nIts, ng)         # Empty matrix for variances for region-specific random slope in spline coefficients (log omiga_r)
sigma1_s                <- matrix(NA, nIts, ng)         # Empty matrix for variances for superregion-specific random intercept in spline coefficients (log s)
sigma2_s                <- matrix(NA, nIts, ng)         # Empty matrix for variances for superregion-specific random slope in spline coefficients (log omiga_s)
theta                   <- matrix(NA, nIts, dim(F)[2])  # Empty matrix for theta matrix
u                       <- matrix(NA, nIts, J*T)        # Empty matrix for component of nonlinear trend at national level
v                       <- matrix(NA, nIts, K*T)        # Empty matrix for component of nonlinear trend at regional level
sv                      <- matrix(NA, nIts, L*T)        # Empty matrix for component of nonlinear trend at superregional level
w                       <- matrix(NA, nIts, T)          # Empty matrix for component of nonlinear trend at global level
alpha                   <- matrix(NA, nIts, I)          # Empty matrix for latent variable in prevalence model
zeta                    <- matrix(NA, nIts, I)          # Empty matrix for probit of proportion of q over p in prevalence model

##### The following lines set initial values for model parameters ##############
phi_c.prop.sd           <- 0.5                          # Initial value for proposal SD for log variance for normal prior for country random intercepts (log kappa_a^c)
phi_r.prop.sd           <- 0.5                          # Initial value for proposal SD for log variance for normal prior for region random intercepts (log kappa_a^s)
phi_s.prop.sd           <- 0.5                          # Initial value for proposal SD for log variance for normal prior for superregion random intercepts (log kappa_a^r)
eta_c.prop.sd           <- 0.05                         # Initial value for proposal SD for log variance for normal prior for country random slopes (log kappa_b^c)
eta_r.prop.sd           <- 0.05                         # Initial value for proposal SD for log variance for normal prior for region random slopes (log kappa_b^s)
eta_s.prop.sd           <- 0.05                         # Initial value for proposal SD for log variance for normal prior for superregion random slopes (log kappa_b^r)
phi_natl.prop.sd        <- 0.2                         # Initial value for proposal SD for log variance of random effects for national studies (log nu_"national")
phi_subn.prop.sd        <- 0.2                         # Initial value for proposal SD for log variance of random effects for subnational studies (log nu_s)
phi_comm.prop.sd        <- 0.2                         # Initial value for proposal SD for log variance of random effects for community studies (log nu_c)
tau.prop.sd             <- 0.2                         # Initial value for proposal SD for log variance for within-study errors that differ between age groups (log tau)
theta_c.prop.sd         <- 3                           # Initial value for proposal SD for log precision parameter for random walk at country level (log lambda_c)
theta_r.prop.sd         <- 3                           # Initial value for proposal SD for log precision parameter for random walk at region level (log lambda_s)
theta_s.prop.sd         <- 3                           # Initial value for proposal SD for log precision parameter for random walk at superregion level (log lambda_r)
theta_g.prop.sd         <- 3                           # Initial value for proposal SD for log precision parameter for random walk at global level (log lambda_g)
sigma1_c.prop.sd        <- rep(0.5, ng)                # Initial values for proposal SDs for log variances for country-specific random slope in spline coefficients (log omiga_c)
sigma2_c.prop.sd        <- rep(0.5, ng)                # Initial values for proposal SDs for log variances for country-specific random intercept in spline coefficients (log c)
sigma1_r.prop.sd        <- rep(0.5, ng)                # Initial values for proposal SDs for log variances for region-specific random slope in spline coefficients (log omiga_r)
sigma2_r.prop.sd        <- rep(0.5, ng)                # Initial values for proposal SDs for log variances for region-specific random intercept in spline coefficients (log r)
sigma1_s.prop.sd        <- rep(0.5, ng)                # Initial values for proposal SDs for log variances for superregion-specific random slope in spline coefficients (log omiga_s)
sigma2_s.prop.sd        <- rep(0.5, ng)                # Initial values for proposal SDs for log variances for superregion-specific random intercept in spline coefficients (log s)
alpha.prop.var          <- rep(0.1, I)                 # Initial values for proposal variance for the latent variable (alpha)
zeta.prop.var           <- rep(0.1, I)                 # Initial values for proposal variance for probit of proportion of q over p

phi_s[1]                <-(-3)                         # Initial value for log variance for normal prior for superregion random intercepts (log kappa_a^r)
phi_r[1]                <-(-3)                         # Initial value for log variance for normal prior for region random intercepts (log kappa_a^s)
phi_c[1]                <-(-3)                         # Initial value for log variance for normal prior for country random intercepts (log kappa_a^c)
eta_s[1]                <-(-9)                         # Initial value for log variance for normal prior for superregion random slopes (log kappa_b^r)
eta_r[1]                <-(-10)                         # Initial value for log variance for normal prior for region random slopes (log kappa_b^s)
eta_c[1]                <-(-10)                         # Initial value for log variance for normal prior for country random slopes (log kappa_b^c)
phi_natl[1]             <- -4                          # Initial value for log variance of random effects for national studies (log nu_"national")
phi_subn[1]             <- -3.5                        # Initial value for log variance of random effects for subnational studies (log nu_s)
phi_comm[1]             <- -3                          # Initial value for log variance of random effects for community studies (log nu_c)
tau[1]                  <- -5                          # Initial value for log variance for within-study errors that differ between age groups (log tau)
theta_c[1]              <- 10                          # Initial value for log precision parameter for random walk at country level (log lambda_c)
theta_r[1]              <- 12                          # Initial value for log precision parameter for random walk at region level (log lambda_s)
theta_s[1]              <- 12                          # Initial value for log precision parameter for random walk at superregion level (log lambda_r)
theta_g[1]              <- 13                          # Initial value for log precision parameter for random walk at global level (log lambda_g)

gamma[1,]               <- rep(0, 2*ng + 2*ng*L + 2*ng*Z + 2*ng*J)  # Initial values for age model parameters
sigma1_c[1,]            <- rep(1e-15, ng)              # Initial values for variances for country-specific random intercept in spline coefficients (log c)
sigma2_c[1,]            <- rep(1e-15, ng)              # Initial values for variances for country-specific random slope in spline coefficients (log omiga_c)
sigma1_r[1,]            <- rep(1e-15, ng)              # Initial values for variances for region-specific random intercept in spline coefficients (log r)
sigma2_r[1,]            <- rep(1e-15, ng)              # Initial values for variances for region-specific random slope in spline coefficients (log omiga_r)
sigma1_s[1,]            <- rep(1e-15, ng)              # Initial values for variances for superregion-specific random intercept in spline coefficients (log s)
sigma2_s[1,]            <- rep(1e-15, ng)              # Initial values for variances for superregion-specific random slope in spline coefficients (log omiga_s)
theta[1,]               <- rep(0, dim(F)[2])           # Initial values for theta matrix
u[1,]                   <- rep(0, J*T)                 # Initial values for component of nonlinear trend at national level
v[1,]                   <- rep(0, K*T)                 # Initial values for component of nonlinear trend at regional level
sv[1,]                  <- rep(0, L*T)                 # Initial values for component of nonlinear trend at superregional level
w[1,]                   <- rep(0, T)                   # Initial values for component of nonlinear trend at global level
alpha[1,]               <- rep(0, I)                   # Initial values for latent variable
zeta[1,]                <- rep(0, I)                   # Initial values for probit of proportion of q over p

###### Overdisperse the starting values ########################################
theta.max               <- 20                                                               # Constrain theta values for random walk to 20
phi_s[1]                <- rnorm(1, phi_s[1],       phi_s.prop.sd/2.4*sqrt(6))              # Log variance for normal prior for superregion random intercepts (log kappa_a^r)
phi_r[1]                <- rnorm(1, phi_r[1],       phi_r.prop.sd/2.4*sqrt(6))              # Log variance for normal prior for region random intercepts (log kappa_a^s)
phi_c[1]                <- rnorm(1, phi_c[1],       phi_c.prop.sd/2.4*sqrt(6))              # Log variance for normal prior for country random intercepts (log kappa_a^c)
eta_s[1]                <- rnorm(1, eta_s[1],       eta_s.prop.sd/2.4*sqrt(6))              # Log variance for normal prior for superregion random slopes (log kappa_b^r)
eta_r[1]                <- rnorm(1, eta_r[1],       eta_r.prop.sd/2.4*sqrt(6))              # Log variance for normal prior for region random slopes (log kappa_b^s)
eta_c[1]                <- rnorm(1, eta_c[1],       eta_c.prop.sd/2.4*sqrt(6))              # Log variance for normal prior for country random slopes (log kappa_b^c)
phi_natl[1]             <- rnorm(1, phi_natl[1],    phi_natl.prop.sd/2.4*sqrt(3))            # Log variance of random effects for national studies (log nu_"national")
foo <- rnorm(1, phi_subn[1], phi_subn.prop.sd/2.4*sqrt(3))                                   # Log variance of random effects for subnational studies (log nu_s)
while(foo < phi_natl[1]) foo <- rnorm(1, phi_subn[1], phi_subn.prop.sd/2.4*sqrt(3))          # Loop until the constraint is satisfied
phi_subn[1] <- foo                                                                           # Set value
foo <- rnorm(1, phi_comm[1], phi_comm.prop.sd/2.4*sqrt(3))                                   # Log variance of random effects for community studies (log nu_c)
while(foo < phi_subn[1]) foo <- rnorm(1, phi_comm[1], phi_comm.prop.sd/2.4*sqrt(3))          # Loop until the constraint is satisfied
phi_comm[1] <- foo                                                                           # Set value

foo <- rnorm(1, theta_c[1], theta_c.prop.sd/2.4)                                             # Log precision parameter for random walk at country level (log lambda_c)
while(foo > theta.max)  foo <- rnorm(1, theta_c[1], theta_c.prop.sd/2.4)                     # Loop until the constraint is satisfied
theta_c[1] <- foo                                                                            # Set value
foo <- rnorm(1, theta_r[1], theta_r.prop.sd/2.4)                                             # Log precision parameter for random walk at region level (log lambda_s)
while(foo > theta.max | foo < theta_c[1]) foo <- rnorm(1, theta_r[1], theta_r.prop.sd/2.4)   # Loop until the constraint is satisfied
theta_r[1] <- foo                                                                            # Set value
foo <- rnorm(1, theta_s[1], theta_s.prop.sd/2.4)                                             # Log precision parameter for random walk at superregion level (log lambda_r)
while(foo > theta.max | foo < theta_r[1]) foo <- rnorm(1, theta_s[1], theta_s.prop.sd/2.4)   # Loop until the constraint is satisfied
theta_s[1] <- foo                                                                            # Set value
foo <- rnorm(1, theta_g[1], theta_g.prop.sd/2.4)                                             # Log precision parameter for random walk at global level (log lambda_g)
while(foo > theta.max | foo < theta_s[1]) foo <- rnorm(1, theta_g[1], theta_g.prop.sd/2.4)   # Loop until the constraint is satisfied
theta_g[1] <- foo                                                                            # Set value

tau[1]        <- rnorm(1, tau[1], tau.prop.sd/2.4)                                           # Log variance for within-study errors that differ between age groups (log tau)
alpha[1,]     <- rnorm(I, alpha[1,], alpha.prop.var/2.4*sqrt(2))
zeta[1,]      <- rnorm(I, zeta[1,],  zeta.prop.var/2.4*sqrt(2))                                                                                             # Variances for latent variable (alpha)
sigma1_c[1,]  <- exp(rnorm(ng, log(sigma1_c[1,]), sigma1_c.prop.sd/2.4*sqrt(6)))             # Variances for country-specific random intercept in spline coefficients (log c)
sigma2_c[1,]  <- exp(rnorm(ng, log(sigma2_c[1,]), sigma2_c.prop.sd/2.4*sqrt(6)))             # Variances for country-specific random slope in spline coefficients (log omiga_c)
sigma1_r[1,]  <- exp(rnorm(ng, log(sigma1_r[1,]), sigma1_r.prop.sd/2.4*sqrt(6)))             # Variances for region-specific random intercept in spline coefficients (log r)
sigma2_r[1,]  <- exp(rnorm(ng, log(sigma2_r[1,]), sigma2_r.prop.sd/2.4*sqrt(6)))             # Variances for region-specific random slope in spline coefficients (log omiga_r)
sigma1_s[1,]  <- exp(rnorm(ng, log(sigma1_s[1,]), sigma1_s.prop.sd/2.4*sqrt(6)))             # Variances for superregion-specific random intercept in spline coefficients (log s)
sigma2_s[1,]  <- exp(rnorm(ng, log(sigma2_s[1,]), sigma2_s.prop.sd/2.4*sqrt(6)))             # Variances for superregion-specific random slope in spline coefficients (log omiga_s)

##### Diagonal of likelihood variance matrix ###################################
Sigma.diag              <- cwvar_probit + exp(tau[1])                                   # Diagonal of variance matrix, including within-study errors that differ between age groups
SigmaInv.diag           <- 1/Sigma.diag                                             # Inverse of diagonal of variance matrix

##### Variance of study-specific random effects ################################
V.ssre                                      <- rep(NA, N)                           # Empty vector for study-specific random effect variances
V.ssre[uid.match$coverage == "National"]    <- exp(phi_natl[1])                     # Set variances for random effects for national studies
V.ssre[uid.match$coverage == "Subnational"] <- exp(phi_subn[1])                     # Set variances for random effects for subnational studies
V.ssre[uid.match$coverage == "Community"]   <- exp(phi_comm[1])                     # Set variances for random effects for community studies

##### Diagonal of variance matrices for theta and gamma ########################
V.diag <- c(                                                                        # Diagonal of variance for theta matrix
            1/epsilon,                                                              # Flat variance for global intercept
            1/epsilon,                                                              # Flat variance for global slope
            rep(exp(phi_s[1]), L),                                                  # Variances for normal prior for superregion random intercepts
            rep(exp(eta_s[1]), L),                                                  # Variances for normal prior for superregion random slopes
            rep(exp(phi_r[1]), Z),                                                  # Variances for normal prior for region random intercepts
            rep(exp(eta_r[1]), Z),                                                  # Variances for normal prior for region random slopes
            rep(exp(phi_c[1]), J),                                                  # Variances for normal prior for country random intercepts
            rep(exp(eta_c[1]), J),                                                  # Variances for normal prior for country random slopes
            V.ssre                                                                  # Variances for study-specific random effects
            )
V.diag    <- c(V.diag, rep(1/epsilon, p))                                           # Add flat variances for covariates
VInv.diag <- 1/V.diag                                                               # Inverse variance
WInv.diag <- c(rep(epsilon, ng), rep(epsilon, ng),                                  # Inverse variance for age model
               rep(1/sigma1_c[1,], each=J), rep(1/sigma2_c[1,], each=J),
               rep(1/sigma1_r[1,], each=Z), rep(1/sigma2_r[1,], each=Z),
               rep(1/sigma1_s[1,], each=L), rep(1/sigma2_s[1,], each=L))

###### Initial values for the theta update #####################################
Q.init                  <- F.spam.prime %*% (SigmaInv.diag * F.spam)
diag(Q.init)            <- diag(Q.init) + VInv.diag
U.theta.init            <- chol(Q.init, cholsymmetrycheck=FALSE)

matrix_region           <- matrix(gamma[1, 2*ng+2*ng*J      + 1:(ng*Z)], Z, ng)[match(which.region, index_multiRinSR),]
matrix_region_time      <- matrix(gamma[1, 2*ng+2*ng*J+ng*Z + 1:(ng*Z)], Z, ng)[match(which.region, index_multiRinSR),]
matrix_region[is.na(matrix_region)]           <- 0 # Set regional effects to zero for regions in single-region superregions
matrix_region_time[is.na(matrix_region_time)] <- 0
varphiPlusC.age <- rowSums((
                            rep(1, I) %*% t(gamma[1,      1:ng])
                            + time    %*% t(gamma[1, ng + 1:ng])
                            +        matrix(gamma[1, 2*ng                    + 1:(ng*J)], J, ng)[which.country,]
                            + time * matrix(gamma[1, 2*ng+ng*J               + 1:(ng*J)], J, ng)[which.country,]
                            +        matrix_region
                            + time * matrix_region_time
                            +        matrix(gamma[1, 2*ng+2*ng*J+2*ng*Z      + 1:(ng*L)], L, ng)[which.sregion,]
                            + time * matrix(gamma[1, 2*ng+2*ng*J+2*ng*Z+ng*L + 1:(ng*L)], L, ng)[which.sregion,]
                            ) * ageMat )

###### Initial values for the update of nonlinear terms ########################
V1uInv.diag             <- Vinv.M.u <- rep(0, J*T)                                  # Country level
V1vInv.diag             <- Vinv.M.v <- rep(0, K*T)                                  # Region level
V1svInv.diag            <- Vinv.M.sv<- rep(0, L*T)                                  # Superregion level
V1wInv.diag             <- Vinv.M.w <- rep(0, T)                                    # Global level

###### Initial values for the gamma update #####################################
R.age <- cbind(                                                                     # Starting matrix mapping ages to gamma parameters
               ageMat,                                                              # Age matrix
               ageMat*time,                                                         # Age matrix multiplied by time
               ageMat_multiplies(StudyToCountry),                                   # Age matrix corresponding to sigma1_c
               ageMat_multiplies(StudyToCountry.time),                              # Age matrix corresponding to sigma2_c
               ageMat_multiplies(StudyToRegion[,multipleRegionsInSregion]),         # Age matrix corresponding to sigma1_r
               ageMat_multiplies(StudyToRegion.time[,multipleRegionsInSregion]),    # Age matrix corresponding to sigma2_r
               ageMat_multiplies(StudyToSregion),                                   # Age matrix corresponding to sigma1_s
               ageMat_multiplies(StudyToSregion.time)                               # Age matrix corresponding to sigma2_s
               )
R.age.spam              <- as.spam(R.age)                                           # Spam version of mapping matrix
R.age.spam.prime        <- t(R.age.spam)                                            # Transposed spam version of mapping matrix
Q.init                  <- R.age.spam.prime %*% (SigmaInv.diag * R.age.spam)        # Full conditional precision of gamma
diag(Q.init)            <- diag(Q.init) + WInv.diag                                 # Full conditional precision of gamma
U.gamma.init            <- chol(Q.init)                                             # Cholesky decomposition

###### Start chains at values consistent with the hyperparameter starting values
##### Theta ####################################################################
theta[1,]               <- update.theta(SigmaInv.diag, VInv.diag, u[1,], v[1,], sv[1,], w[1,])         # First update of theta

##### Non-linear component at national level ###################################
#### Users should refer to pp5-7 and pp14-16 of Danaei et al for the line-by-  #
#### line details of this section of code                                      #
country.count           <- as.numeric(colSums(table(data_year,country)>0))                             # Calculates number of years with data present for each country
tPtsPerCountry          <- vector("integer", J)                                                        # Calculates number of years with data present for each country
tPtsPerCountry[as.numeric(colnames(country.table))] <- country.count[index(colnames(country.table))]   # Calculates number of years with data present for each country
u.Prec.yMinusMean       <- SigmaInv.diag * (alpha[1,] - F %*% theta[1,] - (v[1,which.regionTime] + sv[1,which.sregionTime] + w[1,which.time]) - varphiPlusC.age)
agg1                    <- aggregate(u.Prec.yMinusMean, list(which.countryTime), sum)
agg2                    <- aggregate(SigmaInv.diag, list(which.countryTime), sum)
Vinv.M.u[agg1[,1]]      <- agg1[,2]
V1uInv.diag[agg2[,1]]   <- agg2[,2]
uStar                   <- lapply(1:J, u.prop.function, tPtsPerCountry, V1uInv.diag, Vinv.M.u, theta_c[1], theta_c[1], u[1,])
u.star                  <- rep(NA, J*T)
for(j in 1:J) {
    u.star[((j-1)*T+1):(j*T)] <- uStar[[j]]$u.star
}
u[1,]                   <- Re(u.star)

##### Non-linear component at regional level ###################################
##### See Danaei et al pp5-7 and 14-16 for details                             #
tPtsPerRegion           <- as.numeric(colSums(table(data_year,region)>0))               # Calculates number of years with data present for each region
v.Prec.yMinusMean       <- SigmaInv.diag * (alpha[1,] - F %*% theta[1,] - (u[1,which.countryTime] + sv[1,which.sregionTime] + w[1,which.time]) - varphiPlusC.age)
agg3                    <- aggregate(v.Prec.yMinusMean, list(which.regionTime), sum)
agg4                    <- aggregate(SigmaInv.diag, list(which.regionTime), sum)
Vinv.M.v[agg3[,1]]      <- agg3[,2]
V1vInv.diag[agg4[,1]]   <- agg4[,2]
u.star                  <- rep(NA, K*T)
uStar                   <- lapply(1:K, u.prop.function, tPtsPerRegion, V1vInv.diag, Vinv.M.v, theta_r[1], theta_r[1], v[1,])
for(j in 1:K) {
    u.star[((j-1)*T+1):(j*T)] <- uStar[[j]]$u.star
}
u.star[rep(!multipleRegionsInSregion, each=T)] <- 0
v[1,]                   <- Re(u.star)

##### Non-linear component at superregional level ##############################
##### See Danaei et al pp5-7 and 14-16 for details                             #
tPtsPerSregion          <- as.numeric(colSums(table(data_year,sregion)>0))              # Calculates number of years with data present for each superregion
sv.Prec.yMinusMean      <- SigmaInv.diag * (alpha[1,] - F %*% theta[1,] - (u[1,which.countryTime] + v[1,which.regionTime] + w[1,which.time]) - varphiPlusC.age)
agg5                    <- aggregate(sv.Prec.yMinusMean, list(which.sregionTime), sum)
agg6                    <- aggregate(SigmaInv.diag, list(which.sregionTime), sum)
Vinv.M.sv[agg5[,1]]     <- agg5[,2]
V1svInv.diag[agg6[,1]]  <- agg6[,2]
u.star                  <- rep(NA, L*T)
uStar                   <- lapply(1:L, u.prop.function, tPtsPerSregion, V1svInv.diag, Vinv.M.sv, theta_s[1], theta_s[1], sv[1,])
for(j in 1:L) {
    u.star[((j-1)*T+1):(j*T)] <- uStar[[j]]$u.star
}
sv[1,]                  <- Re(u.star)

##### Non-linear component at global level #####################################
##### See Danaei et al pp5-7 and 14-16 for details                             #
w.Prec.yMinusMean       <- SigmaInv.diag * (alpha[1,] - F %*% theta[1,] - (u[1,which.countryTime] + sv[1,which.sregionTime] + v[1,which.regionTime]) - varphiPlusC.age)
agg7                    <- aggregate(w.Prec.yMinusMean, list(which.time), sum)
agg8                    <- aggregate(SigmaInv.diag, list(which.time), sum)
Vinv.M.w[agg7[,1]]      <- agg7[,2]
V1wInv.diag[agg8[,1]]   <- agg8[,2]
uStar                   <- lapply(1, u.prop.function, 2, V1wInv.diag, Vinv.M.w, theta_g[1], theta_g[1], w[1,])
u.star                  <- uStar[[1]]$u.star
w[1,]                   <- as.vector(Re(u.star))

##### Age model ################################################################
F.theta.Mm              <- as.vector(F.spam %*% theta[1,] + u[1,which.countryTime] + v[1,which.regionTime] + sv[1,which.sregionTime] + w[1,which.time])
Q                       <- R.age.spam.prime %*% (SigmaInv.diag * R.age.spam)                                                    # Full conditional precision of gamma
diag(Q)                 <- diag(Q) + WInv.diag                                                                                  # Full conditional precision of gamma
U                       <- update.spam.chol.NgPeyton(U.gamma.init, Q)                                                           # Update Cholesky decomposition
VinvM                   <- as.matrix(R.age.spam.prime) %*% (SigmaInv.diag * (alpha[1,] - F.theta.Mm))                           # Full conditional precision x full conditional mean
gamma[1,]               <- backsolve(U, forwardsolve(U, VinvM, trans=TRUE) + rnorm(length(VinvM)))                              # Returns updated gamma following Gibbs step

matrix_region           <- matrix(gamma[1, 2*ng+2*ng*J      + 1:(ng*Z)], Z, ng)[match(which.region, index_multiRinSR),]
matrix_region_time      <- matrix(gamma[1, 2*ng+2*ng*J+ng*Z + 1:(ng*Z)], Z, ng)[match(which.region, index_multiRinSR),]
matrix_region[is.na(matrix_region)]           <- 0 # Set regional effects to zero for regions in single-region superregions
matrix_region_time[is.na(matrix_region_time)] <- 0
varphiPlusC.age <- rowSums((
                            rep(1, I) %*% t(gamma[1,      1:ng])
                            + time    %*% t(gamma[1, ng + 1:ng])
                            +        matrix(gamma[1, 2*ng                    + 1:(ng*J)], J, ng)[which.country,]
                            + time * matrix(gamma[1, 2*ng+ng*J               + 1:(ng*J)], J, ng)[which.country,]
                            +        matrix_region
                            + time * matrix_region_time
                            +        matrix(gamma[1, 2*ng+2*ng*J+2*ng*Z      + 1:(ng*L)], L, ng)[which.sregion,]
                            + time * matrix(gamma[1, 2*ng+2*ng*J+2*ng*Z+ng*L + 1:(ng*L)], L, ng)[which.sregion,]
                            ) * ageMat )

###### Set current value holders ###############################################
phi_s.current           <- phi_s[1]                                             # Current value holder for log variance for normal prior for superregion random intercepts (log kappa_a^r)
phi_r.current           <- phi_r[1]                                             # Current value holder for log variance for normal prior for region random intercepts (log kappa_a^s)
phi_c.current           <- phi_c[1]                                             # Current value holder for log variance for normal prior for country random intercepts (log kappa_a^c)
eta_s.current           <- eta_s[1]                                             # Current value holder for log variance for normal prior for superregion random slopes (log kappa_b^r)
eta_r.current           <- eta_r[1]                                             # Current value holder for log variance for normal prior for region random slopes (log kappa_b^s)
eta_c.current           <- eta_c[1]                                             # Current value holder for log variance for normal prior for country random slopes (log kappa_b^c)
phi_natl.current        <- phi_natl[1]                                          # Current value holder for log variance of random effects for national studies (log nu_"national")
phi_subn.current        <- phi_subn[1]                                          # Current value holder for log variance of random effects for subnational studies (log nu_s)
phi_comm.current        <- phi_comm[1]                                          # Current value holder for log variance of random effects for community studies (log nu_c)
tau.current             <- tau[1]                                               # Current value holder for log variance for within-study errors that differ between age groups (log tau)
theta_c.current         <- theta_c[1]                                           # Current value holder for log precision parameter for random walk at country level (log lambda_c)
theta_r.current         <- theta_r[1]                                           # Current value holder for log precision parameter for random walk at region level (log lambda_s)
theta_s.current         <- theta_s[1]                                           # Current value holder for log precision parameter for random walk at superregion level (log lambda_r)
theta_g.current         <- theta_g[1]                                           # Current value holder for log precision parameter for random walk at global level (log lambda_g)
gamma.current           <- gamma[1,]                                            # Current value holder for age model parameters
sigma1_c.current        <- sigma1_c[1,]                                         # Current value holder for variances for country-specific random intercept in spline coefficients (log c)
sigma2_c.current        <- sigma2_c[1,]                                         # Current value holder for variances for country-specific random slope in spline coefficients (log omiga_c)
sigma1_r.current        <- sigma1_r[1,]                                         # Current value holder for variances for region-specific random intercept in spline coefficients (log r)
sigma2_r.current        <- sigma2_r[1,]                                         # Current value holder for variances for region-specific random slope in spline coefficients (log omiga_r)
sigma1_s.current        <- sigma1_s[1,]                                         # Current value holder for variances for superregion-specific random intercept in spline coefficients (log s)
sigma2_s.current        <- sigma2_s[1,]                                         # Current value holder for variances for superregion-specific random slope in spline coefficients (log omiga_s)
theta.current           <- theta[1,]                                            # Current value holder for theta matrix
u.current               <- u[1,]                                                # Current value holder for component of nonlinear trend at national level
v.current               <- v[1,]                                                # Current value holder for component of nonlinear trend at regional level
sv.current              <- sv[1,]                                               # Current value holder for component of nonlinear trend at superregional level
w.current               <- w[1,]                                                # Current value holder for component of nonlinear trend at global level
alpha.current           <- alpha[1,]                                            # Current value holder for latent variable
zeta.current            <- zeta[1,]                                            # Current value holder for probit of proportion of q over p
deviance[1]             <- (sum(LogLik(alpha[1,], zeta[1,])) +                 # Calculate the initial deviance
                            sum(dnorm(alpha[1,], mean = F.theta.Mm + R.age.spam %*% gamma[1,], sd = sqrt(Sigma.diag), log=TRUE))
                            ) * (-2)




################################################################################
##### ***Start of MCMC loop*** #################################################
################################################################################
# Study counts for use in MCMC loop
N_comm             <- sum(uid.match$coverage=="Community")                          # Number of studies with community coverage
N_subn             <- sum(uid.match$coverage=="Subnational")                        # Number of studies with subnational coverage
N_natl             <- sum(uid.match$coverage=="National")                           # Number of studies with national coverage

# Initialise acceptance counts for Metropolis-Hastings steps
phi.acc            <- 0             # Set the acceptance count for the log variances for the study-specific random effects to zero
tau.acc            <- 0             # Set the acceptance count for the log variance for the within-study errors that differ between age groups to zero
theta_c.acc        <- 0             # Set the acceptance count for the log precision parameter for random walk at country level to zero
theta_r.acc        <- 0             # Set the acceptance count for the log precision parameter for random walk at region level zero
theta_s.acc        <- 0             # Set the acceptance count for the log precision parameter for random walk at superregion level to zero
theta_g.acc        <- 0             # Set the acceptance count for the log precision parameter for random walk at global level to zero
V.acc              <- 0             # Set the acceptance count for the linear random intercepts and slopes to zero
sigmas.acc         <- rep(0, ng)    # Set the acceptance count for the variances of the spline coefficients to zero
alpha.acc          <- rep(0, I)     # Set the acceptance count for the variances of the latent variable to zero

for (i in 2:nLong) {                                                                 # Loop from 2nd to the last MCMC iteration
    #if(i%%100==0) print(i)                                                         # Print the number of iterations to screen (every 100 iterations)
    if (i%%500 == 0) tracePlots()                                         # Save/update traceplots to PDF (every 2000 iterations)
    if (i%%10000 == 0 | i == nLong) {
        burnt <- (nIts-5000+1):(i/10)
        printDeviance()                                                              # Print deviance to screen
    }
    if(i %in% c(25000,45000,60000)){save.image(paste(outdirname,filename,"_Burnt_int.RData",sep=''))} # save intermediate rdata files
    # Tune the proposal variances every freq.val=200 iterations in the burn-in iterations (eg 5000 if nLong == 55000)
    if (i%%freq.val == 0 & i <= nLong - 50000) {
        phi_c.prop.sd       <- phi_c.prop.sd    * adaptJump(6, V.acc/freq.val)        # Tune the proposal SD for log variance for normal prior for country random intercepts (log kappa_a^c)
        eta_c.prop.sd       <- eta_c.prop.sd    * adaptJump(6, V.acc/freq.val)        # Tune the proposal SD for log variance for normal prior for country random slopes (log kappa_b^c)
        phi_r.prop.sd       <- phi_r.prop.sd    * adaptJump(6, V.acc/freq.val)        # Tune the proposal SD for log variance for normal prior for region random intercepts (log kappa_a^s)
        eta_r.prop.sd       <- eta_r.prop.sd    * adaptJump(6, V.acc/freq.val)        # Tune the proposal SD for log variance for normal prior for region random slopes (log kappa_b^s)
        phi_s.prop.sd       <- phi_s.prop.sd    * adaptJump(6, V.acc/freq.val)        # Tune the proposal SD for log variance for normal prior for superregion random intercepts (log kappa_a^r)
        eta_s.prop.sd       <- eta_s.prop.sd    * adaptJump(6, V.acc/freq.val)        # Tune the proposal SD for log variance for normal prior for superregion random slopes (log kappa_b^r)
        theta_c.prop.sd     <- theta_c.prop.sd  * adaptJump(1, theta_c.acc/freq.val)  # Tune the proposal SD for log precision parameter for random walk at country level (log lambda_c)
        theta_r.prop.sd     <- theta_r.prop.sd  * adaptJump(1, theta_r.acc/freq.val)  # Tune the proposal SD for log precision parameter for random walk at region level (log lambda_s)
        theta_s.prop.sd     <- theta_s.prop.sd  * adaptJump(1, theta_s.acc/freq.val)  # Tune the proposal SD for log precision parameter for random walk at superregion level (log lambda_r)
        theta_g.prop.sd     <- theta_g.prop.sd  * adaptJump(1, theta_g.acc/freq.val)  # Tune the proposal SD for log precision parameter for random walk at global level (log lambda_g)
        tau.prop.sd         <- tau.prop.sd      * adaptJump(1, tau.acc/freq.val)      # Tune the proposal SD for log variance for within-study errors that differ between age groups (log tau)
        phi_natl.prop.sd    <- phi_natl.prop.sd * adaptJump(3, phi.acc/freq.val)      # Tune the proposal SD for log variance of random effects for national studies (log nu_"national")
        phi_subn.prop.sd    <- phi_subn.prop.sd * adaptJump(3, phi.acc/freq.val)      # Tune the proposal SD for log variance of random effects for subnational studies (log nu_s)
        phi_comm.prop.sd    <- phi_comm.prop.sd * adaptJump(3, phi.acc/freq.val)      # Tune the proposal SD for log variance of random effects for community studies (log nu_c)
        alpha.prop.var		<- alpha.prop.var 	* adaptJump(n=rep(2,I), pjump=alpha.acc/freq.val, type='ben', i=i, K=freq.val) 	# Tune the proposal variance for latent variable (alpha)
        zeta.prop.var		  <- zeta.prop.var 	* adaptJump(n=rep(2,I), pjump=alpha.acc/freq.val, type='ben', i=i, K=freq.val) 	# Tune the proposal variance for probit of proportion of q over p
        for (j in 1:ng) {
            sigma1_c.prop.sd[j] <- sigma1_c.prop.sd[j] * adaptJump(6, sigmas.acc[j]/freq.val)  # Tune proposal variances for log variances for country-specific random intercept in spline coefficients (log c)
            sigma2_c.prop.sd[j] <- sigma2_c.prop.sd[j] * adaptJump(6, sigmas.acc[j]/freq.val)  # Tune proposal variances for log variances for country-specific random slope in spline coefficients (log omiga_c)
            sigma1_r.prop.sd[j] <- sigma1_r.prop.sd[j] * adaptJump(6, sigmas.acc[j]/freq.val)  # Tune proposal variances for log variances for region-specific random intercept in spline coefficients (log r)
            sigma2_r.prop.sd[j] <- sigma2_r.prop.sd[j] * adaptJump(6, sigmas.acc[j]/freq.val)  # Tune proposal variances for log variances for region-specific random slope in spline coefficients (log omiga_r)
            sigma1_s.prop.sd[j] <- sigma1_s.prop.sd[j] * adaptJump(6, sigmas.acc[j]/freq.val)  # Tune proposal variances for log variances for superregion-specific random intercept in spline coefficients (log s)
            sigma2_s.prop.sd[j] <- sigma2_s.prop.sd[j] * adaptJump(6, sigmas.acc[j]/freq.val)  # Tune proposal variances for log variances for superregion-specific random slope in spline coefficients (log omiga_s)
        }

        phi.acc             <- 0                                                    # Set the acceptance count for the study-specific random effects to zero
        tau.acc             <- 0                                                    # Set the acceptance count for the within-study errors that differ between age groups to zero
        theta_c.acc         <- 0                                                    # Set the acceptance count for the log precision parameter for random walk at country level to zero
        theta_r.acc         <- 0                                                    # Set the acceptance count for the log precision parameter for random walk at region level to zero
        theta_s.acc         <- 0                                                    # Set the acceptance count for the log precision parameter for random walk at superregion level to zero
        theta_g.acc         <- 0                                                    # Set the acceptance count for the log precision parameter for random walk at global level to zero
        V.acc               <- 0                                                    # Set the acceptance count for the linear random intercepts and slopes to zero
        sigmas.acc          <- rep(0, ng)                                           # Set the acceptance counts for the random spline coefficients to zero
        alpha.acc           <- rep(0, I)                                            # Set the acceptance counts for the latent variable (alpha) to zero
    }                                                                               # Close tuning loop

    #### Update latent variable (alpha) ########################################
    alpha.star	<- rnorm(I, alpha.current, sqrt(alpha.prop.var))                            # Propose new value for variance of latent variable (alpha)
    zeta.star	<- rnorm(I, zeta.current, sqrt(zeta.prop.var))                            # Propose new value for variance of probit of proportion of q over p

    R <- exp( 	                                                                            # Acceptance ratio R for proposed state
      dnorm(alpha.star, mean = F.theta.Mm + R.age.spam %*% gamma.current,               # Log likelihood of proposed state for latent variable (alpha)
            sd = sqrt(Sigma.diag), log=TRUE)
      - dnorm(alpha.current, mean = F.theta.Mm + R.age.spam %*% gamma.current,            # Log likelihood of existing state for latent variable (alpha)
              sd = sqrt(Sigma.diag), log=TRUE)
      + LogLik(alpha.star, zeta.star)                                                 # Log likelihood of binomial process given proposed alpha and probit proportion of q over p
      - LogLik(alpha.current, zeta.current)                                           # Log likelihood of binomial process given existing alpha and probit proportion of q over p
      + dnorm(zeta.star, mean = 0, sd = 1, log = TRUE)
      - dnorm(zeta.current, mean = 0, sd = 1, log = TRUE))
    accept <- which(runif(I) < R)                                                           # Metropolis-Hastings update: accept proposed state with probability R
    if (length(accept) > 0) {
      alpha.current[accept] <- alpha.star[accept]                                         # If accepted, update alpha
      zeta.current[accept] <- zeta.star[accept]                                   # If accepted, update probit of proportion of q over p
      alpha.acc[accept]     <- alpha.acc[accept] + 1                                      # If accepted, increment the acceptance count for alpha
    }


    #### STUDY-SPECIFIC RANDOM EFFECTS: update variances #######################
    #### see pp8-9 and 13 of Danaei et al ######################################
    ############################################################################
    ssre     <- theta.current[2 + 2*L + 2*Z + 2*J + (1:N)]                                  # Extract the current values from the theta matrix
    phi_natl.star    <- rnorm(1, phi_natl.current, phi_natl.prop.sd)                        # Propose new value for log variance of national study-specific random effects
    phi_subn.star    <- rnorm(1, phi_subn.current, phi_subn.prop.sd)                        # Propose new value for log variance of subnational study-specific random effects
    phi_comm.star    <- rnorm(1, phi_comm.current, phi_comm.prop.sd)                        # Propose new value for log variance of community study-specific random effects
    if (phi_natl.star < phi_subn.star & phi_subn.star < phi_comm.star) {                    # Constraint: national level has smaller variance than subnational and in turn community
        R <- exp(                                                                           # Acceptance ratio R for proposed state
              ssreLik(N_natl, phi_natl.star, ssre[uid.match$coverage=="National"])          # Log likelihood of proposed state for national level
            + LogPriorPhi(phi_natl.star)                                                    # Log prior of proposed state for national level
            - ssreLik(N_natl, phi_natl.current, ssre[uid.match$coverage=="National"])       # Log likelihood of existing state for national level
            - LogPriorPhi(phi_natl.current)                                                 # Log prior of existing state for national level
            + ssreLik(N_subn, phi_subn.star, ssre[uid.match$coverage=="Subnational"])       # Log likelihood of proposed state for subnational level
            + LogPriorPhi(phi_subn.star)                                                    # Log prior of proposed state for subnational level
            - ssreLik(N_subn, phi_subn.current, ssre[uid.match$coverage=="Subnational"])    # Log likelihood of existing state for subnational level
            - LogPriorPhi(phi_subn.current)                                                 # Log prior of existing state for subnational level
            + ssreLik(N_comm, phi_comm.star, ssre[uid.match$coverage=="Community"])         # Log likelihood of proposed state for community level
            + LogPriorPhi(phi_comm.star)                                                    # Log prior of proposed state for community level
            - ssreLik(N_comm, phi_comm.current, ssre[uid.match$coverage=="Community"])      # Log likelihood of existing state for community level
            - LogPriorPhi(phi_comm.current)                                                 # Log prior of existing state for community level
            )
        if (runif(1) < R) {                                                                 # Metropolis-Hastings update: accept proposed state with probability R
            phi_natl.current    <- phi_natl.star                                            # If accepted, set log variance of random effects for national studies to * value (log nu_natl)
            phi_subn.current    <- phi_subn.star                                            # If accepted, set log variance of random effects for subnational studies to * value (log nu_s)
            phi_comm.current    <- phi_comm.star                                            # If accepted, set log variance of random effects for community studies to * value (log nu_c)
            phi.acc             <- phi.acc + 1                                              # If accepted, increment the acceptance count for the study-specific random effects
            V.ssre[uid.match$coverage == "National"]        <- exp(phi_natl.current)        # If accepted, set variance of random effects for national studies to * value
            V.ssre[uid.match$coverage == "Subnational"]     <- exp(phi_subn.current)        # If accepted, set variance of random effects for subnational studies to * value
            V.ssre[uid.match$coverage == "Community"]       <- exp(phi_comm.current)        # If accepted, set variance of random effects for community studies to * value
            V.diag[2 + 2*L + 2*Z + 2*J + (1:N)]             <- V.ssre                       # If accepted, update overall variance diagonal vector
            VInv.diag           <- 1/V.diag                                                 # If accepted, update overall inverse variance diagonal vector
        }                                                                                   # Close Metropolis-Hastings update
    }                                                                                       # Close update of study-specific random effect variances

    #### UPDATE LINEAR RANDOM INTERCEPTS AND SLOPES VARIANCES AND THETA MATRIX #
    #### See pp5 and 13-14 of Danaei et al #####################################
    ############################################################################
    phi_c.sd.star <- rnorm(1, sqrt(exp(phi_c.current)), phi_c.prop.sd)                      # Propose SD for normal prior for country random intercepts
    phi_r.sd.star <- rnorm(1, sqrt(exp(phi_r.current)), phi_r.prop.sd)                      # Propose SD for normal prior for region random intercepts
    phi_s.sd.star <- rnorm(1, sqrt(exp(phi_s.current)), phi_s.prop.sd)                      # Propose SD for normal prior for superregion random intercepts
    eta_c.sd.star <- rnorm(1, sqrt(exp(eta_c.current)), eta_c.prop.sd)                      # Propose SD for normal prior for country random slopes
    eta_r.sd.star <- rnorm(1, sqrt(exp(eta_r.current)), eta_r.prop.sd)                      # Propose SD for normal prior for region random slopes
    eta_s.sd.star <- rnorm(1, sqrt(exp(eta_s.current)), eta_s.prop.sd)                      # Propose SD for normal prior for superregion random slopes

    ##### All proposed SD values must be positive ##############################
    if ( phi_s.sd.star > 0 & eta_s.sd.star > 0 & phi_r.sd.star > 0 & eta_r.sd.star > 0 & phi_c.sd.star > 0 & eta_c.sd.star > 0 ) {
        V.diag.star <- c(                                                                   # Proposed diagonal of variance matrix
            1/epsilon,                                                                      # Flat prior for global intercept
            1/epsilon,                                                                      # Flat prior for global slope
            rep(phi_s.sd.star^2, L),                                                        # Variance for normal prior for superregion random intercepts
            rep(eta_s.sd.star^2, L),                                                        # Variance for normal prior for superregion random slopes
            rep(phi_r.sd.star^2, Z),                                                        # Variance for normal prior for region random intercepts
            rep(eta_r.sd.star^2, Z),                                                        # Variance for normal prior for region random slopes
            rep(phi_c.sd.star^2, J),                                                        # Variance for normal prior for country random intercepts
            rep(eta_c.sd.star^2, J),                                                        # Variance for normal prior for country random slopes
            V.ssre,                                                                         # Variance of study-specific random effects
            rep(1/epsilon,p)                                                                # Flat priors for covariates
            )
        VInv.diag.star <- 1/V.diag.star                                                     # Inverse variance

        ##### Gibbs step to propose new theta based on the new variances #######
        Q <- Q.star         <- F.spam.prime %*% (SigmaInv.diag * F.spam)                    # Full conditional precision of theta and theta*
        VinvM               <- F.spam.prime %*% (SigmaInv.diag * (alpha.current - (u.current[which.countryTime] + v.current[which.regionTime]
                               + sv.current[which.sregionTime] + w.current[which.time]) - varphiPlusC.age)) # Full conditional precision x full conditional mean
        diag(Q.star)        <- diag(Q.star) + VInv.diag.star                                # Full conditional precision of theta*
        U.star              <- update.spam.chol.NgPeyton(U.theta.init, Q.star)              # Update Cholesky decomposition
        M.star              <- backsolve(U.star, forwardsolve(U.star, VinvM))               # Calculate proposed mean of theta
        theta.star          <- M.star + backsolve(U.star, rnorm(length(VinvM)))             # Calculate proposed theta matrix

        ##### Calculate the equivalent information for the existing state ######
        diag(Q)             <- diag(Q) + VInv.diag                                          # Full conditional precision of theta
        U                   <- update.spam.chol.NgPeyton(U.theta.init, Q)                   # Update Cholesky decomposition
        M                   <- backsolve(U, forwardsolve(U, VinvM))                         # Calculate mean of current theta
        F.theta.star.Mm     <- F %*% theta.star + u.current[which.countryTime] + v.current[which.regionTime] + sv.current[which.sregionTime] + w.current[which.time]
        R <- exp(                                                                           # Metropolis-Hastings acceptance ratio
              sum(dnorm(alpha.current, mean = F.theta.star.Mm + R.age.spam %*% gamma.current, sd = sqrt(Sigma.diag), log = TRUE))
            + 0.5 * (sum(log(VInv.diag.star)) - (sum(VInv.diag.star * theta.star^2))) - sum(log(diag(U.star)))
            + 0.5 * t(theta.star - M.star) %*% Q.star %*% (theta.star - M.star)
            - sum(dnorm(alpha.current, mean = F.theta.Mm + R.age.spam %*% gamma.current, sd = sqrt(Sigma.diag), log = TRUE))
            - 0.5 * (sum(log(VInv.diag)) - (sum(VInv.diag * theta.current^2))) + sum(log(diag(U)))
            - 0.5 * t(theta.current - M) %*% Q %*% (theta.current - M)  )

        ##### Accept/reject ###
        if (runif(1) < R) {                                                                 # Metropolis-Hastings update: accept proposed state with probability R
            phi_s.current           <- log(phi_s.sd.star^2)                                 # If accepted, set log variance for normal prior for superregion random intercepts (log kappa_a^r)
            eta_s.current           <- log(eta_s.sd.star^2)                                 # If accepted, set log variance for normal prior for superregion random slopes (log kappa_b^r)
            phi_r.current           <- log(phi_r.sd.star^2)                                 # If accepted, set log variance for normal prior for region random intercepts (log kappa_a^s)
            eta_r.current           <- log(eta_r.sd.star^2)                                 # If accepted, set log variance for normal prior for region random slopes (log kappa_b^s)
            phi_c.current           <- log(phi_c.sd.star^2)                                 # If accepted, set log variance for normal prior for country random intercepts (log kappa_a^c)
            eta_c.current           <- log(eta_c.sd.star^2)                                 # If accepted, set log variance for normal prior for country random slopes (log kappa_b^c)
            theta.current           <- theta.star                                           # If accepted, update theta matrix
            VInv.diag               <- VInv.diag.star                                       # If accepted, update diagonal of inverse variance matrix
            V.diag                  <- V.diag.star                                          # If accepted, update diagonal of variance matrix
            V.acc                   <- V.acc + 1                                              # If accepted, update acceptance count for the linear random intercepts and slopes
            F.theta.Mm              <- as.vector(F.spam %*% theta.current + u.current[which.countryTime] + v.current[which.regionTime]
                                                 + sv.current[which.sregionTime] + w.current[which.time])    # If accepted, update
        }
    }

    #### NON-LINEAR TRENDS #####################################################
    #### Users should refer to pp5-7 and pp14-16 of Danaei et al for the line- #
    #### by-line details of this section of code ###############################
    ############################################################################
    # Component of nonlinear trend at national level #
    theta_c.star <- rnorm(1, theta_c.current, theta_c.prop.sd)                              # Propose theta_c*
    if (theta_c.star < theta_r.current & theta_c.star < theta.max) {                        # Only enter update step if constraints are satisfied
        u.Prec.yMinusMean           <- SigmaInv.diag * (alpha.current - F %*% theta.current - (v.current[which.regionTime] +
                                       sv.current[which.sregionTime] + w.current[which.time]) - varphiPlusC.age)
        agg1                        <- aggregate(u.Prec.yMinusMean, list(which.countryTime), sum)
        agg2                        <- aggregate(SigmaInv.diag, list(which.countryTime), sum)
        Vinv.M.u[agg1[,1]]          <- agg1[,2]
        V1uInv.diag[agg2[,1]]       <- agg2[,2]
        uStar                       <- lapply(1:J, u.prop.function, tPtsPerCountry, V1uInv.diag, Vinv.M.u, theta_c.star, theta_c.current, u.current)    # Propose u* values
        u.star                      <- rep(NA, J*T)
        u.dens.old <- u.dens.star   <- rep(NA, J)
        for (j in 1:J) {
            u.star[((j-1)*T+1):(j*T)]   <- uStar[[j]]$u.star
            u.dens.old[j]               <- uStar[[j]]$dens.old
            u.dens.star[j]              <- uStar[[j]]$dens.star
        }
        u.star                      <- Re(u.star)
        u.dens.old                  <- Re(u.dens.old)
        u.dens.star                 <- Re(u.dens.star)
        F.theta.Mm.star             <- as.vector(F.spam %*% theta.current + u.star[which.countryTime] + v.current[which.regionTime] + sv.current[which.sregionTime] + w.current[which.time])
        R <- exp(                                                                           # Acceptance ratio R for proposed state
              sum(dnorm(alpha.current, mean = F.theta.Mm.star + R.age.spam %*% gamma.current, sd = sqrt(Sigma.diag), log = TRUE))
            - sum(dnorm(alpha.current, mean = F.theta.Mm + R.age.spam %*% gamma.current, sd = sqrt(Sigma.diag), log = TRUE))
            + LogPostThetaC(theta_c.star, u.star)
            - LogPostThetaC(theta_c.current, u.current)
            + sum(u.dens.old)
            - sum(u.dens.star)
            )
        if (runif(1) < R) {                                                                 # Metropolis-Hastings step: accept proposed state with probability R
            theta_c.current <- theta_c.star
            u.current       <- u.star
            F.theta.Mm      <- F.theta.Mm.star
            theta_c.acc     <- theta_c.acc + 1
        }
    }

    # Component of nonlinear trend at regional level #
    theta_r.star <- rnorm(1, theta_r.current, theta_r.prop.sd)
    if (theta_r.star > theta_c.current & theta_r.star < theta_s.current & theta_r.star < theta.max) {
        v.Prec.yMinusMean <- SigmaInv.diag * (alpha.current - F %*% theta.current - (u.current[which.countryTime] + sv.current[which.sregionTime] + w.current[which.time]) - varphiPlusC.age)
        agg3                        <- aggregate(v.Prec.yMinusMean, list(which.regionTime), sum)
        agg4                        <- aggregate(SigmaInv.diag, list(which.regionTime), sum)
        Vinv.M.v[agg3[,1]]          <- agg3[,2]
        V1vInv.diag[agg4[,1]]       <- agg4[,2]
        u.star                      <- rep(NA, K*T)
        u.dens.old <- u.dens.star   <- rep(NA, K)
        uStar <- lapply(1:K, u.prop.function, tPtsPerRegion, V1vInv.diag, Vinv.M.v, theta_r.star, theta_r.current, v.current)
        for (j in 1:K) {
            u.star[((j-1)*T+1):(j*T)] <- uStar[[j]]$u.star
            u.dens.old[j]             <- uStar[[j]]$dens.old
            u.dens.star[j]            <- uStar[[j]]$dens.star
        }
        u.star                      <- Re(u.star)
        u.dens.old                  <- Re(u.dens.old)
        u.dens.star                 <- Re(u.dens.star)
        u.star[rep(!multipleRegionsInSregion, each=T)]  <- 0                                # Set to zero in those regions that are also super-regions
        u.dens.star[!multipleRegionsInSregion]          <- 0
        u.dens.old[!multipleRegionsInSregion]           <- 0
        F.theta.Mm.star             <- as.vector(F.spam %*% theta.current + u.current[which.countryTime] + u.star[which.regionTime] + sv.current[which.sregionTime] + w.current[which.time])
        R <- exp(                                                                           # Acceptance ratio R for proposed state
              sum(dnorm(alpha.current, mean = F.theta.Mm.star + R.age.spam %*% gamma.current, sd = sqrt(Sigma.diag), log = TRUE))
            - sum(dnorm(alpha.current, mean = F.theta.Mm + R.age.spam %*% gamma.current, sd = sqrt(Sigma.diag), log = TRUE))
            + LogPostThetaR(theta_r.star, u.star)
            - LogPostThetaR(theta_r.current, v.current)
            + sum(u.dens.old)
            - sum(u.dens.star)
            )
        if (runif(1) < R) {                                                                 # Metropolis-Hastings step: accept proposed state with probability R
            theta_r.current <- theta_r.star
            v.current       <- u.star
            F.theta.Mm      <- F.theta.Mm.star
            theta_r.acc     <- theta_r.acc + 1
        }
    }

    # Component of nonlinear trend at superregional level #
    theta_s.star <- rnorm(1, theta_s.current, theta_s.prop.sd)
    if (theta_s.star > theta_r.current & theta_s.star < theta_g.current & theta_s.star < theta.max) {
        sv.Prec.yMinusMean <- SigmaInv.diag * (alpha.current - F %*% theta.current - (u.current[which.countryTime] + v.current[which.regionTime] + w.current[which.time]) - varphiPlusC.age)
        agg5                        <- aggregate(sv.Prec.yMinusMean, list(which.sregionTime), sum)
        agg6                        <- aggregate(SigmaInv.diag, list(which.sregionTime), sum)
        Vinv.M.sv[agg5[,1]]         <- agg5[,2]
        V1svInv.diag[agg6[,1]]      <- agg6[,2]
        u.star                      <- rep(NA, L*T)
        u.dens.old <- u.dens.star   <- rep(NA, L)
        uStar <- lapply(1:L, u.prop.function, tPtsPerSregion, V1svInv.diag, Vinv.M.sv, theta_s.star, theta_s.current, sv.current)
        for (j in 1:L) {
            u.star[((j-1)*T+1):(j*T)]   <- uStar[[j]]$u.star
            u.dens.old[j]               <- uStar[[j]]$dens.old
            u.dens.star[j]              <- uStar[[j]]$dens.star
        }
        u.star                      <- Re(u.star)
        u.dens.old                  <- Re(u.dens.old)
        u.dens.star                 <- Re(u.dens.star)
        F.theta.Mm.star             <- as.vector(F.spam %*% theta.current + u.current[which.countryTime] + v.current[which.regionTime] + u.star[which.sregionTime] + w.current[which.time])
        R <- exp(                                                                           # Acceptance ratio R for proposed state
              sum(dnorm(alpha.current, mean = F.theta.Mm.star + R.age.spam %*% gamma.current, sd = sqrt(Sigma.diag), log = TRUE))
            - sum(dnorm(alpha.current, mean = F.theta.Mm + R.age.spam %*% gamma.current, sd = sqrt(Sigma.diag), log = TRUE))
            + LogPostThetaS(theta_s.star, u.star)
            - LogPostThetaS(theta_s.current, sv.current)
            + sum(u.dens.old)
            - sum(u.dens.star)
            )
        if (runif(1) < R) {                                                                 # Metropolis-Hastings step: accept proposed state with probability R
            theta_s.current <- theta_s.star
            sv.current      <- u.star
            F.theta.Mm      <- F.theta.Mm.star
            theta_s.acc     <- theta_s.acc + 1
        }
    }

    # Component of nonlinear trend at global level #
    theta_g.star <- rnorm(1, theta_g.current, theta_g.prop.sd)
    if (theta_g.star > theta_s.current & theta_g.star < theta.max) {
        w.Prec.yMinusMean <- SigmaInv.diag  * (alpha.current - F %*% theta.current - (u.current[which.countryTime] + sv.current[which.sregionTime] + v.current[which.regionTime]) - varphiPlusC.age)
        for (j in unique(which.time)) {
            Vinv.M.w[j]             <- sum(w.Prec.yMinusMean[which.time==j])
            V1wInv.diag[j]          <- sum(SigmaInv.diag[which.time==j])
        }
        uStar                       <- lapply(1, u.prop.function, 2, V1wInv.diag, Vinv.M.w, theta_g.star, theta_g.current, w.current)
        u.star                      <- uStar[[1]]$u.star
        u.dens.old                  <- uStar[[1]]$dens.old
        u.dens.star                 <- uStar[[1]]$dens.star
        u.star                      <- as.vector(Re(u.star))
        u.dens.old                  <- Re(u.dens.old)
        u.dens.star                 <- Re(u.dens.star)
        F.theta.Mm.star             <- as.vector(F.spam %*% theta.current + u.current[which.countryTime] + v.current[which.regionTime] + sv.current[which.sregionTime] + u.star[which.time])
        R <- exp(                                                                           # Acceptance ratio R for proposed state
              sum(dnorm(alpha.current, mean = F.theta.Mm.star + R.age.spam %*% gamma.current, sd = sqrt(Sigma.diag), log = TRUE))
            - sum(dnorm(alpha.current, mean = F.theta.Mm + R.age.spam %*% gamma.current, sd = sqrt(Sigma.diag), log = TRUE))
            + LogPostThetaG(theta_g.star, u.star)
            - LogPostThetaG(theta_g.current, w.current)
            + u.dens.old
            - u.dens.star
            )
        if (runif(1) < R) {                                                                 # Metropolis-Hastings step: accept proposed state with probability R
            theta_g.current <- theta_g.star
            w.current       <- u.star
            F.theta.Mm      <- F.theta.Mm.star
            theta_g.acc <- theta_g.acc + 1
        }
    }

    ##### GAMMA: update using Gibbs ############################################
    ############################################################################
    Q                   <- R.age.spam.prime %*% (SigmaInv.diag * R.age.spam)                                                  # Full conditional precision of gamma
    diag(Q)             <- diag(Q) + WInv.diag                                                                                # Full conditional precision of gamma
    U                   <- update.spam.chol.NgPeyton(U.gamma.init, Q)                                                         # Update Cholesky decomposition
    VinvM               <- as.matrix(R.age.spam.prime) %*% (SigmaInv.diag * (alpha.current - F.theta.Mm))                     # Full conditional precision x conditional mean
    gamma.current       <- backsolve(U, forwardsolve(U, VinvM, trans=TRUE) + rnorm(length(VinvM)))                            # Updated gamma after Gibbs step
    matrix_region       <- matrix(gamma.current[2*ng+2*ng*J      + 1:(ng*Z)], Z, ng)[match(which.region, index_multiRinSR),]  # re-index columns for regions
    matrix_region_time  <- matrix(gamma.current[2*ng+2*ng*J+ng*Z + 1:(ng*Z)], Z, ng)[match(which.region, index_multiRinSR),]
    matrix_region[is.na(matrix_region)] <- 0                                             # for regions in single-region superregions: regional effects are zero
    matrix_region_time[is.na(matrix_region_time)] <- 0
    varphiPlusC.age 	<- rowSums(( rep(1, I) %*% t(gamma.current[1:ng])
                                   + time %*% t(gamma.current[ng + 1:ng])
                                   +        matrix(gamma.current[2*ng                    + 1:(ng*J)], J, ng)[which.country,]
                                   + time * matrix(gamma.current[2*ng+ng*J               + 1:(ng*J)], J, ng)[which.country,]
                                   +        matrix_region
                                   + time * matrix_region_time
                                   +        matrix(gamma.current[2*ng+2*ng*J+2*ng*Z      + 1:(ng*L)], L, ng)[which.sregion,]
                                   + time * matrix(gamma.current[2*ng+2*ng*J+2*ng*Z+ng*L + 1:(ng*L)], L, ng)[which.sregion,]
                                  ) * ageMat )

    ##### SIGMA: update using Metropolis-Hastings ##############################
    ############################################################################
    for (j in 1:ng) {                                                                                    # Loop over ng parameters
        log.sigma1_c.star <- rnorm(1, log(sigma1_c.current[j]), sigma1_c.prop.sd[j])                     # Propose sigma* values on log scale
        log.sigma2_c.star <- rnorm(1, log(sigma2_c.current[j]), sigma2_c.prop.sd[j])
        log.sigma1_r.star <- rnorm(1, log(sigma1_r.current[j]), sigma1_r.prop.sd[j])
        log.sigma2_r.star <- rnorm(1, log(sigma2_r.current[j]), sigma2_r.prop.sd[j])
        log.sigma1_s.star <- rnorm(1, log(sigma1_s.current[j]), sigma1_s.prop.sd[j])
        log.sigma2_s.star <- rnorm(1, log(sigma2_s.current[j]), sigma2_s.prop.sd[j])

        R <- exp(                                                                                        # Posterior probability R for proposed state
              ssreLik(J, log.sigma1_c.star,        gamma.current[2*ng                    + (((j-1)*J+1):(j*J))]) + LogPriorPhi(log.sigma1_c.star)
            - ssreLik(J, log(sigma1_c.current[j]), gamma.current[2*ng                    + (((j-1)*J+1):(j*J))]) - LogPriorPhi(log(sigma1_c.current[j]))
            + ssreLik(J, log.sigma2_c.star,        gamma.current[2*ng+ng*J               + (((j-1)*J+1):(j*J))]) + LogPriorPhi(log.sigma2_c.star)
            - ssreLik(J, log(sigma2_c.current[j]), gamma.current[2*ng+ng*J               + (((j-1)*J+1):(j*J))]) - LogPriorPhi(log(sigma2_c.current[j]))
            + ssreLik(Z, log.sigma1_r.star,        gamma.current[2*ng+2*ng*J             + (((j-1)*Z+1):(j*Z))]) + LogPriorPhi(log.sigma1_r.star)
            - ssreLik(Z, log(sigma1_r.current[j]), gamma.current[2*ng+2*ng*J             + (((j-1)*Z+1):(j*Z))]) - LogPriorPhi(log(sigma1_r.current[j]))
            + ssreLik(Z, log.sigma2_r.star,        gamma.current[2*ng+2*ng*J+ng*Z        + (((j-1)*Z+1):(j*Z))]) + LogPriorPhi(log.sigma2_r.star)
            - ssreLik(Z, log(sigma2_r.current[j]), gamma.current[2*ng+2*ng*J+ng*Z        + (((j-1)*Z+1):(j*Z))]) - LogPriorPhi(log(sigma2_r.current[j]))
            + ssreLik(L, log.sigma1_s.star,        gamma.current[2*ng+2*ng*J+2*ng*Z      + (((j-1)*L+1):(j*L))]) + LogPriorPhi(log.sigma1_s.star)
            - ssreLik(L, log(sigma1_s.current[j]), gamma.current[2*ng+2*ng*J+2*ng*Z      + (((j-1)*L+1):(j*L))]) - LogPriorPhi(log(sigma1_s.current[j]))
            + ssreLik(L, log.sigma2_s.star,        gamma.current[2*ng+2*ng*J+2*ng*Z+ng*L + (((j-1)*L+1):(j*L))]) + LogPriorPhi(log.sigma2_s.star)
            - ssreLik(L, log(sigma2_s.current[j]), gamma.current[2*ng+2*ng*J+2*ng*Z+ng*L + (((j-1)*L+1):(j*L))]) - LogPriorPhi(log(sigma2_s.current[j]))
        )

        if (runif(1) < R) {                                                                              # Metropolis-Hastings update: accept proposed state with probability R
            sigma1_c.current[j]  <- exp(log.sigma1_c.star)                                               # If accepted, set sigma to sigma*
            sigma2_c.current[j]  <- exp(log.sigma2_c.star)
            sigma1_r.current[j]  <- exp(log.sigma1_r.star)
            sigma2_r.current[j]  <- exp(log.sigma2_r.star)
            sigma1_s.current[j]  <- exp(log.sigma1_s.star)
            sigma2_s.current[j]  <- exp(log.sigma2_s.star)
            WInv.diag[2*ng                    + (((j-1)*J+1):(j*J))] <- rep(1/exp(log.sigma1_c.star), J) # If accepted, set variance for sigma to variance for sigma*
            WInv.diag[2*ng+ng*J               + (((j-1)*J+1):(j*J))] <- rep(1/exp(log.sigma2_c.star), J)
            WInv.diag[2*ng+2*ng*J             + (((j-1)*Z+1):(j*Z))] <- rep(1/exp(log.sigma1_r.star), Z)
            WInv.diag[2*ng+2*ng*J+ng*Z        + (((j-1)*Z+1):(j*Z))] <- rep(1/exp(log.sigma2_r.star), Z)
            WInv.diag[2*ng+2*ng*J+2*ng*Z      + (((j-1)*L+1):(j*L))] <- rep(1/exp(log.sigma1_s.star), L)
            WInv.diag[2*ng+2*ng*J+2*ng*Z+ng*L + (((j-1)*L+1):(j*L))] <- rep(1/exp(log.sigma2_s.star), L)
            sigmas.acc[j] <- sigmas.acc[j]+1                                                             # If accepted, increment the acceptance count for sigma
        }
    }                                                                                                    # Close loop over ng parameters

    ##### TAU: update using Metropolis-Hastings ################################
    ############################################################################
    tau.star                <- rnorm(1, tau.current, tau.prop.sd)                                       # Propose tau* value on log scale
    Sigma.diag.star         <- cwvar_probit + exp(tau.star)                                                   # Variance diagonal with tau* value
    SigmaInv.diag.star      <- 1/Sigma.diag.star                                                        # Inverse variance diagonal including tau* value
    R <- exp(                                                                                           # Posterior probability R for proposed state
        sum(dnorm(alpha.current, mean = F.theta.Mm + R.age.spam %*%                                     # Log likelihood of proposed state
                    gamma.current, sd = sqrt(Sigma.diag.star), log = TRUE)) -
        sum(dnorm(alpha.current, mean = F.theta.Mm + R.age.spam %*%                                     # Log likelihood of existing state
                          gamma.current, sd = sqrt(Sigma.diag), log=TRUE)) +
        LogPriorPhi(tau.star) -                                                                         # Log prior of proposed state
        LogPriorPhi(tau.current)                                                                        # Log prior of existing state
        )                                                                                               # End of calculation of posterior probability
    if (runif(1) < R) {                                                                                 # Metropolis-Hastings update: accept proposed state with probability R
        tau.current         <- tau.star                                                                 # If accepted, set tau to tau star
        tau.acc             <- tau.acc + 1                                                              # If accepted, increment the acceptance count for tau
        Sigma.diag          <- Sigma.diag.star                                                          # If accepted, set variance to variance*
        SigmaInv.diag       <- SigmaInv.diag.star                                                       # If accepted, set inverse-variance to inverse-variance*
    }                                                                                                   # Close Metropolis-Hastings update

    ##### Save state every 10th iteration ######################################
    ############################################################################
    if (i%%10 == 0) {                           # Thinning is carried out here, with parameters only saved every 10 iterations
        j               <- i/10                 # Save iteration in corresponding position/row of vectors/matrices
        u[j,]           <- u.current            # Save current state for country-level random walk parameters
        v[j,]           <- v.current            # Save current state for region-level random walk parameters
        sv[j,]          <- sv.current           # Save current state for superregion-level random walk parameters
        w[j,]           <- w.current            # Save current state for global-level random walk parameters
        theta[j,]       <- theta.current        # Save current state for theta matrix
        phi_natl[j]     <- phi_natl.current     # Save current state for log variance of random effects for national studies (log nu_"national")
        phi_subn[j]     <- phi_subn.current     # Save current state for log variance of random effects for subnational studies (log nu_s)
        phi_comm[j]     <- phi_comm.current     # Save current state for log variance of random effects for community studies (log nu_c)
        tau[j]          <- tau.current          # Save current state for log variance for within-study errors that differ between age groups (log tau)
        theta_c[j]      <- theta_c.current      # Save current state for log precision parameter for random walk at country level (log lambda_c)
        theta_r[j]      <- theta_r.current      # Save current state for log precision parameter for random walk at region level (log lambda_s)
        theta_s[j]      <- theta_s.current      # Save current state for log precision parameter for random walk at superregion level (log lambda_r)
        theta_g[j]      <- theta_g.current      # Save current state for log precision parameter for random walk at global level (log lambda_g)
        phi_c[j]        <- phi_c.current        # Save current state for log variance for normal prior for country random intercepts (log kappa_a^c)
        phi_r[j]        <- phi_r.current        # Save current state for log variance for normal prior for region random intercepts (log kappa_a^s)
        phi_s[j]        <- phi_s.current        # Save current state for log variance for normal prior for superregion random intercepts (log kappa_a^r)
        eta_c[j]        <- eta_c.current        # Save current state for log variance for normal prior for country random slopes (log kappa_b^c)
        eta_r[j]        <- eta_r.current        # Save current state for log variance for normal prior for region random slopes (log kappa_b^s)
        eta_s[j]        <- eta_s.current        # Save current state for log variance for normal prior for superregion random slopes (log kappa_b^r)
        gamma[j,]       <- gamma.current        # Save current state for age model parameters
        sigma1_c[j,]    <- sigma1_c.current     # Save current state for variances for country-specific random intercept in spline coefficients
        sigma2_c[j,]    <- sigma2_c.current     # Save current state for variances for country-specific random slope in spline coefficients
        sigma1_r[j,]    <- sigma1_r.current     # Save current state for variances for country-specific random intercept in spline coefficients
        sigma2_r[j,]    <- sigma2_r.current     # Save current state for variances for country-specific random slope in spline coefficients
        sigma1_s[j,]    <- sigma1_s.current     # Save current state for variances for country-specific random intercept in spline coefficients
        sigma2_s[j,]    <- sigma2_s.current     # Save current state for variances for country-specific random slope in spline coefficients
        alpha[j,]       <- alpha.current        # Save current state for latent variable (alpha)
        zeta[j,]        <- zeta.current     # Save current state for probit of proportion of q over p
        deviance[j]     <- (sum(LogLik(alpha[j,], zeta[j,])) + sum(dnorm(alpha[j,], mean = F.theta.Mm + R.age.spam %*% gamma[j,], sd = sqrt(Sigma.diag), log=TRUE))) * (-2) # Save deviance
    }                                           # End of thinning loop
}                                               # End of MCMC loop

################################################################################
##### ***End of MCMC loop*** ###################################################
################################################################################



##### Output results ###########################################################
##### Save the burnt-in, thinned chains                                        #
################################################################################
burnt       <- (nIts-5000+1):nIts               # Discard the first (nIts-5000)/10 post-thinning iterations
u           <- u[burnt,]                        # Save component of nonlinear trend at national level
v           <- v[burnt,]                        # Save component of nonlinear trend at regional level
sv          <- sv[burnt,]                       # Save component of nonlinear trend at superregional level
w           <- w[burnt,]                        # Save component of nonlinear trend at global level
theta       <- theta[burnt,]                    # Save theta matrix
phi_natl    <- phi_natl[burnt]                  # Save log variance of random effects for national studies (log nu_"national")
phi_subn    <- phi_subn[burnt]                  # Save log variance of random effects for subnational studies (log nu_s)
phi_comm    <- phi_comm[burnt]                  # Save log variance of random effects for community studies (log nu_c)
tau         <- tau[burnt]                       # Save log variance for within-study errors that differ between age groups (log tau)
theta_c     <- theta_c[burnt]                   # Save log precision parameter for random walk at country level (log lambda_c)
theta_r     <- theta_r[burnt]                   # Save log precision parameter for random walk at region level (log lambda_s)
theta_s     <- theta_s[burnt]                   # Save log precision parameter for random walk at superregion level (log lambda_r)
theta_g     <- theta_g[burnt]                   # Save log precision parameter for random walk at global level (log lambda_g)
phi_c       <- phi_c[burnt]                     # Save log variance for normal prior for country random intercepts (log kappa_a^c)
phi_r       <- phi_r[burnt]                     # Save log variance for normal prior for region random intercepts (log kappa_a^s)
phi_s       <- phi_s[burnt]                     # Save log variance for normal prior for superregion random intercepts (log kappa_a^r)
eta_c       <- eta_c[burnt]                     # Save log variance for normal prior for country random slopes (log kappa_b^c)
eta_r       <- eta_r[burnt]                     # Save log variance for normal prior for region random slopes (log kappa_b^s)
eta_s       <- eta_s[burnt]                     # Save log variance for normal prior for superregion random slopes (log kappa_b^r)
gamma       <- gamma[burnt,]                    # Save age model parameters
sigma1_c    <- sigma1_c[burnt,]                 # Save variances for country-specific random intercept in spline coefficients
sigma2_c    <- sigma2_c[burnt,]                 # Save variances for country-specific random slope in spline coefficients
sigma1_r    <- sigma1_r[burnt,]                 # Save variances for country-specific random intercept in spline coefficients
sigma2_r    <- sigma2_r[burnt,]                 # Save variances for country-specific random slope in spline coefficients
sigma1_s    <- sigma1_s[burnt,]                 # Save variances for country-specific random intercept in spline coefficients
sigma2_s    <- sigma2_s[burnt,]                 # Save variances for country-specific random slope in spline coefficients
alpha       <- alpha[burnt,]                    # Save latent variable (alpha)
zeta        <- zeta[burnt,]                 # Save probit of proportion of q over p
deviance    <- deviance[burnt]                  # Save deviance
tracePlots()                                    # Output final traceplots
save.image(paste0(outdirname, filename, "Burnt.RData")) # Save R workspace containing results
