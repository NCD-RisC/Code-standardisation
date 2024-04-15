# NCD-RisC
# Example of making age groups
# January 2024

source('../R/make_age_groups.R')

library(dplyr)

data <- read.csv('../data/USA NHANES 2017-2018.csv')

# get risk factor specific min and max age
data$age_min  <- with(data, ifelse(sex == "male", age_min_anthro_M, age_min_anthro_F))
data$age_max  <- with(data, ifelse(sex == "male", age_max_anthro_M, age_max_anthro_F))

# make standard age groups
ages <- make_age_groups(data$age, data$age_min, data$age_maxm, anthro = TRUE)

# assign to the data frame, accounting for exceptions

# age are already in age groups for US EPESEs, FHS, PRHHP and CHS; except HSE
age_list <- which(!is.na(data$age_group) & !data$id_study%in%c("GBR_2015_HSE","GBR_2016_HSE","GBR_2017_HSE","GBR_2018_HSE","GBR_2019_HSE"))  # drop HSE as it can be taken care of with normal methods
print(unique(data$id_study[age_list]))
age_mean0 <- age_group0 <- rep(NA, nrow(data))
age_mean0[age_list]  <- data$age_mean[age_list]
age_group0[age_list] <- as.character(data$age_group[age_list])

# assign standard age groups
ages$age_group <- gsub('-200', '+', ages$age_group)
data$age_mean <- ages$age_mean
data$age_group <- ages$age_group

# deal with special cases (taken from BP: it may differ according to risk factor)

# special single age group treatment for Finland YFS, Sweden PSWG
x <- which(data$survey_short%in%c("YFS_urban","YFS_rural","PSWG","Oulu35"))
data$age_mean[x] <- data$age[x]
data$age_group[x] <- paste(data$age[x],data$age[x],sep="-")

# split 60-69 in GBR_2000/2005/2006_HSE; DX/TX was asked 65+; 20180422
list1 <- which(data$id_study%in%c("GBR_2000_HSE","GBR_2005_HSE","GBR_2006_HSE")&data$age_group=="60-69"&data$age<65)
list2 <- which(data$id_study%in%c("GBR_2000_HSE","GBR_2005_HSE","GBR_2006_HSE")&data$age_group=="60-69"&data$age>=65)
data$age_group[list1] <- "60-64"
data$age_max[list1] <- 64; data$age_mean[list1] <- (60+65)/2
data$age_group[list2] <- "65-69"
data$age_min[list2] <- 65; data$age_mean[list2] <- (65+70)/2

# split 30-39 in BRA_2013_PNS; DX/TX was asked 35+; 20191107
list1 <- which(data$id_study%in%c("BRA_2013_PNS")&data$age_group=="30-39"&data$age<35)
list2 <- which(data$id_study%in%c("BRA_2013_PNS")&data$age_group=="30-39"&data$age>=35)
data$age_group[list1] <- "30-64"
data$age_max[list1] <- 34; data$age_mean[list1] <- (30+35)/2
data$age_group[list2] <- "35-39"
data$age_min[list2] <- 35; data$age_mean[list2] <- (35+40)/2

# restore age that are already in age groups
data$age_mean[age_list]  <- age_mean0[age_list]
data$age_group[age_list] <- age_group0[age_list]
