make_age_groups_anthro <- function(age_clean, sex, age_min_anthro_F, age_max_anthro_F, age_min_anthro_M, age_max_anthro_M) {
    agemin <-
        ifelse(age_clean<20,age_clean,
        ifelse(age_clean>=20 & age_clean<30 & age_min_anthro_F>=20 & age_min_anthro_F<30 & sex==2, age_min_anthro_F,
        ifelse(age_clean>=30 & age_clean<40 & age_min_anthro_F>=30 & age_min_anthro_F<40 & sex==2, age_min_anthro_F,
        ifelse(age_clean>=40 & age_clean<50 & age_min_anthro_F>=40 & age_min_anthro_F<50 & sex==2, age_min_anthro_F,
        ifelse(age_clean>=50 & age_clean<60 & age_min_anthro_F>=50 & age_min_anthro_F<60 & sex==2, age_min_anthro_F,
        ifelse(age_clean>=60 & age_clean<70 & age_min_anthro_F>=60 & age_min_anthro_F<70 & sex==2, age_min_anthro_F,
        ifelse(age_clean>=70 & age_clean<80 & age_min_anthro_F>=70 & age_min_anthro_F<80 & sex==2, age_min_anthro_F,
        ifelse(age_clean>=80 & sex==2, 80,
        ifelse(age_clean>=20 & age_clean<30 & age_min_anthro_M>=20 & age_min_anthro_M<30 & sex==1, age_min_anthro_M,
        ifelse(age_clean>=30 & age_clean<40 & age_min_anthro_M>=30 & age_min_anthro_M<40 & sex==1, age_min_anthro_M,
        ifelse(age_clean>=40 & age_clean<50 & age_min_anthro_M>=40 & age_min_anthro_M<50 & sex==1, age_min_anthro_M,
        ifelse(age_clean>=50 & age_clean<60 & age_min_anthro_M>=50 & age_min_anthro_M<60 & sex==1, age_min_anthro_M,
        ifelse(age_clean>=60 & age_clean<70 & age_min_anthro_M>=60 & age_min_anthro_M<70 & sex==1, age_min_anthro_M,
        ifelse(age_clean>=70 & age_clean<80 & age_min_anthro_M>=70 & age_min_anthro_M<80 & sex==1, age_min_anthro_M,
        ifelse(age_clean>=80 & sex==1, 80, floor(age_clean/10)*10)
        ))))))))))))))
    agemax <-
        ifelse(age_clean<20,age_clean,
        ifelse(age_clean>=20 & age_clean<30 & age_max_anthro_F>=20 & age_max_anthro_F<30 & sex==2, age_max_anthro_F,
        ifelse(age_clean>=30 & age_clean<40 & age_max_anthro_F>=30 & age_max_anthro_F<40 & sex==2, age_max_anthro_F,
        ifelse(age_clean>=40 & age_clean<50 & age_max_anthro_F>=40 & age_max_anthro_F<50 & sex==2, age_max_anthro_F,
        ifelse(age_clean>=50 & age_clean<60 & age_max_anthro_F>=50 & age_max_anthro_F<60 & sex==2, age_max_anthro_F,
        ifelse(age_clean>=60 & age_clean<70 & age_max_anthro_F>=60 & age_max_anthro_F<70 & sex==2, age_max_anthro_F,
        ifelse(age_clean>=70 & age_clean<80 & age_max_anthro_F>=70 & age_max_anthro_F<80 & sex==2, age_max_anthro_F,
        ifelse(age_clean>=80 & sex==2, age_max_anthro_F,   
        ifelse(age_clean>=20 & age_clean<30 & age_max_anthro_M>=20 & age_max_anthro_M<30 & sex==1, age_max_anthro_M,
        ifelse(age_clean>=30 & age_clean<40 & age_max_anthro_M>=30 & age_max_anthro_M<40 & sex==1, age_max_anthro_M,
        ifelse(age_clean>=40 & age_clean<50 & age_max_anthro_M>=40 & age_max_anthro_M<50 & sex==1, age_max_anthro_M,
        ifelse(age_clean>=50 & age_clean<60 & age_max_anthro_M>=50 & age_max_anthro_M<60 & sex==1, age_max_anthro_M,
        ifelse(age_clean>=60 & age_clean<70 & age_max_anthro_M>=60 & age_max_anthro_M<70 & sex==1, age_max_anthro_M,
        ifelse(age_clean>=70 & age_clean<80 & age_max_anthro_M>=70 & age_max_anthro_M<80 & sex==1, age_max_anthro_M,
        ifelse(age_clean>=80 & sex==1, age_max_anthro_M,9+floor(age_clean/10)*10)
        ))))))))))))))
    age_group <- paste(agemin, '-', agemax, sep="")
    age_mean  <- ifelse(agemax-agemin==0, agemax, ifelse(agemax==200, 84.91,  agemin+(agemax+1-agemin)/2))
    return(data.frame(age_mean, age_group))
}