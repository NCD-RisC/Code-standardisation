clean_single_psu_ssa <- function(tmp){ # ssa = study-sex-age group
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