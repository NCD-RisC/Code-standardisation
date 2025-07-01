remove(list = ls())
scriptname      <- "2_combine_MCMC_chains.R"
maindir      <- "/rds/general/user/arayner/ephemeral/"

modelnum    <- 110
mod_dir_name <- "model_110_total_diabetes"

parentdir <- paste0(maindir,mod_dir_name,"/")
setwd(parentdir)

sexes <- c("female","male")
vars <- c("prev_diabetes")

script <- paste0(parentdir,scriptname)


num_iterations_to_save <- 5000 # final size of combined cut
burn_length <- 0               # number to further drop from beginning of chains; keep same across outcomes

Num.Chains <- 10

suffix_string <- "_TenForIV"


## Combines chains

for (variable in vars) {
  for (sex in sexes) {
    rstudioapi::jobRunScript(script, 
                             name = paste(variable,sex),
                             workingDir = parentdir,
                             importEnv = TRUE,
                             exportEnv = paste0("Model",modelnum,'_',sex,'_', variable,"_Combined"))
    Sys.sleep(80)
  }
}


## Saves combine chains environment
for (sex in sexes) {
  for (variable in vars) {
    print(paste(sex, variable))
    
    tmp <- get(paste0('Model',modelnum,'_',sex,'_',variable,'_Combined'))
    
    saveRDS(tmp, file = paste0(parentdir,'/',variable,"_",sex,"/Model",modelnum,'_',sex,'_',variable,"_Combined",suffix_string,".RDS"))
  }
}
