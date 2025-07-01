#### LOAD IN THE DATASET WE ARE GOING TO TAKE THESE FROM
## we will sample from a multivariate normal with mean given by the posterior mean of parameter values from previous set of runs
## and covariance matrix given by the posterior covariance matrix of all of the parameters multiplied by (1.1^2) 

####
# setwd("E:/model_110_total_diabetes/")
# variable <- "prev_DM_fgl_70_hba1c_65_self_med"
# sex <- "female"
# mod.no <- 110
# seedVal <- 1
######

print(seedVal)

combined_cut <- readRDS(paste0(getwd(),"/",variable,"_",sex,"/Model",mod.no,"_",sex,"_",variable,"_Combined_TenForIV.RDS"))
attach(combined_cut)

#### REPARAMETRISE
# 1. To satisfy condition phi_natl <= phi_subn <= phi_comm 
phi_diff_subn_natl <- log(exp(phi_subn) - exp(phi_natl))
phi_diff_comm_subn <- log(exp(phi_comm) - exp(phi_subn))

# 2. To satisfy condition theta_max >= theta_g >= theta_s >= theta_r >= theta_c
theta.max <- 20
theta.max.prime   <- 1/(exp(theta.max))
theta_g_prime     <- 1/exp(theta_g)
theta_s_prime     <- 1/exp(theta_s)
theta_r_prime     <- 1/exp(theta_r)
theta_c_prime     <- 1/exp(theta_c) 
theta_diff_g_max  <- log(theta_g_prime - theta.max.prime)
theta_diff_s_g    <- log(theta_s_prime - theta_g_prime)
theta_diff_r_s    <- log(theta_r_prime - theta_s_prime)
theta_diff_c_r    <- log(theta_c_prime - theta_r_prime)

# Linear intercepts and slopes
theta_1 <- theta[,1:(2 + 2*L + 2*Z + 2*J)]
# Offsets
theta_2 <- theta[,(2 + 2*L + 2*Z + 2*J + N + 1): (2 + 2*L + 2*Z + 2*J + N + p)]

#list names of model objects
obs <- c("phi_c","phi_r","phi_s",
         "eta_c","eta_r","eta_s",
         "phi_natl","phi_diff_subn_natl","phi_diff_comm_subn",
         "tau",
         "theta_diff_g_max","theta_diff_s_g","theta_diff_r_s","theta_diff_c_r",
         "gamma",
         "sigma1_c","sigma1_r","sigma1_s","sigma2_c","sigma2_r","sigma2_s",
         "theta_1","theta_2",
         "u","v","sv","w",
         "alpha","zeta")   #sample both latent values here and only use non-modelled one in initialising model run

#create data frame to sample values from      
sampling_df <- NULL
for (o in obs) {
  if (grepl("sigma", o)) {
    sampling_df <- cbind(sampling_df, log(get(o))) #log of sigmas
  } else {
    sampling_df <- cbind(sampling_df, get(o))
  }
}
sampling_df <- as.data.frame(sampling_df)


#create a list of indexes for the sampling dataframe
g_dim <- function(x) {
  if (is.matrix(x)) {
    return(dim(x))
  } else if (is.vector(x)) {
    return(c(length(x), 1))
  }
}

index_list <- list()
for (o in obs) {
  
  i <- which(obs == o)
  
  if (i == 1) {
    index_list[[o]] <- 1:g_dim(get(o))[2]
  } else {
    index_list[[o]] <- tail(index_list[[obs[i-1]]], n = 1)  +  1:(g_dim(get(o))[2])
  }
  
}

detach(combined_cut)

#ptm <- proc.time()
sampling_cov_matrix <- cov(sampling_df)
#proc.time() - ptm

#ptm <- proc.time()
sampling_cov_matrix_scaled <- sampling_cov_matrix * 1.5
#proc.time() - ptm
#print("DONE CALCULATING COVARIANCE")

sampling_means <- colMeans(sampling_df)

save(sampling_means,sampling_cov_matrix_scaled, index_list, file = paste0(getwd(),"/",variable,"_",sex,"/",filename_save,"_initial_values_distribution_windices.RData",sep=''))


