#### LOAD IN THE DATASET WE ARE GOING TO TAKE THESE FROM
## we will sample from a multivariate normal with mean given by the posterior mean of parameter values from previous set of runs
## and covariance matrix given by the posterior covariance matrix of all of the parameters multiplied by (1.1^2) 

print(seedVal)

load(paste0(getwd(),"/",variable,"_",sex,"/Model",mod.no,"_",variable,"_",sex,"_initial_values_distribution_windices.RData"))

# SAMPLE one sample and save
ptm <- proc.time()
initial_values <- mvrnorm(n = 1, mu = sampling_means, Sigma = sampling_cov_matrix_scaled)
proc.time() - ptm

filename_save <- paste0("Model",mod.no,"_",variable,"_",sex,"_Seed_",seedVal)


#####carry over index list also
save(initial_values, index_list, file = paste0(outdir,filename_save,"_initial_values_windices.RData"))
print("job complete")
