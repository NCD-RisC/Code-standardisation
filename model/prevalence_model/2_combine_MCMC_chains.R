## Combine chains MCMC - Ado prev model
# NP April 2023 adapted from AM 2022

library(rstan)
library(ggplot2)
library(dplyr)
library(tidyr)
library(grid)
library(gridExtra)
library(gtools)

source("2_convergence_plots_functions_adult_DM.R", local = environment())

#### SCRIPT #############
folder <- paste0(variable, "_",sex)
setwd(paste0(parentdir,"/",folder))

com_name   <- paste0("Model", modelnum, "_", sex, "_", variable,"_Combined")
all.files  <- list.files()

data.files <- mixedsort(list.files(pattern = "Burnt.RData", all.files = FALSE, full.names = TRUE))
Num.Length <- round(num_iterations_to_save/Num.Chains)
Num.first  <- num_iterations_to_save - Num.Length * (Num.Chains - 1)

##### Combine chains ##################################################
##### Run 1 ###########################################################
print(data.files[1])
load(data.files[1])
seed_seqs <- c(seedVal)

burnt                <- seq(burn_length + 1, burn_length + num_iterations_to_save, length=Num.first)
u.combined           <- u[burnt,]                        # Save component of nonlinear trend at national level
v.combined           <- v[burnt,]                        # Save component of nonlinear trend at regional level
sv.combined          <- sv[burnt,]                       # Save component of nonlinear trend at superregional level
w.combined           <- w[burnt,]                        # Save component of nonlinear trend at global level
theta.combined       <- theta[burnt,]                    # Save theta matrix
phi_natl.combined    <- phi_natl[burnt]                  # Save log variance of random effects for national studies (log nu_"national")
phi_subn.combined    <- phi_subn[burnt]                  # Save log variance of random effects for subnational studies (log nu_s)
phi_comm.combined    <- phi_comm[burnt]                  # Save log variance of random effects for community studies (log nu_c)
tau.combined         <- tau[burnt]                       # Save log variance for within-study errors that differ between age groups (log tau)
theta_c.combined     <- theta_c[burnt]                   # Save log precision parameter for random walk at country level (log lambda_c)
theta_r.combined     <- theta_r[burnt]                   # Save log precision parameter for random walk at region level (log lambda_s)
theta_s.combined     <- theta_s[burnt]                   # Save log precision parameter for random walk at superregion level (log lambda_r)
theta_g.combined     <- theta_g[burnt]                   # Save log precision parameter for random walk at global level (log lambda_g)
phi_c.combined       <- phi_c[burnt]                     # Save log variance for normal prior for country random intercepts (log kappa_a^c)
phi_r.combined       <- phi_r[burnt]                     # Save log variance for normal prior for region random intercepts (log kappa_a^s)
phi_s.combined       <- phi_s[burnt]                     # Save log variance for normal prior for superregion random intercepts (log kappa_a^r)
eta_c.combined       <- eta_c[burnt]                     # Save log variance for normal prior for country random slopes (log kappa_b^c)
eta_r.combined       <- eta_r[burnt]                     # Save log variance for normal prior for region random slopes (log kappa_b^s)
eta_s.combined       <- eta_s[burnt]                     # Save log variance for normal prior for superregion random slopes (log kappa_b^r)
gamma.combined       <- gamma[burnt,]                    # Save age model parameters
sigma1_c.combined    <- sigma1_c[burnt,]                 # Save variances for country-specific random intercept in spline coefficients
sigma2_c.combined    <- sigma2_c[burnt,]                 # Save variances for country-specific random slope in spline coefficients
sigma1_r.combined    <- sigma1_r[burnt,]                 # Save variances for country-specific random intercept in spline coefficients
sigma2_r.combined    <- sigma2_r[burnt,]                 # Save variances for country-specific random slope in spline coefficients
sigma1_s.combined    <- sigma1_s[burnt,]                 # Save variances for country-specific random intercept in spline coefficients
sigma2_s.combined    <- sigma2_s[burnt,]                 # Save variances for country-specific random slope in spline coefficients
deviance.combined    <- deviance[burnt]                # Save variances for sregion-specific random slope in spline coefficients

alpha.combined       <- alpha[burnt,]                    # Save latent variable (alpha)
zeta.combined       <- zeta[burnt,]                    # Save latent variable (alpha)



sink(paste0(Num.Chains,".txt"))
sink()

##### Runs 2 to 10 ####################################################
for(RunNum in 2:Num.Chains){
    data.files <- mixedsort(list.files(pattern = "Burnt.RData", all.files = FALSE, full.names = TRUE))
    print(data.files[RunNum])
    load(data.files[RunNum])
    seed_seqs <- c(seed_seqs, seedVal)
    
    burnt  <- seq(burn_length + 1,burn_length + num_iterations_to_save, length = Num.Length)
    u.combined      	<- rbind(u.combined, u[burnt,])
    v.combined       	<- rbind(v.combined, v[burnt,])
    sv.combined      	<- rbind(sv.combined, sv[burnt,])
    w.combined       	<- rbind(w.combined, w[burnt,])
    theta.combined   	<- rbind(theta.combined, theta[burnt,])
    phi_natl.combined <- c(phi_natl.combined, phi_natl[burnt])
    phi_subn.combined <- c(phi_subn.combined, phi_subn[burnt])
    phi_comm.combined <- c(phi_comm.combined, phi_comm[burnt])
    tau.combined 		  <- c(tau.combined, tau[burnt])
    
    theta_g.combined 	<- c(theta_g.combined, theta_g[burnt])
    theta_s.combined 	<- c(theta_s.combined, theta_s[burnt])
    theta_r.combined 	<- c(theta_r.combined, theta_r[burnt])
    theta_c.combined 	<- c(theta_c.combined, theta_c[burnt])
    phi_c.combined   	<- c(phi_c.combined, phi_c[burnt])
    phi_r.combined   	<- c(phi_r.combined, phi_r[burnt])
    phi_s.combined   	<- c(phi_s.combined, phi_s[burnt])
    eta_c.combined   	<- c(eta_c.combined, eta_c[burnt])
    eta_r.combined   	<- c(eta_r.combined, eta_r[burnt])
    eta_s.combined   	<- c(eta_s.combined, eta_s[burnt])
    gamma.combined   	<- rbind(gamma.combined, gamma[burnt,])
    sigma1_c.combined <- rbind(sigma1_c.combined, sigma1_c[burnt,])
    sigma1_r.combined <- rbind(sigma1_r.combined, sigma1_r[burnt,])
    sigma1_s.combined <- rbind(sigma1_s.combined, sigma1_s[burnt,])
    
    sigma2_c.combined <- rbind(sigma2_c.combined,sigma2_c[burnt,])                 # Save variances for country-specific random slope in spline coefficients
    sigma2_r.combined <- rbind(sigma2_r.combined,sigma2_r[burnt,])               # Save variances for country-specific random slope in spline coefficients
    sigma2_s.combined <- rbind(sigma2_s.combined,sigma2_s[burnt,])                 # Save variances for country-specific random slope in spline coefficients
    
    deviance.combined <- c(deviance.combined, deviance[burnt])
    
    alpha.combined    <- rbind(alpha.combined, alpha[burnt,])                    # Save latent variable (alpha)
    zeta.combined     <- rbind(zeta.combined, zeta[burnt,])
}


### COMBINE ALL AND REMOVE INTEREMDIATE OBJECTS ###############
burnt    <- c(1:num_iterations_to_save)
u  		   <- u.combined[burnt, ]
v  		   <- v.combined[burnt, ]
sv 		   <- sv.combined[burnt, ]
w  		   <- w.combined[burnt, ]
theta    <- theta.combined[burnt, ]
phi_natl <- phi_natl.combined[burnt]
phi_subn <- phi_subn.combined[burnt]
phi_comm <- phi_comm.combined[burnt]
tau 		 <- tau.combined[burnt]
theta_g  <- theta_g.combined[burnt]
theta_s  <- theta_s.combined[burnt]
theta_r  <- theta_r.combined[burnt]
theta_c  <- theta_c.combined[burnt]
phi_c    <- phi_c.combined[burnt]
phi_r    <- phi_r.combined[burnt]
phi_s    <- phi_s.combined[burnt]
eta_c    <- eta_c.combined[burnt]
eta_r    <- eta_r.combined[burnt]
eta_s    <- eta_s.combined[burnt]
gamma    <- gamma.combined[burnt,]
sigma1_c <- sigma1_c.combined[burnt,]
sigma1_r <- sigma1_r.combined[burnt,]
sigma1_s <- sigma1_s.combined[burnt,]
sigma2_c <- sigma2_c.combined[burnt,]
sigma2_r <- sigma2_r.combined[burnt,]
sigma2_s <- sigma2_s.combined[burnt,]

deviance	<- deviance.combined[burnt]

alpha  	    <- alpha.combined[burnt, ]
zeta        <- zeta.combined[burnt, ]

rm(u.combined, v.combined, sv.combined, w.combined)
rm(theta.combined)
rm(phi_natl.combined, phi_subn.combined, phi_comm.combined)
rm(tau.combined)
rm(theta_g.combined, theta_s.combined, theta_r.combined, theta_c.combined)
rm(phi_c.combined, phi_r.combined, phi_s.combined, eta_c.combined, eta_r.combined, eta_s.combined)
rm(gamma.combined)
rm(sigma1_c.combined, sigma1_r.combined, sigma1_s.combined)
rm(sigma2_c.combined, sigma2_r.combined, sigma2_s.combined)
rm(deviance.combined)
rm(alpha.combined)
rm(zeta.combined)

attach(covar)
attach(subset)


###################### OUTPUTS #################################################
########## Plots and convergence diagnostic values #############################

combinedtracePlots(suffix_string)
histograms(suffix_string)

a_g <- theta[burnt,1]
b_g <- theta[burnt,2]

for (i in 1:ng){
  assign(paste0("sigma1_c",i),sigma1_c[,i])
  assign(paste0("sigma1_r",i),sigma1_r[,i])
  assign(paste0("sigma1_s",i),sigma1_s[,i])
  assign(paste0("sigma2_c",i),sigma2_c[,i])
  assign(paste0("sigma2_r",i),sigma2_r[,i])
  assign(paste0("sigma2_s",i),sigma2_s[,i])
}


var.names <- c("phi_s","phi_r","phi_c","eta_s","eta_r","eta_c","phi_natl","phi_subn","phi_comm",
               "tau","theta_c","theta_r","theta_s","theta_g","a_g","b_g",
               paste0("sigma1_c",seq(1:ng)),
               paste0("sigma1_r",seq(1:ng)),
               paste0("sigma1_s",seq(1:ng)),
               paste0("sigma2_c",seq(1:ng)),
               paste0("sigma2_r",seq(1:ng)),
               paste0("sigma2_s",seq(1:ng)))


rank_plots      <- list()
evolution_plots <- list()
values          <- NULL

for (var.name in var.names){
  print(paste0("getting plots for variable ", var.name))
  values.tmp                  <- get_all_diagnostic_values(data.matrix(get(var.name)),var.name)
  values                      <- rbind(values,values.tmp)
  rank_plots[[var.name]]      <- make_rank_plots(get(var.name), var.name)
  evolution_plots[[var.name]] <- make_diagnostic_iteration_plots(var.name)
}

write.csv(values, paste0(com_name, "_parameters_convergence_diagnostics",suffix_string,".csv"), row.names = FALSE)

cairo_pdf(filename = paste0(com_name, "_rank_plots", suffix_string,".pdf"), onefile = T)

for (var.name in var.names){
  
  grid.arrange(rank_plots[[var.name]], evolution_plots[[var.name]], nrow = 2, ncol = 1, heights = c(2, 1))
  
}

dev.off()

rm(a_g,b_g,rank_plots,evolution_plots,values,var.names)
rm(list = paste0("sigma1_c",seq(1:ng)))
rm(list = paste0("sigma1_r",seq(1:ng)))
rm(list = paste0("sigma1_s",seq(1:ng)))
rm(list = paste0("sigma2_c",seq(1:ng)))
rm(list = paste0("sigma2_r",seq(1:ng)))
rm(list = paste0("sigma2_s",seq(1:ng)))
