################################################################################
##### FUNCTIONS ################################################################
################################################################################




##### Gibbs and Gibbs-like update functions ####################################
################################################################################
##### Non-linear trend proposal function #######################################
u.prop.function <- function(i, tPtsPer, V1Inv.diag, Vinv.M, theta.star, theta.old, u.old) {
################################################################################
### This function is used to propose new values for the non-linear change      #
### over time component of the model. It is based on "Gaussian Markov          #
### Random Fields: Theory and Applications, Rue H, Held L, Chapman and         #
### Hall 2005". A full description of the application to metabolic risks       #
### data is provided in Finucane et al and the appendix of Danaei et al.       #
### There are three possible scenarios: no data observed in a country, one     #
### year of data observed, or two or more years observed                       #
################################################################################
    val.sw <- paste("val",tPtsPer[i],sep='')                                            # Produces a character string containing the number of time points for a country
    if ( tPtsPer[i]==2 ) {
        var.temp  <- V1Inv.diag[((i-1)*T+1):(i*T)]
        var.check <- min(var.temp[var.temp!=0])
        if (var.check<.1) val.sw <- "val1"
    }
    switch(val.sw,                                                                      # Calls appropriate section of function depending on number of time points for a country
        val0 = {                                                                        # No data for that country
            uStar                           <- eigenNoData$vec %*% (1/ sqrt(exp(theta.star)*eigenNoData$val) * rnorm(T))
            uStar.old                       <- eigenNoData$vec %*% (1/ sqrt(exp(theta.old)* eigenNoData$val) * rnorm(T))
            dens                            <- 1/2 * (T-2)*theta.star - 1/2 * t(uStar) %*% (exp(theta.star) * SigmaGenInvNoTheta %*% uStar)
            dens.old                        <- 1/2 * (T-2)*theta.old  - 1/2 * t(u.old[((i-1)*T+1):(i*T)]) %*% (exp(theta.old) * SigmaGenInvNoTheta %*% u.old[((i-1)*T+1):(i*T)])
            return(list(u.star=uStar, dens.star=dens, u.star.old=uStar.old, dens.old=dens.old))
            },
        val1 = {                                                                        # One datapoint for that country
            Q                               <- exp(theta.star) * P
            diag(Q)                         <- diag(Q) + V1Inv.diag[((i-1)*T+1):(i*T)]
            Q.eigen                         <- eigen(Q)
            Q.eigen$values[T]               <- Inf
            Qinv                            <- Q.eigen$vectors %*% (1/Q.eigen$values * t(Q.eigen$vectors))
            correct.factor                  <- Qinv %*% matrix(1, T, T) / sum(Qinv)
            mu                              <- Qinv %*% Vinv.M[((i-1)*T+1):(i*T)]
            muStar                          <- mu - correct.factor %*% mu
            SigmaStar                       <- Qinv - correct.factor %*% Qinv
            SigmaStar.eigen                 <- eigen(SigmaStar)
            SigmaStar.eigen$val[(T-1):T]    <- 0
            EigenValGenInv                  <- 1/SigmaStar.eigen$val
            EigenValGenInv[(T-1):T]         <- 0
            SigmaGenInv                     <- SigmaStar.eigen$vec %*% (EigenValGenInv * t(SigmaStar.eigen$vec))
            uStar                           <- muStar + SigmaStar.eigen$vec %*% (sqrt(SigmaStar.eigen$val) * rnorm(T))
            dens                            <- -1/2 * sum(log(SigmaStar.eigen$val[1:(T-2)])) -1/2 * t(uStar - muStar) %*% (SigmaGenInv %*% (uStar - muStar))
            Q                               <- exp(theta.old) * P
            diag(Q)                         <- diag(Q) + V1Inv.diag[((i-1)*T+1):(i*T)]
            Q.eigen                         <- eigen(Q)
            Q.eigen$values[T]               <- Inf
            Qinv                            <- Q.eigen$vectors %*% (1/Q.eigen$values * t(Q.eigen$vectors))
            correct.factor                  <- Qinv %*% matrix(1, T, T) / sum(Qinv)
            mu                              <- Qinv %*% Vinv.M[((i-1)*T+1):(i*T)]
            muStar                          <- mu - correct.factor %*% mu
            SigmaStar                       <- Qinv - correct.factor %*% Qinv
            SigmaStar.eigen                 <- eigen(SigmaStar)
            SigmaStar.eigen$val[(T-1):T]    <- 0
            EigenValGenInv                  <- 1/SigmaStar.eigen$val
            EigenValGenInv[(T-1):T]         <- 0
            SigmaGenInv                     <- SigmaStar.eigen$vec %*% (EigenValGenInv * t(SigmaStar.eigen$vec))
            uStar.old                       <- muStar + SigmaStar.eigen$vec %*% (sqrt(SigmaStar.eigen$val) * rnorm(T))
            dens.old                        <- -1/2*sum(log(SigmaStar.eigen$val[1:(T-2)])) - 1/2*t(u.old[((i-1)*T+1):(i*T)] - muStar) %*% (SigmaGenInv %*% (u.old[((i-1)*T+1):(i*T)] -
                                                muStar))
            return(list(u.star=uStar, dens.star=dens, u.star.old=uStar.old, dens.old=dens.old))
            },
            {                                                                           # Two or more datapoints for that country
            Q                               <- exp(theta.star) * P
            diag(Q)                         <- diag(Q) + V1Inv.diag[((i-1)*T+1):(i*T)]
            Qinv                            <- solve(Q)
            correct.factor                  <- Qinv %*% t(A) %*% solve(A %*% Qinv %*% t(A)) %*% A
            mu                              <- Qinv %*% Vinv.M[((i-1)*T+1):(i*T)]
            muStar                          <- mu - correct.factor %*% mu
            SigmaStar                       <- Qinv - correct.factor %*% Qinv
            SigmaStar.eigen                 <- eigen(SigmaStar)
            SigmaStar.eigen$val[(T-1):T]    <- 0
            EigenValGenInv                  <- 1/SigmaStar.eigen$val
            EigenValGenInv[(T-1):T]         <- 0
            SigmaGenInv                     <- SigmaStar.eigen$vec %*% (EigenValGenInv * t(SigmaStar.eigen$vec))
            uStar                           <- muStar + SigmaStar.eigen$vec %*% (sqrt(SigmaStar.eigen$val) * rnorm(T))
            dens                            <- -1/2 * sum(log(SigmaStar.eigen$val[1:(T-2)])) -1/2 * t(uStar - muStar) %*% (SigmaGenInv %*%  (uStar - muStar))
            Q                               <- exp(theta.old) * P
            diag(Q)                         <- diag(Q) + V1Inv.diag[((i-1)*T+1):(i*T)]
            Qinv                            <- solve(Q)
            correct.factor                  <- Qinv %*% t(A) %*% solve(A %*% Qinv %*% t(A)) %*% A
            mu                              <- Qinv %*% Vinv.M[((i-1)*T+1):(i*T)]
            muStar                          <- mu - correct.factor %*% mu
            SigmaStar                       <- Qinv - correct.factor %*% Qinv
            SigmaStar.eigen                 <- eigen(SigmaStar)
            SigmaStar.eigen$val[(T-1):T]    <- 0
            EigenValGenInv                  <- 1/SigmaStar.eigen$val
            EigenValGenInv[(T-1):T]         <- 0
            SigmaGenInv                     <- SigmaStar.eigen$vec %*% (EigenValGenInv * t(SigmaStar.eigen$vec))
            uStar.old                       <- muStar + SigmaStar.eigen$vec %*% (sqrt(SigmaStar.eigen$val) * rnorm(T))
            dens.old                        <- -1/2 * sum(log(SigmaStar.eigen$val[1:(T-2)])) -1/2 * t(u.old[((i-1)*T+1):(i*T)] - muStar) %*% (SigmaGenInv %*% (u.old[((i-1)*T+1):(i*T)] -
                                                muStar))
            return(list(u.star=uStar, dens.star=dens, u.star.old=uStar.old, dens.old=dens.old)) } ) }




################################################################################
##### Functions used in Metropolis-Hastings updates                            #
################################################################################
##### Function used to calculate log likelihoods
LogLik <- function(alpha, zeta) {
    p <- pnorm(alpha)
    q <- pnorm(zeta) * p
    r <- (p - q) / (1 - q)

    lik1 <- dbinom(y_P, number, p[flag == 0], log = TRUE)
    lik2 <- dbinom(y_Q, number_q, q[flag == 1], log = TRUE)
    lik3 <- dbinom(y_R, number_r, r[flag == 1], log = TRUE)

    res <- rep(NA, I)
    res[flag == 0] <- lik1
    res[flag == 1] <- lik2 + lik3

    return(res)
}

##### Function used to calculate log likelihood for study-specific random effects
ssreLik <- function(N, phi, ssre) {
    return((-N*phi - sum(ssre^2)/exp(phi))/2)
}

##### Function used to calculate log prior for study-specific random effects
LogPriorPhi <- function(phi) {
    return(phi/2)
}

##### Functions used in calculation of the log posteriors for the precision  ###
##### parameters for non-linear trends as described in Danaei et al pp.14-16 ###
CalcThetaPost <- function(i, uvw) {
    return(uvw[((i-1)*T+1):(i*T)] %*% P %*% uvw[((i-1)*T+1):(i*T)])
}
LogPostThetaC <- function(theta_c, u) {
    return((J*(T-2)-1) * theta_c / 2 - exp(theta_c)/2 * sum(unlist(lapply(1:J, CalcThetaPost, u))))
}
LogPostThetaR <- function(theta_r, v) {
    return((sum(multipleRegionsInSregion)*(T-2)-1) * theta_r / 2 - exp(theta_r)/2 * sum(unlist(lapply((1:K)[multipleRegionsInSregion], CalcThetaPost, v))))
}
LogPostThetaS <- function(theta_s, sv) {
    return((L*(T-2)-1) * theta_s / 2 - exp(theta_s)/2 * sum(unlist(lapply(1:L, CalcThetaPost, sv))))
}
LogPostThetaG <- function(theta_g, w) {
    return((T-2-1) * theta_g / 2 - exp(theta_g)/2 * w %*% P %*% w)
}




################################################################################
##### Tuning function to help efficient MCMC updates                           #
##### See Gelman, Roberts, Gilk, Bayesian Statistics 5, pp.599-607             #
################################################################################
adaptJump <- function(n, pjump, pgoal=NULL, max.mult=5, type='simple', i=NULL, K=NULL) {
    pjump[pjump==1] <- 0.999
    if (is.null(pgoal)) {
        const <- rep(log(.44),length(n))                                                   # One dimensional jump
        const[n==2] <- log(.35)                                                            # Two dimensional jump
        const[n==3] <- log(.32)                                                            # Three dimensional jump
        const[n>3] <- log(.25)                                                             # More than three dimensional jump (slightly conservative)
    }
    else {
        const <- log(pgoal)
    }

    if (type=='simple') {
        if (length(n)==1) {
            return(min(max.mult,max(1/max.mult,const/log(pjump))))
        }
        else {
            return(pmin(max.mult,pmax(1/max.mult,const/log(pjump))))
        }
    }
    else if (type=='ben') {
        c0 <- 10
        c1 <- .8
        pgoal <- exp(const)
        gamma <- c0/((i/K)^c1)
        return(exp(gamma*(pjump-pgoal)))
    }
}




################################################################################
##### Other utility functions                                                  #
################################################################################
##### Function used to calculate a 10 year weighted average for covariates
TenYearWeightedAvg <- function(var) WMA(var,n=10,w=(1:10)/sum(1:10))

##### Function used to multiply age mat to country/region/superregion match matrix
ageMat_multiplies <- function(match_matrix) {
    res <- match_matrix * ageMat[,1]
    for (i in 2:ncol(ageMat)) res <- cbind(res, match_matrix * ageMat[,i])
    return(res)
}

##### Function used to print deviance
printDeviance <- function() {
    mean.deviance   <- round(mean(deviance[burnt], na.rm=TRUE),2)                                           # Calculates estimated mean deviance
    p_D             <- round(var(deviance[burnt],  na.rm=TRUE)/2,2)                                         # Calculates effective number of parameters
    dic             <- round(mean(deviance[burnt], na.rm=TRUE) + var(deviance[burnt],na.rm=TRUE)/2, 2)      # Calculates DIC
    cat("mean deviance =", mean.deviance, "\n", "p_d =", p_D, "\n", "p =", p, "\n", "DIC =", dic, "\n")     # Prints values to screen
}

##### Function used to update theta
update.theta <- function(SigmaInv.diag, VInv.diag, u, v, sv, w, a) {
    
    Q       <- F.spam.prime %*% (SigmaInv.diag * F.spam)                                # Full conditional precision of theta
    diag(Q) <- diag(Q) + VInv.diag                                                      # Full conditional precision of theta
    U       <- update.spam.chol.NgPeyton(U.theta.init, Q)                               # Updates the Cholesky decomposition based on updated conditional precision
    VinvM   <- F.prime %*% (SigmaInv.diag * (a - (u[which.countryTime] + v[which.regionTime] + sv[which.sregionTime] + w[which.time]) - varphiPlusC.age))
                                                                                        # Full conditional precision times full conditional mean
    return(backsolve(U, forwardsolve(U, VinvM, trans=TRUE) + rnorm(length(VinvM))))     # Returns updated theta following Gibbs step
}




################################################################################
##### Trace plot function                                                      #
##### This function prints traceplots for a selection of parameters to a pdf   #
##### document. The list of parameters within the function may be changed      #
##### without affecting the remainder of the code                              #
################################################################################
tracePlots <- function() {
    pdf(paste0(outdirname, filename, "_univariate_traceplots",".pdf"), width=10, height=7.5); par(mfrow=c(3,2), mar=c(0.5,4,0.5,0))  # Opens PDF document
        plot(phi_s,     type='l', xlab='', ylab='phi_s')                   # Log variance for normal prior for superregion random intercepts      (log kappa_a^r)
        plot(phi_r,     type='l', xlab='', ylab='phi_r')                   # Log variance for normal prior for region random intercepts           (log kappa_a^s)
        plot(phi_c,     type='l', xlab='', ylab='phi_c')                   # Log variance for normal prior for country random intercepts          (log kappa_a^c)
        plot(eta_s,     type='l', xlab='', ylab='eta_s')                   # Log variance for normal prior for superregion random slopes          (log kappa_b^r)
        plot(eta_r,     type='l', xlab='', ylab='eta_r')                   # Log variance for normal prior for region random slopes               (log kappa_b^s)
        plot(eta_c,     type='l', xlab='', ylab='eta_c')                   # Log variance for normal prior for country random slopes              (log kappa_b^c)
        plot(phi_natl,  type='l', xlab='', ylab='phi_natl')                   # Log variance of random effects for national studies (analogous to collapsed log nu_w and log nu_u)
        plot(phi_subn,  type='l', xlab='', ylab='phi_subn')                   # Log variance of random effects for subnational studies               (log nu_s)
        plot(phi_comm,  type='l', xlab='', ylab='phi_comm')                   # Log variance of random effects for community studies                 (log nu_c)
        plot(tau,       type='l', xlab='', ylab='tau')                   # Log variance for within-study errors that differ between age groups  (log tau)
        plot(theta_c,   type='l', xlab='', ylab='theta_c')                   # Log precision parameter for random walk at country level             (log lambda_c)
        plot(theta_r,   type='l', xlab='', ylab='theta_r')                   # Log precision parameter for random walk at region level              (log lambda_s)
        plot(theta_s,   type='l', xlab='', ylab='theta_s')                   # Log precision parameter for random walk at superregion level         (log lambda_r)
        plot(theta_g,   type='l', xlab='', ylab='theta_g')                   # Log precision parameter for random walk at global level              (log lambda_g)
        plot(deviance,  type='l', xlab='', ylab='deviance')                   # Deviance
        plot(theta[,1], type='l', xlab='', ylab='a_g')                   # Global random intercept
        plot(theta[,2], type='l', xlab='', ylab='b_g')                   # Global random slope
        for(i in 1:p)   plot(theta[,2+2*L+2*sum(multipleRegionsInSregion)+2*J+N+i], type='l', xlab='', ylab=paste('covar', i))   # Covariate terms  (beta matrix)
        for(i in 1:(2*ng))  plot(gamma[,i], type='l', xlab='', ylab=paste('gamma', i))           # Age model parameters: global random intercept and slope (log psi and log phi)
        for(i in 1:ng) { plot(log(sigma1_c[,i]), type='l', xlab='', ylab=paste('sigma1_c', i)) } # Log variances for country-specific random intercept in spline coefficients (log c)
        for(i in 1:ng) { plot(log(sigma2_c[,i]), type='l', xlab='', ylab=paste('sigma2_c', i)) } # Log variances for country-specific random slope in spline coefficients (log omiga_c)
        for(i in 1:ng) { plot(log(sigma1_r[,i]), type='l', xlab='', ylab=paste('sigma1_r', i)) } # Log variances for region-specific random intercept in spline coefficients (log r)
        for(i in 1:ng) { plot(log(sigma2_r[,i]), type='l', xlab='', ylab=paste('sigma2_r', i)) } # Log variances for region-specific random slope in spline coefficients (log omiga_r)
        for(i in 1:ng) { plot(log(sigma1_s[,i]), type='l', xlab='', ylab=paste('sigma1_s', i)) } # Log variances for superregion-specific random intercept in spline coefficients (log s)
        for(i in 1:ng) { plot(log(sigma2_s[,i]), type='l', xlab='', ylab=paste('sigma2_s', i)) } # Log variances for superregion-specific random slope in spline coefficients (log omiga_s)
    dev.off()  # Closes PDF document
}

#### END OF FUNCTIONS ##########################################################
