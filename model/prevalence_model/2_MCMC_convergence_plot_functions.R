# NP 2023
# Functions to calculate convergence diagnostics and produce trace and rank plots
# Ado prev BMI (need to change traceplot and histogram plots if age model changes)


##### FUNCTIONS
get_evolution_diagnostics <- function(chains){
  chains <- data.matrix(chains)
  evolution <- NULL
  for (i in 100:nrow(chains)){
    if(i%%100 == 0){
      evolution.tmp <- data.frame(it = i, 
                                  "Rhat" =  Rhat(chains[1:i,]),
                                  "Bulk_ESS" = ess_bulk(chains[1:i,]),
                                  "Tail_ESS" = ess_tail(chains[1:i,]))
      evolution <- rbind(evolution, evolution.tmp)
    }
  }
  return(evolution)
}

get_all_diagnostic_values <- function(chains, var.name){
  rhat.val <- Rhat(chains)
  ess.bulk <- ess_bulk(chains)
  ess.tail <- ess_tail(chains)
  values <- data.frame(var.name = var.name, rhat.val = rhat.val,ess.bulk = ess.bulk,ess.tail = ess.tail)
  return(values)
}

make_rank_plots <- function(chains, var.name){
  chains <- data.frame(Value = chains) %>%
    mutate(Seed = c(rep(seed_seqs[1], Num.first), rep(seed_seqs[2:length(seed_seqs)], each = Num.Length)))
  chains  <- chains[order(chains$Value),]
  chains$rank <- 1:num_iterations_to_save
  
  p <- ggplot(chains, aes(x = rank)) + geom_histogram(breaks = seq(0, nrow(chains), 1000)) +
    facet_wrap(~Seed) + 
    geom_hline(yintercept = 1000/length(seed_seqs))+
    ggtitle(var.name) +
    xlab('')
  
  return(p)
}

make_diagnostic_iteration_plots <- function(var.name){
  chains <- get(paste0(var.name))
  evolution_diagnostics <- get_evolution_diagnostics(chains)
  
  rhat.plot <- ggplot(evolution_diagnostics, aes(x = it, y = Rhat)) +
    geom_point() +
    geom_hline(yintercept = 1.01)
  
  evolution_diagnostics  <- evolution_diagnostics %>%
    select(-Rhat)%>%
    gather(key = "ESS", value = "value", -it)
  
  ess.plot <- ggplot(evolution_diagnostics, aes(x = it, y = value, color = ESS)) +
    geom_point()+
    scale_colour_manual(values = c("Blue", "Black"))+
    geom_hline(yintercept = 400)+
    ylab("Effective Sample Size")+ 
    theme(legend.position = c(0.2,0.7)) +
    theme(legend.title = element_text("ESS"))
  
  p <- arrangeGrob(rhat.plot, ess.plot, nrow = 1, ncol = 2, widths = c(1,1))
}


combinedtracePlots <- function(suffix_string = ""){
  
  seqs <- seq(Num.first,num_iterations_to_save-Num.Length, by=Num.Length)
  seqs2 <- c(seqs, num_iterations_to_save)
  
  pdf(paste0(com_name, "_traceplots_parameters", suffix_string,".pdf"),width=10, height=7.5)
  par(mfrow=c(3,2), mar=c(0.5,4,0.5,0))
  
  plot(phi_s[burnt], type='l', xlab='', ylab='phi_s',xaxt='n')
  axis(1, at = seqs2,labels= seed_seqs, las=2, cex.axis=0.65)
  abline(v=seqs,col="grey")
  
  plot(phi_r[burnt], type='l', xlab='', ylab='phi_r',xaxt='n')
  axis(1, at = seqs2,labels= seed_seqs, las=2, cex.axis=0.65)
  abline(v=seqs,col="grey")
  
  plot(phi_c[burnt], type='l', xlab='', ylab='phi_c',xaxt='n')
  axis(1, at = seqs2,labels= seed_seqs, las=2, cex.axis=0.65)
  abline(v=seqs,col="grey")
  
  plot(eta_s[burnt], type='l', xlab='', ylab='eta_s',xaxt='n')
  axis(1, at = seqs2,labels= seed_seqs, las=2, cex.axis=0.65)
  abline(v=seqs,col="grey")
  
  plot(eta_r[burnt], type='l', xlab='', ylab='eta_r',xaxt='n')
  axis(1, at = seqs2,labels= seed_seqs, las=2, cex.axis=0.65)
  abline(v=seqs,col="grey")
  
  plot(eta_c[burnt], type='l', xlab='', ylab='eta_c',xaxt='n')
  axis(1, at = seqs2,labels= seed_seqs, las=2, cex.axis=0.65)
  abline(v=seqs,col="grey")
  
  plot(phi_natl[burnt], type='l', xlab='', ylab='phi_natl',xaxt='n')
  axis(1, at = seqs2,labels= seed_seqs, las=2, cex.axis=0.65)
  abline(v=seqs,col="grey")
  
  plot(phi_subn[burnt], type='l', xlab='', ylab='phi_subn',xaxt='n')
  axis(1, at = seqs2,labels= seed_seqs, las=2, cex.axis=0.65)
  abline(v=seqs,col="grey")
  
  plot(phi_comm[burnt], type='l', xlab='', ylab='phi_comm',xaxt='n')
  axis(1, at = seqs2,labels= seed_seqs, las=2, cex.axis=0.65)
  abline(v=seqs,col="grey")
  
  plot(tau[burnt], type='l', xlab='', ylab='tau',xaxt='n')
  axis(1, at = seqs2,labels= seed_seqs, las=2, cex.axis=0.65)
  abline(v=seqs,col="grey")
  
  plot(theta_c[burnt], type='l', xlab='', ylab='theta_c',xaxt='n')
  axis(1, at = seqs2,labels= seed_seqs, las=2, cex.axis=0.65)
  abline(v=seqs,col="grey")
  
  plot(theta_r[burnt], type='l', xlab='', ylab='theta_r',xaxt='n')
  axis(1, at = seqs2,labels= seed_seqs, las=2, cex.axis=0.65)
  abline(v=seqs,col="grey")
  
  plot(theta_s[burnt], type='l', xlab='', ylab='theta_s',xaxt='n')
  axis(1, at = seqs2,labels= seed_seqs, las=2, cex.axis=0.65)
  abline(v=seqs,col="grey")
  
  plot(theta_g[burnt], type='l', xlab='', ylab='theta_g',xaxt='n')
  axis(1, at = seqs2,labels= seed_seqs, las=2, cex.axis=0.65)
  abline(v=seqs,col="grey")
  
  plot(theta[burnt,1], type='l', xlab='', ylab='a_g',xaxt='n')
  axis(1, at = seqs2,labels= seed_seqs, las=2, cex.axis=0.65)
  abline(v=seqs,col="grey")
  
  plot(theta[burnt,2], type='l', xlab='', ylab='b_g',xaxt='n')
  axis(1, at = seqs2,labels= seed_seqs, las=2, cex.axis=0.65)
  abline(v=seqs,col="grey")
  
  for(i in 1:p){
    plot(theta[burnt,2+2*L+2*sum(multipleRegionsInSregion)+2*J+N+i], type='l', xlab='', ylab=paste('covar', i),xaxt='n')	
    axis(1, at = seqs2,labels= seed_seqs, las=2, cex.axis=0.65)
    abline(v=seqs,col="grey")		
  }
  for(i in 1:(2*ng)){
    plot(gamma[burnt, i], type='l', xlab='', ylab=paste('gamma', i),xaxt='n')
    axis(1, at = seqs2,labels= seed_seqs, las=2, cex.axis=0.65)
    abline(v=seqs,col="grey")
  }
  for(i in 1:ng){
    plot(log(sigma1_c[burnt, i]), type='l', xlab='', ylab=paste('log sigma1_c', i),xaxt='n')
    axis(1, at = seqs2,labels= seed_seqs, las=2, cex.axis=0.65)
    abline(v=seqs,col="grey")
  }
  for(i in 1:ng){
    plot(log(sigma1_r[burnt, i]), type='l', xlab='', ylab=paste('log sigma1_r', i),xaxt='n')
    axis(1, at = seqs2,labels= seed_seqs, las=2, cex.axis=0.65)
    abline(v=seqs,col="grey")
  }
  for(i in 1:ng){
    plot(log(sigma1_s[burnt, i]), type='l', xlab='', ylab=paste('log sigma1_s', i),xaxt='n')
    axis(1, at = seqs2,labels= seed_seqs, las=2, cex.axis=0.65)
    abline(v=seqs,col="grey")
  }
  
  for(i in 1:ng){
    plot(log(sigma2_c[burnt, i]), type='l', xlab='', ylab=paste('log sigma2_c', i),xaxt='n')
    axis(1, at = seqs2,labels= seed_seqs, las=2, cex.axis=0.65)
    abline(v=seqs,col="grey")
  }
  for(i in 1:ng){
    plot(log(sigma2_r[burnt, i]), type='l', xlab='', ylab=paste('log sigma2_r', i),xaxt='n')
    axis(1, at = seqs2,labels= seed_seqs, las=2, cex.axis=0.65)
    abline(v=seqs,col="grey")
  }
  for(i in 1:ng){
    plot(log(sigma2_s[burnt, i]), type='l', xlab='', ylab=paste('log sigma2_s', i),xaxt='n')
    axis(1, at = seqs2,labels= seed_seqs, las=2, cex.axis=0.65)
    abline(v=seqs,col="grey")
  }
  
  for(i in 1:ng){
    plot(alpha[burnt, i], type='l', xlab='', ylab=paste('alpha', i),xaxt='n')
    axis(1, at = seqs2,labels= seed_seqs, las=2, cex.axis=0.65)
    abline(v=seqs,col="grey")
  }
  
  for(i in 1:ng){
    plot(zeta[burnt, i], type='l', xlab='', ylab=paste('zeta', i),xaxt='n')
    axis(1, at = seqs2,labels= seed_seqs, las=2, cex.axis=0.65)
    abline(v=seqs,col="grey")
  }
  
  dev.off()
}


histograms <- function(suffix_str = ""){
  
  pdf(paste0(com_name, "_univar_posterior",suffix_str,".pdf"),width=10, height=7.5)
  par(mfrow=c(3,2), mar=c(4,4,0.5,0))
  
  hist(phi_s[burnt], xlab='', main='phi_s')
  
  hist(eta_s[burnt],  xlab='', main='eta_s')
  
  hist(phi_r[burnt],  xlab='', main='phi_r')
  
  hist(eta_r[burnt],  xlab='', main='eta_r')
  
  hist(phi_c[burnt],  xlab='', main='phi_c')
  
  hist(eta_c[burnt],  xlab='', main='eta_c')
  
  hist(phi_natl[burnt],  xlab='', main='phi_natl')
  
  hist(phi_subn[burnt],  xlab='', main='phi_subn')
  
  hist(phi_comm[burnt],  xlab='', main='phi_comm')
  
  hist(tau[burnt],  xlab='', main='tau')
  
  hist(theta[burnt,1],  xlab='', main='a_g')
  
  hist(theta[burnt,2],  xlab='', main='b_g')
  
  hist(theta_c[burnt],  xlab='', main='theta_c')
  
  hist(theta_r[burnt],  xlab='', main='theta_r')
  
  hist(theta_s[burnt],  xlab='', main='theta_s')
  
  hist(theta_g[burnt],  xlab='', main='theta_g')
  
  for(i in 1:p){
    hist(theta[burnt,2+2*L+2*sum(multipleRegionsInSregion)+2*J+N+i],  xlab='', main=paste('covar', i))
  }
  for(i in 1:(2*ng)){
    hist(gamma[burnt, i],  xlab='', main=paste('gamma', i))
  }
  for(i in 1:ng){
    hist(log(sigma1_c[burnt, i]),  xlab='', main=paste('log sigma1_c', i))
  }
  for(i in 1:ng){
    hist(log(sigma1_r[burnt, i]),  xlab='', main=paste('log sigma1_r', i))
  }
  for(i in 1:ng){
    hist(log(sigma1_s[burnt, i]),  xlab='', main=paste('log sigma1_s', i))
  }
  
  for(i in 1:ng){
    hist(log(sigma2_c[burnt, i]),  xlab='', main=paste('log sigma2_c', i))
  }
  for(i in 1:ng){
    hist(log(sigma2_r[burnt, i]),  xlab='', main=paste('log sigma2_r', i))
  }
  for(i in 1:ng){
    hist(log(sigma2_s[burnt, i]),  xlab='', main=paste('log sigma2_s', i))
  }
  
  for(i in 1:ng){
    hist(alpha[burnt, i],  xlab='', main=paste('alpha', i))
  }
  
  for(i in 1:ng){
    hist(zeta[burnt, i],  xlab='', main=paste('zeta', i))
  }
  
  dev.off()
}




