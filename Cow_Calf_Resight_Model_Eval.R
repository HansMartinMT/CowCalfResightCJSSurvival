#Calf Resight Model Evaluation
model.parms <- c( "month_eff","migr_eff","b0.phi","annual")
# run the MCMC chain in JAGS
cjs.result <- jags.parallel( 
  data = YHT.Calf.Obs.Data, 
  inits = NULL,
  parameters.to.save = model.parms,
  "cjs_Time_Mig_nCov.txt",
  n.chains = 3, 
  n.iter = 100, 
  n.burnin = 50,
  n.thin = 5,
  digits = 4
)

cjs.result$BUGSoutput$mean$annual
