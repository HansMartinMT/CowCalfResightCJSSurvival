#Calf Resight Model Evaluation
model.parms <- c( "month_eff","dens_eff","mu_phi","phi","migr_eff","b0.phi","b0_p","b0.p","annual")
# run the MCMC chain in JAGS
cjs.result <- jags.parallel( 
  data = YHT.Calf.Obs.Data, 
  inits = NULL,
  parameters.to.save = model.parms,
  "cjs_nTime_Mig_nCov.txt",
  n.chains = 3, 
  n.iter = 30000, 
  n.burnin = 28000,
  n.thin = 5,
  digits = 4
)

cjs.result1$BUGSoutput$mean$annual
mcmcplot(cjs.result1)

cjs.result1 <- jags.parallel( 
  data = YHT.Calf.Obs.Data, 
  inits = NULL,
  parameters.to.save = model.parms,
  "cjs_TimeY_MigRes_nCov.txt",
  n.chains = 3, 
  n.iter = 30000, 
  n.burnin = 28000,
  n.thin = 5,
  digits = 4
)

cjs.result2 <- jags.parallel( 
  data = YHT.Calf.Obs.Data, 
  inits = NULL,
  parameters.to.save = model.parms,
  "cjs_Time_MigRes_nCov.txt",
  n.chains = 3, 
  n.iter = 30000, 
  n.burnin = 28000,
  n.thin = 5,
  digits = 4
)

cjs.result3 <- jags.parallel( 
  data = YHT.Calf.Obs.Data, 
  inits = NULL,
  parameters.to.save = model.parms,
  "cjs_TimeY_MigRes_Density.txt",
  n.chains = 3, 
  n.iter = 30000, 
  n.burnin = 28000,
  n.thin = 5,
  digits = 4
)

results1<-tibble::tibble(
  Surv=cjs.result$BUGSoutput$mean$annual[1,],
  SD=cjs.result$BUGSoutput$sd$annual[1,],
  Year=year_index_lookup %>% arrange(Index_year) %>% pull(Year)) %>% 
  mutate(LCI=Surv-(2*SD),
         UCI=Surv+(2*SD),
         Migr="resident"
  ) 

results2<-tibble::tibble(
  Surv=cjs.result$BUGSoutput$mean$annual[2,],
  SD=cjs.result$BUGSoutput$sd$annual[2,],
  Year=year_index_lookup %>% arrange(Index_year) %>% pull(Year)) %>% 
  mutate(LCI=Surv-(2*SD),
         UCI=Surv+(2*SD),
         Migr="migrant"
  )
results3<-tibble::tibble(
  Surv=cjs.result$BUGSoutput$mean$annual[3,],
  SD=cjs.result$BUGSoutput$sd$annual[3,],
  Year=year_index_lookup %>% arrange(Index_year) %>% pull(Year)) %>% 
  mutate(LCI=Surv-(2*SD),
         UCI=Surv+(2*SD),
         Migr="west"
  )

results<-bind_rows(results1,results2) %>% 
  select(Year,Migr,Surv,SD,LCI,UCI) 
write.csv(results,"results_cjs_nTime_MigRes_nCov.csv")  

results %>% ggplot2::ggplot(aes(x=Year,y=Surv,color=Migr))+
  geom_point()+
  geom_errorbar(aes(ymin=LCI, ymax=UCI), width=.1)+
  geom_line()
