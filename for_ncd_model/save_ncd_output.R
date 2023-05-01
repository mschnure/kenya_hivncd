source("model/run_systematic.R")
source("for_ncd_model/extract_ncd_output.R")

## Choose MCMC run 
load("~/Dropbox/Documents_local/Hopkins/PhD/Dissertation/ABM/kenya_hivncd/mcmcruns/mcmc_v29_2023-04-21.Rdata")
mcmc=mcmc.29

# 100 sims for now 
simset.for.ncd = extract.simset(mcmc,
                                additional.burn=500, # throw away first 500
                                additional.thin=20) # thin by 20, 100 remaining

# mcmc output only has years through 2021; need to rerun through 2040
simset.for.ncd = run.intervention.on.simset(simset.for.ncd,
                                            end.year = 2040,
                                            intervention = NO.INTERVENTION)

# simset.for.ncd.all.max = run.intervention.on.simset(simset.for.ncd,
#                                                     end.year = 2040,
#                                                     intervention = all.max)

# SAVE NEW OUTPUT
khm = extract.simset.output(simset.for.ncd,
                            intervention.id="no.int")

# khm.all.max.interventions = extract.simset.output(simset.for.ncd.all.max,
#                                                   intervention.id="all.max.int")


save(khm,
     file=paste0("~/Dropbox/Documents_local/Hopkins/PhD/Dissertation/ABM/rHivNcd/data/hiv_simset_",Sys.Date(),".RData"))
# save(khm.all.max.interventions,
#      file=paste0("~/Dropbox/Documents_local/Hopkins/PhD/Dissertation/ABM/rHivNcd/data/hiv_simset_intervention_",Sys.Date(),".RData"))
