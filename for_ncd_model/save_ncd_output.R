source("model/run_systematic.R")
source("for_ncd_model/extract_ncd_output.R")

## Choose MCMC run 
load("mcmcruns/mcmc_v19_2023-03-06.Rdata")
mcmc=mcmc.19

# 100 sims for now 
simset.for.ncd = extract.simset(mcmc,
                                additional.burn=500, # throw away first 500
                                additional.thin=20) # thin by 20, 100 remaining

# mcmc output only has years through 2021; need to rerun through 2040
simset.for.ncd = run.intervention.on.simset(simset.for.ncd,
                                            end.year = 2040,
                                            intervention = NO.INTERVENTION)

# SAVE NEW OUTPUT
khm = extract.simset.output(simset.for.ncd,
                            intervention.id="no.int")


save(khm,file="~/Dropbox/Documents_local/Hopkins/PhD/Dissertation/ABM/rHivNcd/data/hiv_simset.RData")
