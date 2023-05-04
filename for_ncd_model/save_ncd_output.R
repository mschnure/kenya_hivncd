source("model/run_systematic.R")
source("for_ncd_model/extract_ncd_output.R")
source("interventions/extract_intervention_results.R")

print("loading mcmc.29")
load("~/Dropbox/Documents_local/Hopkins/PhD/Dissertation/ABM/kenya_hivncd/mcmcruns/mcmc_v29_2023-04-21.Rdata")

mcmc=mcmc.29
simset = suppressWarnings(extract.simset(mcmc,
                                         additional.burn=504, 
                                         additional.thin=4)) 

simset = add.parameters(simset,
                        parameters = runif(simset@n.sim,2030,2040),
                        parameter.names = "cascade.improvement.end.year",
                        parameter.lower.bounds = 2030,
                        parameter.upper.bounds = 2040)

RUN.SIMULATIONS.TO.YEAR = 2040
print("running no.int on mcmc.29")
simset.no.int = run.intervention.on.simset(simset,
                                           end.year = RUN.SIMULATIONS.TO.YEAR,
                                           intervention = NO.INTERVENTION)

print("running retention/suppression on mcmc.29")
simset.retention.suppression = run.intervention.on.simset(simset,
                                                          end.year = RUN.SIMULATIONS.TO.YEAR,
                                                          intervention = retention.suppression)

print("running testing/engagement on mcmc.29")
simset.testing.engagement = run.intervention.on.simset(simset,
                                                       end.year = RUN.SIMULATIONS.TO.YEAR,
                                                       intervention = testing.engagement)

print("running all interventions on mcmc.29")
simset.all = run.intervention.on.simset(simset,
                                        end.year = RUN.SIMULATIONS.TO.YEAR,
                                        intervention = all.max)

# SAVE NEW OUTPUT
khm.no.int.1 = extract.simset.output(simset.no.int,
                                     intervention.id="1")

khm.no.int.2 = extract.simset.output(simset.no.int,
                                     intervention.id="2")

khm.retention.suppression = extract.simset.output(simset.retention.suppression,
                                                  intervention.id="3")

khm.testing.engagement = extract.simset.output(simset.testing.engagement,
                                                  intervention.id="4")

khm.all = extract.simset.output(simset.all,
                                intervention.id="5")

# need to rename everything to khm.full for NCD side to work - save these in sequence 
# Scenario 0
khm.full = khm.no.int.1
save(khm.full,
     file=paste0("~/Dropbox/Documents_local/Hopkins/PhD/Dissertation/ABM/rHivNcd/data/hiv_simset_scenario1.RData"))

# Scenario 1
khm.full = khm.no.int.2
save(khm.full,
     file=paste0("~/Dropbox/Documents_local/Hopkins/PhD/Dissertation/ABM/rHivNcd/data/hiv_simset_scenario2.RData"))

# Scenario 2
khm.full = khm.retention.suppression
save(khm.full,
     file=paste0("~/Dropbox/Documents_local/Hopkins/PhD/Dissertation/ABM/rHivNcd/data/hiv_simset_scenario3.RData"))

# Scenario 3
khm.full = khm.testing.engagement
save(khm.full,
     file=paste0("~/Dropbox/Documents_local/Hopkins/PhD/Dissertation/ABM/rHivNcd/data/hiv_simset_scenario4.RData"))

# Scenario 3
khm.full = khm.all
save(khm.full,
     file=paste0("~/Dropbox/Documents_local/Hopkins/PhD/Dissertation/ABM/rHivNcd/data/hiv_simset_scenario5.RData"))
