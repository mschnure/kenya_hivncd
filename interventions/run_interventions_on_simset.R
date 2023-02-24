#####################################################
# Description: Code to run interventions on simset.17 
#####################################################

source("model/run_systematic.R")
source("interventions/extract_intervention_results.R")
print("loading mcmc.17")
load("mcmcruns/mcmc_v17_2023-02-16.Rdata") # v17 gets last few years of incidence better than v18 

mcmc=mcmc.17
simset = suppressWarnings(extract.simset(mcmc,
                                         additional.burn=500, 
                                         additional.thin=14)) # thin=14 --> 105 sims; thin=100 --> 12 sims

RUN.SIMULATIONS.TO.YEAR = 2040
print("running no.int on mcmc.17")
simset.no.int = run.intervention.on.simset(simset,
                                           end.year = RUN.SIMULATIONS.TO.YEAR,
                                           intervention = NO.INTERVENTION)
print("running testing.1 on mcmc.17")
simset.testing.1 = run.intervention.on.simset(simset,
                                              end.year = RUN.SIMULATIONS.TO.YEAR,
                                              intervention = testing.1)
print("running engagement.1 on mcmc.17")
simset.engagement.1 = run.intervention.on.simset(simset,
                                              end.year = RUN.SIMULATIONS.TO.YEAR,
                                              intervention = engagement.1)
print("running gain.suppression.1 on mcmc.17")
simset.gain.suppression.1 = run.intervention.on.simset(simset,
                                                 end.year = RUN.SIMULATIONS.TO.YEAR,
                                                 intervention = gain.suppression.1)
print("running disengagement.unsuppressed.1 on mcmc.17")
simset.disengagement.unsuppressed.1 = run.intervention.on.simset(simset,
                                                       end.year = RUN.SIMULATIONS.TO.YEAR,
                                                       intervention = disengagement.unsuppressed.1)
print("running disengagement.suppressed.1 on mcmc.17")
simset.disengagement.suppressed.1 = run.intervention.on.simset(simset,
                                                                 end.year = RUN.SIMULATIONS.TO.YEAR,
                                                                 intervention = disengagement.suppressed.1)
print("running all.int on mcmc.17")
simset.all.int = run.intervention.on.simset(simset,
                                            end.year = RUN.SIMULATIONS.TO.YEAR,
                                            intervention = all.interventions)

simset.list.full = list(no.int = simset.no.int,
                        testing.1 = simset.testing.1,
                        engagement.1 = simset.engagement.1,
                        gain.suppression.1 = simset.gain.suppression.1,
                        disengagement.unsuppressed.1 = simset.disengagement.unsuppressed.1,
                        disengagement.suppressed.1 = simset.disengagement.suppressed.1,
                        all.int = simset.all.int)

print("generating full.results.array")
full.results.array = generate.full.results.array(simset.list = simset.list)

