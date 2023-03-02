#####################################################
# Description: Code to run interventions on simset.17 
#####################################################

library(bayesian.simulations)
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

print("running all interventions, intermediate level on mcmc.17")
simset.all.intermediate = run.intervention.on.simset(simset,
                                                     end.year = RUN.SIMULATIONS.TO.YEAR,
                                                     intervention = all.intermediate)

print("running engagement/retention only, max level on mcmc.17")
simset.engagement.retention = run.intervention.on.simset(simset,
                                                         end.year = RUN.SIMULATIONS.TO.YEAR,
                                                         intervention = engagement.retention)

print("running all interventions, max level on mcmc.17")
simset.all.max = run.intervention.on.simset(simset,
                                            end.year = RUN.SIMULATIONS.TO.YEAR,
                                            intervention = all.max)

simset.list.full = list(no.int = simset.no.int,
                        all.intermediate = simset.all.intermediate,
                        engagement.retention = simset.engagement.retention,
                        all.max = simset.all.max)

print("generating full.results.array")
full.results.array = generate.full.results.array(simset.list = simset.list.full)

# individual interventions
if(1==2){
    print("running testing.50 on mcmc.17")
    simset.testing.50 = run.intervention.on.simset(simset,
                                                   end.year = RUN.SIMULATIONS.TO.YEAR,
                                                   intervention = testing.50)
    print("running testing.75 on mcmc.17")
    simset.testing.75 = run.intervention.on.simset(simset,
                                                   end.year = RUN.SIMULATIONS.TO.YEAR,
                                                   intervention = testing.75)
    print("running engagement.80 on mcmc.17")
    simset.engagement.80 = run.intervention.on.simset(simset,
                                                      end.year = RUN.SIMULATIONS.TO.YEAR,
                                                      intervention = engagement.80)
    print("running engagement.90 on mcmc.17")
    simset.engagement.90 = run.intervention.on.simset(simset,
                                                      end.year = RUN.SIMULATIONS.TO.YEAR,
                                                      intervention = engagement.90)
    print("running gain.suppression.80 on mcmc.17")
    simset.gain.suppression.80 = run.intervention.on.simset(simset,
                                                            end.year = RUN.SIMULATIONS.TO.YEAR,
                                                            intervention = gain.suppression.80)
    print("running gain.suppression.90 on mcmc.17")
    simset.gain.suppression.90 = run.intervention.on.simset(simset,
                                                            end.year = RUN.SIMULATIONS.TO.YEAR,
                                                            intervention = gain.suppression.90)
    print("running lose.suppression.10 on mcmc.17")
    simset.gain.suppression.10 = run.intervention.on.simset(simset,
                                                            end.year = RUN.SIMULATIONS.TO.YEAR,
                                                            intervention = gain.suppression.10)
    print("running lose.suppression.05 on mcmc.17")
    simset.gain.suppression.05 = run.intervention.on.simset(simset,
                                                            end.year = RUN.SIMULATIONS.TO.YEAR,
                                                            intervention = gain.suppression.05)
    print("running disengagement.unsuppressed.15 on mcmc.17")
    simset.disengagement.unsuppressed.15 = run.intervention.on.simset(simset,
                                                                      end.year = RUN.SIMULATIONS.TO.YEAR,
                                                                      intervention = disengagement.unsuppressed.15)
    print("running disengagement.unsuppressed.10 on mcmc.17")
    simset.disengagement.unsuppressed.10 = run.intervention.on.simset(simset,
                                                                      end.year = RUN.SIMULATIONS.TO.YEAR,
                                                                      intervention = disengagement.unsuppressed.10)
    print("running disengagement.suppressed.15 on mcmc.17")
    simset.disengagement.suppressed.15 = run.intervention.on.simset(simset,
                                                                    end.year = RUN.SIMULATIONS.TO.YEAR,
                                                                    intervention = disengagement.suppressed.15)
    print("running disengagement.suppressed.10 on mcmc.17")
    simset.disengagement.suppressed.10 = run.intervention.on.simset(simset,
                                                                    end.year = RUN.SIMULATIONS.TO.YEAR,
                                                                    intervention = disengagement.suppressed.10)
}



