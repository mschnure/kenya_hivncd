source("model/run_systematic.R")
source("interventions/create_interventions.R")
source("interventions/intervention_set_v1.R")

load("mcmcruns/mcmc_v17_2023-02-16.Rdata")
load("mcmcruns/mcmc_v18_2023-02-23.Rdata")

RUN.SIMULATIONS.TO.YEAR = 2040

##-------------------------------##
##----- COMPARING 17 AND 18 -----##
##-------------------------------##
simset.17 = extract.simset(mcmc.17,
                           additional.burn=500, 
                           additional.thin=100) # 12 sims


simset.18 = extract.simset(mcmc.18,
                           additional.burn=500, 
                           additional.thin=200) 

simset.17.small = extract.simset(mcmc.17,
                           additional.burn=500, 
                           additional.thin=100) # 12 sims


simset.18.small = extract.simset(mcmc.18,
                           additional.burn=500, 
                           additional.thin=200) 

simset.17.large = extract.simset(mcmc.17,
                                 additional.burn=500, 
                                 additional.thin=14) # 100 sims

simset.18.large = extract.simset(mcmc.18,
                                 additional.burn=500, 
                                 additional.thin=20) # 105 sims

simset.18.large = run.intervention.on.simset(simset.18.large,
                                             end.year = RUN.SIMULATIONS.TO.YEAR,
                                             intervention = NO.INTERVENTION)

simset.19.large = extract.simset(mcmc.19,
                                 additional.burn=500, 
                                 additional.thin=5) # 5 --> 400 sims; 20 --> 100 sims

simset.19.large = run.intervention.on.simset(simset.19.large,
                                             end.year = RUN.SIMULATIONS.TO.YEAR,
                                             intervention = NO.INTERVENTION)

simset.no.int.17 = run.intervention.on.simset(simset.17,
                                              end.year = RUN.SIMULATIONS.TO.YEAR,
                                              intervention = NO.INTERVENTION)

simset.no.int.18 = run.intervention.on.simset(simset.18,
                                              end.year = RUN.SIMULATIONS.TO.YEAR,
                                              intervention = NO.INTERVENTION)

simset.all.int.17 = run.intervention.on.simset(simset.17,
                                               end.year = RUN.SIMULATIONS.TO.YEAR,
                                               intervention = all.interventions)

simset.all.int.18 = run.intervention.on.simset(simset.18,
                                               end.year = RUN.SIMULATIONS.TO.YEAR,
                                               intervention = all.interventions)

simplot(simset.no.int.17,simset.all.int.17, years=2010:2030, facet.by='age', data.types='incidence', show.individual.sims = F)
simplot(simset.no.int.18,simset.all.int.18, years=2010:2030, facet.by='age', data.types='incidence', show.individual.sims = F)

simplot(simset.17.large,years=2000:2030, facet.by='age', data.types='incidence', show.individual.sims = F)
simplot(simset.18.large,years=2000:2030, facet.by='age', data.types='incidence', show.individual.sims = F)
simplot(simset.19.large,years=2000:2030, facet.by='age', data.types='incidence', show.individual.sims = F)

##-------------------------------##

mcmc=mcmc.17
simset = extract.simset(mcmc,
                        additional.burn=500, 
                        additional.thin=100) 

simset.no.int = run.intervention.on.simset(simset,
                                           end.year = RUN.SIMULATIONS.TO.YEAR,
                                           intervention = NO.INTERVENTION)

simset.testing.1 = run.intervention.on.simset(simset,
                                              end.year = RUN.SIMULATIONS.TO.YEAR,
                                              intervention = testing.1)

simset.engagement.1 = run.intervention.on.simset(simset,
                                                 end.year = RUN.SIMULATIONS.TO.YEAR,
                                                 intervention = engagement.1)

simset.disengagement.suppressed.1 = run.intervention.on.simset(simset,
                                                               end.year = RUN.SIMULATIONS.TO.YEAR,
                                                               intervention = disengagement.suppressed.1)

simset.disengagement.unsuppressed.1 = run.intervention.on.simset(simset,
                                                                 end.year = RUN.SIMULATIONS.TO.YEAR,
                                                                 intervention = disengagement.unsuppressed.1)

simset.all.int = run.intervention.on.simset(simset,
                                            end.year = RUN.SIMULATIONS.TO.YEAR,
                                            intervention = all.interventions)

##--------------------##
##----- PLOTTING -----##
##--------------------##
simplot(simset.no.int,simset.all.int, years=1980:2040, data.types=c('awareness',"engagement","suppression"), proportion=T)

simplot(simset.no.int,simset.testing.1, years=2010:2040, data.types=c('awareness',"engagement","suppression"), proportion=T)
simplot(simset.no.int,simset.engagement.1, years=2010:2040, data.types=c('awareness',"engagement","suppression"), proportion=T)

simplot(simset.no.int,simset.disengagement.suppressed.1, years=1980:2040, data.types=c('awareness',"engagement","suppression"), proportion=T)
simplot(simset.no.int,simset.disengagement.unsuppressed.1, years=2010:2040, data.types=c('awareness',"engagement","suppression"), proportion=T)


simplot(simset.no.int,simset.all.int, years=2000:2030, facet.by='age', data.types='incidence')
simplot(simset.no.int,simset.all.int, years=2000:2030, facet.by='age', data.types='incidence', show.individual.sims = F)
simplot(simset.no.int,simset.all.int, years=2000:2030, data.types=c('awareness',"engagement","suppression"), proportion=T, show.individual.sims = F)
simplot(simset.no.int,simset.all.int, years=2000:2030, data.types=c('awareness',"engagement","suppression"), 
        facet.by = c("age","sex"),proportion=T, show.individual.sims = F)


##--------------------##
##----- OLD CODE -----##
##--------------------##
## (RUNNING INDIVIDUAL SIMS FROM STARTING VALUES)

load("calibration/starting_values/starting_values_02-09.Rdata") 

RUN.SIMULATIONS.TO.YEAR = 2040

sim.no.int = run.model.for.parameters(variable.parameters = params.start.values,
                                      end.year = RUN.SIMULATIONS.TO.YEAR,
                                      interventions = NO.INTERVENTION)

sim.testing.1 = run.model.for.parameters(variable.parameters = params.start.values,
                                         end.year = RUN.SIMULATIONS.TO.YEAR,
                                         interventions = testing.1)

sim.engagement.1 = run.model.for.parameters(variable.parameters = params.start.values,
                                            end.year = RUN.SIMULATIONS.TO.YEAR,
                                            interventions = engagement.1)

sim.gain.suppression.1 = run.model.for.parameters(variable.parameters = params.start.values,
                                                  end.year = RUN.SIMULATIONS.TO.YEAR,
                                                  interventions = gain.suppression.1)

sim.disengagement.unsuppressed.1 = run.model.for.parameters(variable.parameters = params.start.values,
                                                  end.year = RUN.SIMULATIONS.TO.YEAR,
                                                  interventions = disengagement.unsuppressed.1)

sim.disengagement.suppressed.1 = run.model.for.parameters(variable.parameters = params.start.values,
                                                            end.year = RUN.SIMULATIONS.TO.YEAR,
                                                            interventions = disengagement.suppressed.1)

sim.all.interventions = run.model.for.parameters(variable.parameters = params.start.values,
                                                 end.year = RUN.SIMULATIONS.TO.YEAR,
                                                 interventions = all.interventions)

sim.all.interventions.female = run.model.for.parameters(variable.parameters = params.start.values,
                                                 end.year = RUN.SIMULATIONS.TO.YEAR,
                                                 interventions = all.interventions.female)

sim.all.interventions.male = run.model.for.parameters(variable.parameters = params.start.values,
                                                        end.year = RUN.SIMULATIONS.TO.YEAR,
                                                        interventions = all.interventions.male)


simplot(sim.no.int,sim.testing.1, years=1980:2030, facet.by=c('age','sex'), data.types='awareness', proportion=T)
simplot(sim.no.int,sim.engagement.1, years=1980:2030, facet.by=c('age','sex'), data.types='engagement', proportion=T)
simplot(sim.no.int,sim.gain.suppression.1, years=1980:2030, facet.by=c('age','sex'), data.types='suppression', proportion=T)

simplot(sim.no.int,sim.all.interventions, years=2010:2040, facet.by='age', data.types='incidence')
simplot(sim.no.int,sim.all.interventions, years=1980:2030, data.types=c('awareness',"engagement","suppression"), proportion=T)

# Checking sex 
simplot(sim.no.int,sim.all.interventions.female, years=1980:2030, data.types=c('awareness',"engagement","suppression"), proportion=T, facet.by = c("age","sex"))
simplot(sim.no.int,sim.all.interventions.male, years=1980:2030, data.types=c('awareness',"engagement","suppression"), proportion=T, facet.by = c("age","sex"))

simplot(sim.no.int,sim.all.interventions.female, years=2010:2030, facet.by=c('age',"sex"), ages = "15+", data.types='incidence')
simplot(sim.no.int,sim.all.interventions.male, years=2010:2030, facet.by=c('age',"sex"), ages = "15+", data.types='incidence')

