source("model/run_systematic.R")
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

sim.bad.suppression.female = run.model.for.parameters(variable.parameters = params.start.values,
                                                        end.year = RUN.SIMULATIONS.TO.YEAR,
                                                        interventions = suppression.female.bad)



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

simplot(sim.no.int,sim.bad.suppression.female, years=1980:2030, data.types=c("suppression"),proportion=T, facet.by = c("age","sex"))
simplot(sim.no.int,sim.bad.suppression.female, years=2010:2030, facet.by=c('age',"sex"), ages = "15+", data.types='incidence')

