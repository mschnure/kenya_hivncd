source("model/run_systematic.R")
load("calibration/starting_values/starting_values_02-08.Rdata")

RUN.SIMULATIONS.TO.YEAR = 2040

sim.no.int = run.model.for.parameters(variable.parameters = params.start.values,
                                      end.year = RUN.SIMULATIONS.TO.YEAR,
                                      intervention = NO.INTERVENTION)

sim.testing.1 = run.model.for.parameters(variable.parameters = params.start.values,
                                         end.year = RUN.SIMULATIONS.TO.YEAR,
                                         intervention = testing.1)

sim.engagement.1 = run.model.for.parameters(variable.parameters = params.start.values,
                                            end.year = RUN.SIMULATIONS.TO.YEAR,
                                            intervention = engagement.1)

sim.gain.suppression.1 = run.model.for.parameters(variable.parameters = params.start.values,
                                                  end.year = RUN.SIMULATIONS.TO.YEAR,
                                                  intervention = gain.suppression.1)

sim.all.interventions = run.model.for.parameters(variable.parameters = params.start.values,
                                                 end.year = RUN.SIMULATIONS.TO.YEAR,
                                                 intervention = all.interventions)

simplot(sim.base, sim.no.int, years=1980:2020, facet.by='age', data.types='incidence')
simplot(sim.base, years=1980:2020, facet.by='age', data.types='prevalence')

simplot(sim.base,sim.testing.1, years=1980:2030, facet.by=c('age','sex'), data.types='awareness', proportion=T)
simplot(sim.base,sim.engagement.1, years=1980:2030, facet.by=c('age','sex'), data.types='engagement', proportion=T)
simplot(sim.base,sim.gain.suppression.1, years=1980:2030, facet.by=c('age','sex'), data.types='suppression', proportion=T)


simplot(sim.base,sim.all.interventions, years=1980:2030, data.types=c('awareness',"engagement","suppression"), proportion=T)
simplot(sim.base,sim.all.interventions, years=1980:2030, data.types=c('awareness',"engagement","suppression"), 
        proportion=T, facet.by = c("age","sex"))

simplot(sim.base,sim.all.interventions, years=2010:2030, facet.by='age', data.types='incidence')



