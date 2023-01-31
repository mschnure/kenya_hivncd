load("calibration/debug.parameters.Rdata")

params.error.test = parameters
variable.params.error.test = variable.parameters

sim.error.test = run.model.for.parameters(variable.parameters = variable.params.error.test)


simplot(sim.error.test, years=1980:2020, facet.by='age', data.types='incidence')
simplot(sim.error.test, years=1980:2020, data.types='incidence')

simplot(sim.error.test, years=1980:2020, data.types=c('awareness',"engagement","suppression"), proportion=T)
simplot(sim.error.test, years=1980:2020, data.types=c('awareness',"engagement","suppression"), 
        facet.by = c("age","sex"),proportion=T)


simplot(sim.error.test, years=1980:2020, facet.by='age', data.types='prevalence')
simplot(sim.error.test, years=1980:2020, facet.by='age', data.types='hiv.mortality',proportion = T)
simplot(sim.error.test, years=1980:2020, facet.by='age', data.types='population')

apply(sim.error.test$population,1,sum)
apply(sim.error.test$incidence,1,sum)

