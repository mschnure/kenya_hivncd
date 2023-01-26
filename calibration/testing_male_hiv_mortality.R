source("model/run_systematic.R")
load("mcmcruns/mcmc_v12_2023-01-26.Rdata")
# load("calibration/starting_values_01-26.Rdata")

mcmc=mcmc.12
simset = extract.simset(mcmc, additional.burn=500, additional.thin=20)

params.from.mcmc = simset@parameters[simset@n.sim,]
sim.from.mcmc = run.model.for.parameters(variable.parameters = params.from.mcmc)

params.test = params.from.mcmc
params.test["male.hiv.mortality.multiplier.0"] = 3.5 
params.test["male.hiv.mortality.multiplier.1"] = 1.5 
params.test["male.hiv.mortality.multiplier.2"] = 1 
params.test["female.to.male.multiplier"] = 1.11  # 0.985825292
sim.test = run.model.for.parameters(variable.parameters = params.test)

simplot(sim.from.mcmc,sim.test, years=1980:2020, data.types='incidence')
simplot(sim.from.mcmc,sim.test, years=1980:2020, facet.by='age', data.types='incidence')
simplot(sim.from.mcmc,sim.test, years=1980:2020, facet.by=c('age',"sex"), ages = "15+", data.types='incidence')

simplot(sim.from.mcmc,sim.test, years=1980:2020, data.types='prevalence')
simplot(sim.from.mcmc,sim.test, years=1980:2020, facet.by='age', data.types='prevalence')
simplot(sim.from.mcmc,sim.test, years=1980:2020, facet.by=c('age',"sex"), ages = "15+", data.types='prevalence')

simplot(sim.from.mcmc,sim.test, years=1980:2020, data.types=c('awareness',"engagement","suppression"), proportion=T)
simplot(sim.from.mcmc,sim.test, years=1980:2020, data.types=c('awareness',"engagement","suppression"), 
        facet.by = c("age","sex"),proportion=T)

simplot(sim.from.mcmc,sim.test, years=1980:2020, facet.by='age', data.types='hiv.mortality', proportion = T)

