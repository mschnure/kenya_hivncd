
load("mcmcruns/mcmc_v10_2023-01-17.Rdata")
acceptance.plot(mcmc)
acceptance.plot(mcmc, by.block = T, window.iterations = 200)

trace.plot(mcmc, 'age')
trace.plot(mcmc, 'age', additional.burn = 500)

get.rhats(mcmc)
trace.plot(mcmc, 'female.to.male.m')

trace.plot(mcmc, 'trate')


simset = extract.simset(mcmc, additional.burn=500, additional.thin=20)

source("model/run_systematic.R")
simplot(simset)
simplot(simset, years=1980:2020)
simplot(simset, years=1980:2020, facet.by='age', data.types='incidence')
simplot(simset, years=1980:2020, facet.by='age', data.types='prevalence')
simplot(simset, years=1980:2020, facet.by='age', data.types='hiv.mortality')
simplot(simset, years=1980:2020,  data.types=c('awareness',"engagement","suppression"), proportion=T)
simplot(simset, years=1980:2020, facet.by=c('age','sex'), data.types='awareness', proportion=T)
simplot(simset, years=1980:2020, facet.by=c('age','sex'), data.types='engagement', proportion=T)
simplot(simset, years=1980:2020, facet.by=c('age','sex'), data.types='suppression', proportion=T)

sim.from.mcmc = simset@simulations[[simset@n.sim]]
params.from.mcmc = simset@parameters[simset@n.sim,]

params.test = params.from.mcmc
params.test["log.OR.testing.intercept"] = 1 # 5.481735628 --> CHANGING THIS DRASTICALLY MESSES UP INCIDENCE
params.test["log.OR.testing.slope"] = 0.01 # 0.055520292

# params.test["suppression.rate.0"] = # 0.375661205, 1993
params.test["suppression.rate.1"] = 1.1 # 1.535894482, 2003

params.test["male.engagement.multiplier"] = 0.5 # 0.249105968
params.test["male.suppression.multiplier"] = 1 # 0.737445121

sim.test = run.model.for.parameters(variable.parameters = params.test)

simplot(sim.from.mcmc,sim.test, years=1980:2020, data.types=c('awareness',"engagement","suppression"), proportion=T)
simplot(sim.from.mcmc,sim.test, years=1980:2020, data.types=c('awareness',"engagement","suppression"), 
        facet.by = c("age","sex"),proportion=T)


simplot(sim.from.mcmc,sim.test, years=1980:2020, facet.by='age', data.types='incidence')
simplot(sim.from.mcmc,sim.test, years=1980:2020, facet.by='age', data.types='prevalence')
simplot(sim.from.mcmc,sim.test, years=1980:2020, facet.by='age', data.types='hiv.mortality')

# Check likelihood 
lik = create.likelihood(parameters=sim.from.mcmc$parameters) 
lik.components = attr(lik,"components")

round(sapply(lik.components,function(sub.lik){exp(sub.lik(sim.test) - sub.lik(sim.from.mcmc))}),2) 

