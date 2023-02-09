
# load("mcmcruns/mcmc_v12_2023-01-26.Rdata")
# load("mcmcruns/mcmc_v13_2023-01-30.Rdata")

mcmc=mcmc.15
simset = extract.simset(mcmc,
                           additional.burn=500, 
                           additional.thin=17) 

acceptance.plot(mcmc)
acceptance.plot(mcmc, by.block = T, window.iterations = 200)

trace.plot(mcmc, 'age')
trace.plot(mcmc, 'age', additional.burn = 500)

get.rhats(mcmc)
trace.plot(mcmc, 'female.to.male.m')

trace.plot(mcmc, 'trate')
trace.plot(mcmc,"*transmission")

trace.plot(mcmc,"*testing") 
trace.plot(mcmc,"*engagement") 
trace.plot(mcmc,"*suppression") 

trace.plot(mcmc, '*hiv.mortality')
trace.plot(mcmc, '*male.hiv.mortality')
trace.plot(mcmc, '*hiv.specific.mortality')

# simset = extract.simset(mcmc, additional.burn=500, additional.thin=20)

source("model/run_systematic.R")
simplot(simset)
simplot(simset, years=1980:2020)
simplot(simset, years=1980:2020, facet.by='age', data.types='incidence')
simplot(simset, years=1980:2020, data.types='prevalence')
simplot(simset, years=1980:2020, facet.by='age', data.types='prevalence')
simplot(simset, years=1980:2020, facet.by=c('age',"sex"), ages = "15+", data.types='prevalence')
simplot(simset, years=1980:2020, facet.by=c('age',"sex"), ages = "15+", data.types='incidence')
simplot(simset, years=1980:2020, facet.by='age', data.types='hiv.mortality',proportion = T)
simplot(simset, years=1980:2020,  data.types=c('awareness',"engagement","suppression"), proportion=T)
simplot(simset, years=1980:2020, facet.by=c('age','sex'), data.types='awareness', proportion=T)
simplot(simset, years=1980:2020, facet.by=c('age','sex'), data.types='engagement', proportion=T)
simplot(simset, years=1980:2020, facet.by=c('age','sex'), data.types='suppression', proportion=T)

simplot(simset, years=1980:2020, facet.by='age', data.types='population')

sim.from.mcmc = simset@simulations[[simset@n.sim]]
params.from.mcmc = simset@parameters[simset@n.sim,]

params.test = params.from.mcmc
params.test["log.OR.testing.intercept"] = 1 # 5.481735628 --> CHANGING THIS (to 1) DRASTICALLY MESSES UP INCIDENCE (see changes to trates below) 
# params.start.values has this intercept at 1.72 -> maybe make this tighter? 
params.test["log.OR.testing.slope"] = 0.03 # 0.055520292 

params.test["log.OR.engagement.post.universal.slope"] = 0.18 # 0.282862234

# params.test["suppression.rate.0"] = # 0.375661205, 1993
params.test["suppression.rate.1"] = 1 # 1.535894482, 2003

params.test["male.awareness.multiplier"] = 0.29 # 0.309544560
params.test["male.engagement.multiplier"] = 0.5 # 0.249105968
params.test["male.suppression.multiplier"] = 1 # 0.737445121

# these are actually closer to the starting values (params.start.values) - maybe I need to make those tighter?
params.test["trate.0"] = 0.46 # 0.521859587
params.test["trate.1"] = 0.07 # 0.080078307
params.test["trate.2"] = 0.1 # 0.250371394
params.test["trate.3"] = 0.15 # 0.386591384

sim.test = run.model.for.parameters(variable.parameters = params.test)

simplot(sim.from.mcmc,sim.test, years=1980:2020, facet.by='age', data.types='incidence')
simplot(sim.from.mcmc,sim.test, years=1980:2020, data.types='incidence')

simplot(sim.from.mcmc,sim.test, years=1980:2020, data.types=c('awareness',"engagement","suppression"), proportion=T)
simplot(sim.from.mcmc,sim.test, years=1980:2020, data.types=c('awareness',"engagement","suppression"), 
        facet.by = c("age","sex"),proportion=T)


simplot(sim.from.mcmc,sim.test, years=1980:2020, facet.by='age', data.types='prevalence')
simplot(sim.from.mcmc,sim.test, years=1980:2020, facet.by='age', data.types='hiv.mortality')

# Check likelihood 
lik = create.likelihood(parameters=sim.from.mcmc$parameters) 
lik.components = attr(lik,"components")

round(sapply(lik.components,function(sub.lik){exp(sub.lik(sim.test) - sub.lik(sim.from.mcmc))}),2) 
exp(lik(sim.test)-lik(sim.from.mcmc))
