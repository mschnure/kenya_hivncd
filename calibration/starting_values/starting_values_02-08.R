
# load 2/3 MCMC run 
load("mcmcruns/mcmc2023-02-03.Rdata")

# Take final run 
simset.14 = extract.simset(mcmc.14,
                          additional.burn=1000, 
                          additional.thin=10) 
sim.14 = simset.14@simulations[[simset.14@n.sim]]
params.14 = simset.14@parameters[simset.14@n.sim,]

params.test = params.14

params.test["trate.0"] = 0.545 # 0.523807463; 1990
params.test["trate.1"] = 0.09 # 0.081290762; 1997
params.test["trate.2"] = 0.14 # 0.101781471; 2008
params.test["trate.3"] = 0.13 # 0.126064458; 2015

params.test["age.15.to.19.transmission.multiplier.0"] = 1.1 # 1.022102733
params.test["age.15.to.19.transmission.multiplier.1"] = 0.5 # 0.547173113
params.test["age.15.to.19.transmission.multiplier.2"] = 0.5 # 1.126104749
params.test["age.15.to.19.transmission.multiplier.3"] =0.6 # 0.733753156

params.test["age.50.and.over.transmission.multiplier.0"] = 0.3 # 0.862687945
params.test["age.50.and.over.transmission.multiplier.1"] = 0.6 # 0.775928124
params.test["age.50.and.over.transmission.multiplier.2"] = 0.5 # 0.725242460
params.test["age.50.and.over.transmission.multiplier.3"] = 0.3 # 0.265565812

params.test["birth.transmission.risk.0"] =0.6 # 0.331488684; 1990
params.test["birth.transmission.risk.1"] = 0.4 # 0.398622327; 2020

params.test["hiv.specific.mortality.rates.0"] = 0.01 # 0.004370032; 1990
params.test["hiv.specific.mortality.rates.1"] = 0.10 # 0.090770099; 2005 
params.test["hiv.specific.mortality.rates.2"] # 0.050391405; 2020

params.test["age.0.to.14.hiv.mortality.multiplier.0"] = 10 # 0.651495861
params.test["age.0.to.14.hiv.mortality.multiplier.1"] = 1 # 0.481552078
params.test["age.0.to.14.hiv.mortality.multiplier.2"] = 1 # 0.242421903
params.test["age.15.to.24.hiv.mortality.multiplier.0"] = 1 # 0.375939411
params.test["age.15.to.24.hiv.mortality.multiplier.1"] = 1.5 # 0.583800292
params.test["age.15.to.24.hiv.mortality.multiplier.2"] # 0.829877318
params.test["over.50.hiv.mortality.multiplier.0"] = 5 # 1.645051986
params.test["over.50.hiv.mortality.multiplier.1"] = 4 # 0.235593311
params.test["over.50.hiv.mortality.multiplier.2"] = 1 # 0.957674597


sim.test = run.model.for.parameters(variable.parameters = params.test)

simplot(sim.14,sim.test, years=1980:2020, facet.by='age', data.types='incidence')
simplot(sim.14,sim.test, years=1980:2020, facet.by='age', data.types='prevalence')
simplot(sim.14,sim.test, years=1980:2020, facet.by='age', data.types='hiv.mortality',proportion = T)

simplot(sim.14,sim.test, years=1980:2020, data.types='prevalence')
simplot(sim.14,sim.test, years=1980:2020, facet.by=c('age',"sex"), ages = "15+", data.types='prevalence')
simplot(sim.14,sim.test, years=1980:2020, facet.by=c('age',"sex"), ages = "15+", data.types='incidence')

simplot(sim.14,sim.test, years=1980:2020, data.types=c('awareness',"engagement","suppression"), proportion=T)
simplot(sim.14,sim.test, years=1980:2020, facet.by=c('age','sex'), data.types='awareness', proportion=T)
simplot(sim.14,sim.test, years=1980:2020, facet.by=c('age','sex'), data.types='engagement', proportion=T)
simplot(sim.14,sim.test, years=1980:2020, facet.by=c('age','sex'), data.types='suppression', proportion=T)

simplot(sim.14,sim.test, years=1980:2020, facet.by='age', data.types='population')

params.start.values = params.test

save(params.start.values,file=("calibration/starting_values/starting_values_02-08.Rdata"))
