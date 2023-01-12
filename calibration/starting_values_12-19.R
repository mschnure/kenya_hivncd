
# MCMC4 - starts with MCMC2's final values; revised params in starting_values_12-10; then rerun with all years equally weighted
load("mcmcruns/mcmc2022-12-19 08:12:35.Rdata")

simset = extract.simset(mcmc.4,
                        additional.burn=1000, 
                        additional.thin=10) 

params.0 = simset@parameters[simset@n.sim,] 
sim.0 = simset@simulations[[simset@n.sim]] 

params.test = params.0

# Remove old parameters
params.test = params.test[names(params.test)!="age.15.to.24.hiv.mortality.multiplier"]
params.test = params.test[names(params.test)!="over.50.hiv.mortality.multiplier"]

# Add in new parameters I just added
params.test["age.0.to.14.hiv.mortality.multiplier.1"] = 1
params.test["age.0.to.14.hiv.mortality.multiplier.2"] = 1
params.test["age.15.to.24.hiv.mortality.multiplier.0"] = 1
params.test["age.15.to.24.hiv.mortality.multiplier.1"] = 1
params.test["age.15.to.24.hiv.mortality.multiplier.2"] = 1
params.test["over.50.hiv.mortality.multiplier.0"] = 1
params.test["over.50.hiv.mortality.multiplier.1"] = 1
params.test["over.50.hiv.mortality.multiplier.2"] = 1

# sim.test = run.model.for.parameters(variable.parameters = params.test)

params.test["trate.0"] = 0.5 # 0.57158055
params.test["trate.1"] = 0.035 # 0.01626660
params.test["trate.2"] = 0.06 # 0.08482393
params.test["trate.3"] = 0.185 # 0.18148736

params.test["age.15.to.19.transmission.multiplier"] = 1.2 # 0.76827169
params.test["age.20.to.29.transmission.multiplier"] = 1.17 # 1.16384757
# 
params.test["age.50.and.over.transmission.multiplier.0"] = 0.8 # 0.35712273
params.test["age.50.and.over.transmission.multiplier.1"] = 2.5 # 0.29879870
params.test["age.50.and.over.transmission.multiplier.2"] = 1.05 # 0.36962025
params.test["age.50.and.over.transmission.multiplier.3"] = .6 # 0.32755630


params.test["birth.transmission.risk.0"] = 0.75 # 0.52350496
params.test["birth.transmission.risk.1"] = 0.35 # 0.25963790
params.test["hiv.specific.mortality.rates.0"] = 0.01 # 0.01999578; 1990
params.test["hiv.specific.mortality.rates.1"] = 0.055 # 0.06124771; 2005
params.test["hiv.specific.mortality.rates.2"] = 0.05 # 0.04101335; 2020

params.test["age.0.to.14.hiv.mortality.multiplier.0"] = 20 # 11.20975227
params.test["age.0.to.14.hiv.mortality.multiplier.1"] = 2 # 11.20975227
params.test["age.0.to.14.hiv.mortality.multiplier.2"] = 2 # 11.20975227
params.test["age.15.to.24.hiv.mortality.multiplier.1"] = 1.5 # 0.46777312
params.test["age.15.to.24.hiv.mortality.multiplier.2"] = 1 # 0.46777312
params.test["over.50.hiv.mortality.multiplier.0"] = 3 # 2.24540286
params.test["over.50.hiv.mortality.multiplier.1"] = 2.3 # 2.24540286
params.test["over.50.hiv.mortality.multiplier.2"] = 0.75 # 2.24540286

## plotting comparisons/checking likelihood 
if(1==2){
    sim.test = run.model.for.parameters(variable.parameters = params.test)
    simplot(sim.0,sim.test,data.types = c("incidence"),facet.by = "age",years = 1980:2020)
    simplot(sim.0,sim.test,data.types = c("incidence"),years = 1980:2020)
    simplot(sim.0,sim.test,data.types = c("awareness","engagement","suppression"),proportion=T)
    simplot(sim.0,sim.test,data.types = c("awareness","engagement","suppression"),proportion=T,facet.by = c("age","sex"))
    simplot(sim.0,sim.test,data.types = c("awareness","engagement","suppression"),proportion=T,years = 1980:2020)
    simplot(sim.0,sim.test,data.types = c("population"),facet.by="age",years=1980:2020)
    
    simplot(sim.0,sim.test,data.types = c("hiv.mortality"),facet.by="age",proportion = F,years=1980:2020) 
    simplot(sim.0,sim.test,data.types = c("hiv.mortality"),facet.by="age",proportion = T,years=1980:2020) 
    
    
    simplot(sim.0,sim.test,data.types = c("prevalence"),facet.by = "age",years = 1980:2020)
    
    # Check likelihood 
    lik = create.likelihood(parameters=sim.test$parameters) # run once
    lik.components = attr(lik,"components")
    
    lik(sim.test)
    round(sapply(lik.components,function(sub.lik){exp(sub.lik(sim.test) - sub.lik(sim.0))}),2) 
    
    round(exp(lik(sim.test) - lik(sim.0)),2) 
    
    
    # plotting trates
    qplot(c(1975,1990,1997,2008,2015),c(params.test[paste0("trate.",0:3)]), geom = "line",ylim=c(0,NA))
    desired.years = 1980:2030
    knot.times = c(1975,1990,1997,2008,2015)
    knot.values.for.i = c(params.test["trate.0"],params.test[paste0("trate.",0:3)])
    smooth.trates = exp(spline(knot.times, log(knot.values.for.i), xout = desired.years, method='natural')$y) # fit spline on log scale
    qplot(desired.years,smooth.trates,geom="line") + ylim(0,NA) + geom_vline(xintercept=knot.times)
}


# SAVE THIS SET OF STARTING VALUES TO START OTHER RUNS WITH
params.start.values = params.test
save(params.start.values,file=("calibration/starting_values_12-19.Rdata"))

