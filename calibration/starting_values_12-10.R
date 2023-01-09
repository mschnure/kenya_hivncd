
# MCMC2 - revised values from this below to get starting values for MCMC3 and MCMC4
load("mcmcruns/mcmc2022-12-08 18:35:57.Rdata")

# Other runs
if(1==2){
    # MCMC3 - starting values from below; years after 2010 most weighted
    load("~/Dropbox/Documents_local/Hopkins/PhD/Dissertation/ABM/kenya_hivncd/mcmcruns/mcmc2022-12-12 22:01:19.Rdata")
    
    # MCMC4 - starting values from below; all years equally weighted
    load("~/Dropbox/Documents_local/Hopkins/PhD/Dissertation/ABM/kenya_hivncd/mcmcruns/mcmc2022-12-19 08:12:35.Rdata")
}

simset = extract.simset(mcmc.2,
                        additional.burn=1000, 
                        additional.thin=10) 

params.0 = simset@parameters[simset@n.sim,] 
sim.0 = simset@simulations[[simset@n.sim]] 

params.test = params.0


## Testing out new parameters values to start next run with (MCMC3 and MCMC4) ##
params.test["log.OR.testing.intercept"]=1 # 2.69
params.test["log.OR.testing.slope"]=-0.06 # -0.028

params.test["log.OR.engagement.intercept"] = 0.25 # 0.53

params.test["suppression.rate.0"] = 1 # 1.21; in 1993
params.test["suppression.rate.1"] = 2.3 # 1.66; in 2003

params.test["male.cascade.multiplier"] = 0.45 # 0.31

params.test["age.45.to.65.mortality.intercept.multiplier"] = 5 # 61.7!!
params.test["age.45.to.65.mortality.slope.multiplier"] = 0.98 # 0.95
params.test["over.65.mortality.intercept.multiplier"] = 0.85 # 0.26
params.test["over.65.mortality.slope.multiplier"] = 1 # 0.95

params.test["trate.0"] = 0.57 # 0.52953092
params.test["trate.1"] = 0.056 # 0.02743001
params.test["trate.2"] = 0.05# 0.097
params.test["trate.3"] = 0.19 # 0.25

params.test["age.15.to.19.transmission.multiplier"] = .78 # 0.633
params.test["age.20.to.29.transmission.multiplier"] = 1 # 1.09616054

params.test["age.50.and.over.transmission.multiplier.0"] = .45 # 1.39
params.test["age.50.and.over.transmission.multiplier.1"] = 1.6 # 0.44
params.test["age.50.and.over.transmission.multiplier.2"] = .7 # 1.71
params.test["age.50.and.over.transmission.multiplier.3"] = .4 # 0.41


# Changing HIV mortality and birth transmission risk to try to hit prevalence in younger age groups 
params.test["birth.transmission.risk.0"] = 0.85 # 0.3987
params.test["birth.transmission.risk.1"] = 0.32 # 0.34077092
params.test["hiv.specific.mortality.rates.0"] = 0.017 # 0.01634703; 1990
params.test["hiv.specific.mortality.rates.1"] = 0.09 # 0.05580461; 2005
params.test["hiv.specific.mortality.rates.2"] = 0.055 # 0.05848823; 2020

params.test["age.0.to.14.hiv.mortality.multiplier.0"] = 20 # 0.61038964
params.test["age.15.to.24.hiv.mortality.multiplier"] = 0.2 # 0.36652278
params.test["over.50.hiv.mortality.multiplier"] = 1 # 1.27536146


# SAVE THIS SET OF STARTING VALUES TO START OTHER RUNS WITH
params.start.values = params.test
save(params.start.values,file=("calibration/starting_values_12-10.Rdata"))


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


