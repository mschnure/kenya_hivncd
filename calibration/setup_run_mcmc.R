# library(devtools)
# install_github("tfojo1/bayesian.simulations")
library(bayesian.simulations)
library(distributions)
library(ggplot2) 

source("model/run_systematic.R")
# source("calibration/starting_values_12-10.R")
# load("calibration/starting_values_12-10.Rdata") - used these for mcmc.3 and mcmc.4
# load("calibration/starting_values_12-19.Rdata") - used these for mcmc.5 and mcmc.6
# load("calibration/starting_values_01-05.Rdata") # new starting values with aging rates added in - used these for mcmc.7 and mcmc.8
load("calibration/starting_values_01-09.Rdata")

set.seed(1234) 

# mcmc.1 (12/06) - (seed: 2020)
# mcmc.2 (12/08) - (seed: 2020)
# mcmc.3 (12/12) - (seed: 1234); run with 12/10 starting values
# mcmc.4 (12/19) - (seed: 2020); run with 12/10 starting values, all years equally weighted 

# include all spline points for hiv mortality multipliers by age: 
# mcmc.5 (12/20) - (seed: 1234); run with 12/19 starting values (splined hiv mortality multipliers by age), all years equally weighted 
# mcmc.6 (12/29) - (seed: 1234); run with 12/19 starting values, all years equally weighted, downweight hiv mortality likelihood (1/100000) 
# mcmc.7 (1/5) - (seed: 1234); run with 1/5 starting values, all years equally weighted, new aging rates (returned hiv mortality to original weight)
# mcmc.8 (1/6) - (seed: 1234); run with 1/5 starting values, all years equally weighted, new aging rates, hiv mortality downweighted to 1/100000

# include new splined age transmission multipliers and separate cascade multipliers: 
# mcmc.9 (1/11) - (seed: 1234); run with 1/9 starting values, hiv mortality downweighted to 1/16

# run.mcmc.from.cache() - to resume running if I stop (need the cache directory)

BASE.PARAMETERS=create.model.parameters()

simulation.function = function(sampled.parameters){
    run.model.for.parameters(variable.parameters = sampled.parameters,
                             parameters = BASE.PARAMETERS)
}

likelihood = create.likelihood(parameters = BASE.PARAMETERS)

transformations = sapply(prior@subdistributions,function(dist){
    
    if(is.null(dist@transformation))
        "identity"
    else
        dist@transformation@name
    
})

sds = get.sds(prior)

# Main things: prior, likelihood, function that maps
# Additional things: blocks to sample in, proposal distribution for values to sample - needs a covariance matrix for MVN distribution
control = create.adaptive.blockwise.metropolis.control(var.names = prior@var.names,
                                             simulation.function = simulation.function,
                                             log.prior.distribution = get.density.function(prior,default.log = T),
                                             log.likelihood = likelihood,
                                             var.blocks = parameter.var.blocks,
                                             transformations = transformations,
                                             initial.covariance.mat = diag((sds/20)^2), # step size
                                             burn = 0,
                                             thin = 5) 

# set starting.values 
mcmc.9 = run.mcmc.with.cache(control = control,
                           n.iter = 10000,
                           starting.values = params.start.values, 
                           update.frequency = 5,
                           cache.frequency = 500,
                           cache.dir = "mcmc_cache"
                           )

# run.mcmc.from.cache(dir = "mcmc_cache/")

save(mcmc.9,file=paste0("mcmcruns/mcmc",Sys.time(),".Rdata"))


if(1==2)
{
    mcmc=mcmc.9
    
    simset = extract.simset(mcmc,
                            additional.burn=1000, # throw away first 1000
                            additional.thin=10) # thin by 10, 100 remaining
    
    # prior@var.names
    # parameter.var.blocks
    
    ## FIRST, LOOK AT OVERALL FIT (don't look at other plots until I look at mixing/MCMC properties)
    simplot(simset, years = 1980:2020)

    ## MCMC PROPERTIES ##
    
    ## ACCEPTANCE RATE
    # want it to be around 25%; window says how long to look back (would be thrown off by first iterations)
    acceptance.plot(mcmc,window.iterations = 200)
    
    # checking by the variable blocks; don't want to see one chain dragged down to 0 (not mixing on that parameter)
    acceptance.plot(mcmc,window.iterations = 200,by.block = T,aggregate.chains = T) 
    
    ## TRACE PLOTS
    # will eventually look at trace plots for multiple chains (want all chains to reach the same value; homogenous mix)
    # want to see mixing that eventually gets to a steady state (don't want to see a trend - hasn't reached steady state yet)
    # also don't want to see jumping and getting stuck in one place 
    # if it's getting dragged, there should be data justifying this drag
    
    # matches parameters with string, use * if the parameter doesn't start with that string
    trace.plot(mcmc,"age.") 
    trace.plot(mcmc,"age.",additional.burn = 1000)
    trace.plot(mcmc,"trate") 
    trace.plot(mcmc,"trate",additional.burn = 1000) 
    trace.plot(mcmc,"*transmission")
    trace.plot(mcmc,"female") 
    trace.plot(mcmc,"*engagement") 
    trace.plot(mcmc,"*mortality") 
    trace.plot(mcmc,"birth")
    trace.plot(mcmc,"*testing")
    trace.plot(mcmc,"*suppression") 
    trace.plot(mcmc,"*male") 
    trace.plot(mcmc,"*fertility")
    trace.plot(mcmc,"*aging")
    trace.plot(mcmc,"*hiv.mortality.multiplier")
    
    
    
    ## NOW BACK TO OTHER PLOTS/FITS
    simplot(simset,data.types = c("awareness","engagement","suppression"),proportion = T,years=1980:2020)
    simplot(simset,data.types = c("population"),facet.by = "age",years=1980:2020) 
    simplot(simset,data.types = c("incidence"),facet.by = "age",years=1980:2020) 
    simplot(simset,data.types = c("incidence"),ages = "15+",facet.by = c("age","sex"),years=1980:2020) 
    simplot(simset,data.types = c("prevalence"),facet.by = "age",years=1980:2020) 
    simplot(simset,data.types = c("hiv.mortality"),facet.by="age",proportion = T,years=1980:2020) 
    simplot(simset,data.types = c("awareness","engagement","suppression"),proportion=T,facet.by = c("age","sex"))
    
    # Check last run of mcmc so I can change parameters manually 
    params.0 = simset@parameters[simset@n.sim,] # get the last set of values for parameters
    sim.0 = simset@simulations[[simset@n.sim]] # last simulation
    simplot(sim.0,data.types = c("incidence"),facet.by = "age",years = 1980:2020)
    simplot(sim.0,data.types = c("prevalence"),facet.by = "age",years = 1980:2020)
    simplot(sim.0,data.types = c("awareness","engagement","suppression"),proportion=T)
    simplot(sim.0,data.types = c("hiv.mortality"),facet.by="age",proportion = F,years=1980:2020) 
    simplot(sim.0,data.types = c("hiv.mortality"),facet.by="age",proportion = T,years=1980:2020)  # fixed this plot finally 
    
    simplot(sim.0,data.types = c("population"),facet.by = "age",years=1980:2020) 
    
    
    # comparing mcmc runs (or comparing manually run sim (sim.test) - source starting_values code)
    sim.test = run.model.for.parameters(variable.parameters = params.test)
    
    simset.2 = extract.simset(mcmc.2,
                            additional.burn=1000, 
                            additional.thin=10) 
    sim.2 = simset.2@simulations[[simset@n.sim]]
    
    simset.3 = extract.simset(mcmc.3,
                              additional.burn=1000, 
                              additional.thin=10) 
    sim.3 = simset.3@simulations[[simset@n.sim]]
    
    
    simset.4 = extract.simset(mcmc.4,
                              additional.burn=1000, 
                              additional.thin=10) 
    sim.4 = simset.4@simulations[[simset@n.sim]]
    
    simset.5 = extract.simset(mcmc.5,
                              additional.burn=1000, 
                              additional.thin=10) 
    sim.5 = simset.5@simulations[[simset@n.sim]]
    
    simset.6 = extract.simset(mcmc.6,
                              additional.burn=1000, 
                              additional.thin=10) 
    sim.6 = simset.6@simulations[[simset@n.sim]]
    
    simset.7 = extract.simset(mcmc.7,
                              additional.burn=1000, 
                              additional.thin=10) 
    sim.7 = simset.7@simulations[[simset@n.sim]]
    
    simset.8 = extract.simset(mcmc.8,
                              additional.burn=1000, 
                              additional.thin=10) 
    sim.8 = simset.8@simulations[[simset@n.sim]]
    
    simset.9 = extract.simset(mcmc.9,
                              additional.burn=1000, 
                              additional.thin=10) 
    sim.9 = simset.9@simulations[[simset@n.sim]]

    
    simplot(sim.4,sim.5,sim.6,data.types = c("incidence"),facet.by = "age",years = 1980:2020)
    simplot(sim.4,sim.5,sim.6,data.types = c("prevalence"),facet.by = "age",years = 1980:2020)
    simplot(sim.4,sim.5,sim.6,data.types = c("hiv.mortality"),facet.by = "age",proportion = T,years = 1980:2020)
    simplot(sim.4,sim.5,sim.6,data.types = c("awareness","engagement","suppression"),proportion=T,facet.by = c("age","sex"))
    simplot(sim.4,sim.5,sim.6,data.types = c("population"),facet.by="age",years=1980:2020)
    
    simplot(sim.5,sim.7,data.types = c("incidence"),facet.by = "age",years = 1980:2020)
    simplot(sim.5,sim.7,data.types = c("prevalence"),facet.by = "age",years = 1980:2020)
    simplot(sim.5,sim.7,data.types = c("hiv.mortality"),facet.by = "age",proportion = T,years = 1980:2020)
    simplot(sim.5,sim.7,data.types = c("awareness","engagement","suppression"),proportion=T,facet.by = c("age","sex"))
    simplot(sim.5,sim.7,data.types = c("population"),facet.by="age",years=1980:2020)
    
    simplot(sim.7,sim.8,data.types = c("incidence"),facet.by = "age",years = 1980:2020)
    simplot(sim.7,sim.8,data.types = c("prevalence"),facet.by = "age",years = 1980:2020)
    simplot(sim.7,sim.8,data.types = c("hiv.mortality"),facet.by = "age",proportion = T,years = 1980:2020)
    simplot(sim.7,sim.8,data.types = c("awareness","engagement","suppression"),proportion=T,facet.by = c("age","sex"))
    simplot(sim.7,sim.8,data.types = c("population"),facet.by="age",years=1980:2020)
    
    simplot(sim.8,sim.9,data.types = c("incidence"),years = 1980:2020)
    simplot(sim.8,sim.9,data.types = c("incidence"),facet.by = "age",years = 1980:2020)
    simplot(sim.8,sim.9,data.types = c("prevalence"),facet.by = "age",years = 1980:2020)
    simplot(sim.8,sim.9,data.types = c("hiv.mortality"),facet.by = "age",proportion = T,years = 1980:2020)
    simplot(sim.8,sim.9,data.types = c("awareness","engagement","suppression"),proportion=T,facet.by = c("age","sex"))
    simplot(sim.8,sim.9,data.types = c("awareness","engagement","suppression"),proportion=T)
    simplot(sim.8,sim.9,data.types = c("population"),facet.by="age",years=1980:2020)
    
    simplot(sim.0,sim.test,data.types = c("incidence"),years = 1980:2020)
    simplot(sim.0,sim.4,data.types = c("awareness","engagement","suppression"),proportion=T,years=1980:2020)
    simplot(sim.test,sim.4,data.types = c("awareness","engagement","suppression"),proportion=T,facet.by = c("age","sex"))
    simplot(sim.test,sim.4,data.types = c("awareness","engagement","suppression"),proportion=T,years = 1980:2020)
    simplot(sim.test,sim.4,data.types = c("population"),facet.by="age",years=1980:2020)
    
    simplot(sim.test,sim.4,data.types = c("hiv.mortality"),facet.by="age",proportion = F,years=1980:2020) 
    simplot(sim.0,sim.test,data.types = c("hiv.mortality"),facet.by="age",proportion = T,years=1980:2020) 
    
    simplot(sim.0,sim.4,data.types = c("hiv.mortality"),proportion = T,years=1980:2020) 

    
    
    # Check likelihood 
    lik = create.likelihood(parameters=sim.9$parameters) 
    lik.components = attr(lik,"components")
    
    lik(sim.test)
    round(sapply(lik.components,function(sub.lik){exp(sub.lik(sim.9) - sub.lik(sim.8))}),2) 
    
    round(exp(lik(sim.test) - lik(sim.4)),2) 
    
    print(lik.components$hiv.mortality(sim.4,debug = T))
    
}
