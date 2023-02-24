source("model/run_systematic.R")

# load("mcmcruns/mcmc_v12_2023-01-26.Rdata")
# load("mcmcruns/mcmc_v13_2023-01-30.Rdata")

mcmc=mcmc.18
simset = extract.simset(mcmc,
                           additional.burn=500,  # throw away first 500
                           additional.thin=20) 

simset.17 = extract.simset(mcmc.17,
                           additional.burn=500,  # throw away first 500
                           additional.thin=14) 

simset.18 = extract.simset(mcmc.18,
                           additional.burn=500,  # throw away first 500
                           additional.thin=20) 

simplot(simset.17,simset.18, years=2010:2020, facet.by='age', data.types='incidence', show.individual.sims = F)

## FIRST, LOOK AT OVERALL FIT (don't look at other plots until I look at mixing/MCMC properties)
simplot(simset, years = 1980:2020)

## MCMC PROPERTIES ##

## ACCEPTANCE RATE
# want it to be around 25%; window says how long to look back (would be thrown off by first iterations)
acceptance.plot(mcmc)

# checking by the variable blocks; don't want to see one chain dragged down to 0 (not mixing on that parameter)
acceptance.plot(mcmc,window.iterations = 200,by.block = T,aggregate.chains = T) 

## TRACE PLOTS
# trace plots for multiple chains: want all chains to reach the same value; homogenous mix
# want to see mixing that eventually gets to a steady state (don't want to see a trend - hasn't reached steady state yet)
# also don't want to see jumping and getting stuck in one place 
# if it's getting dragged, there should be data justifying this drag

# matches parameters with string, use * if the parameter doesn't start with that string
trace.plot(mcmc, 'trate')
trace.plot(mcmc,"*transmission")

trace.plot(mcmc, 'female.to.male.m')

trace.plot(mcmc,"*testing") 
trace.plot(mcmc,"*engagement") 
trace.plot(mcmc,"*suppression") 

trace.plot(mcmc,"age.") 
trace.plot(mcmc,"age.",additional.burn = 1000)

trace.plot(mcmc, '*hiv.mortality')
trace.plot(mcmc, '*male.hiv.mortality')
trace.plot(mcmc, '*hiv.specific.mortality')

trace.plot(mcmc,"birth")
trace.plot(mcmc,"*fertility")
trace.plot(mcmc,"*aging")

get.rhats(mcmc)

## NOW BACK TO OTHER PLOTS/FITS
simplot(simset, years=1980:2020, facet.by='age', data.types='incidence')
simplot(simset, years=1980:2020, data.types='prevalence')
simplot(simset, years=1980:2020, facet.by='age', data.types='prevalence')
simplot(simset, years=1980:2020, facet.by=c('age',"sex"), ages = "15+", data.types='prevalence')
simplot(simset, years=1980:2020, facet.by=c('age',"sex"), ages = "15+", data.types='incidence')
simplot(simset, years=1980:2020, facet.by='age', data.types='hiv.mortality',proportion = T)
simplot(simset, years=1980:2020, data.types=c('awareness',"engagement","suppression"), proportion=T)
simplot(simset, years=1980:2020, facet.by=c('age','sex'), data.types='awareness', proportion=T)
simplot(simset, years=1980:2020, facet.by=c('age','sex'), data.types='engagement', proportion=T)
simplot(simset, years=1980:2020, facet.by=c('age','sex'), data.types='suppression', proportion=T)

simplot(simset, years=1980:2020, facet.by='age', data.types='population')

# Can check last run of mcmc to change parameters manually 
params.0 = simset@parameters[simset@n.sim,] # get the last set of values for parameters
sim.0 = simset@simulations[[simset@n.sim]] # last simulation
simplot(sim.0,data.types = c("incidence"),facet.by = "age",years = 1980:2020)
simplot(sim.0,data.types = c("prevalence"),facet.by = "age",years = 1980:2020)
simplot(sim.0,data.types = c("awareness","engagement","suppression"),proportion=T)
simplot(sim.0,data.types = c("hiv.mortality"),facet.by="age",proportion = F,years=1980:2020) 
simplot(sim.0,data.types = c("hiv.mortality"),facet.by="age",proportion = T,years=1980:2020)  # fixed this plot finally 
simplot(sim.0,data.types = c("population"),facet.by = "age",years=1980:2020) 

# Check likelihood 
lik = create.likelihood(parameters=sim.from.mcmc$parameters) 
lik.components = attr(lik,"components")
lik(sim.10)
round(sapply(lik.components,function(sub.lik){exp(sub.lik(sim.10) - sub.lik(sim.11))}),2) 
round(exp(lik(sim.10) - lik(sim.11)),2) 
print(lik.components$hiv.mortality(sim.11,debug = T))


# OLD COMPARISONS 
if(1==2){
    # this is the one that ran on two chains
    simset.10 = extract.simset(mcmc.10,
                               additional.burn=500, 
                               additional.thin=10) 
    sim.10 = simset.10@simulations[[simset@n.sim]]
    
    simset.11 = extract.simset(mcmc.11,
                               additional.burn=1000, 
                               additional.thin=10) 
    sim.11 = simset.11@simulations[[simset@n.sim]]
    
    simset.12 = extract.simset(mcmc.12,
                               additional.burn=500, 
                               additional.thin=13) 
    sim.12 = simset.12@simulations[[simset@n.sim]]
    
    simplot(sim.10,sim.11,data.types = c("incidence"),years = 1980:2020)
    simplot(sim.10,sim.11,data.types = c("incidence"),facet.by = "age",years = 1980:2020)
    simplot(sim.10,sim.11,data.types = c("prevalence"),facet.by = "age",years = 1980:2020)
    simplot(sim.10,sim.11,data.types = c("hiv.mortality"),facet.by = "age",proportion = T,years = 1980:2020)
    simplot(sim.10,sim.11,data.types = c("awareness","engagement","suppression"),proportion=T,facet.by = c("age","sex"))
    simplot(sim.10,sim.11,data.types = c("awareness","engagement","suppression"),proportion=T)
    simplot(sim.10,sim.11,data.types = c("population"),facet.by="age",years=1980:2020)    
}


