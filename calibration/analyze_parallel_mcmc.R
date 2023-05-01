source("model/run_systematic.R")

simset.31= extract.simset(mcmc.31,
                           additional.burn=500,
                           additional.thin=20) # for paper, cut to 1000 sims


simset.31.sampled = add.parameters(simset.31,
                                   parameters = runif(simset.31@n.sim,2030,2040),
                                   parameter.names = "cascade.improvement.end.year",
                                   parameter.lower.bounds = 2030,
                                   parameter.upper.bounds = 2040)

simset.31.sampled = run.intervention.on.simset(simset.31.sampled,
                                               end.year = 2040,
                                               intervention = NO.INTERVENTION)


## FIRST, LOOK AT OVERALL FIT (don't look at other plots until I look at mixing/MCMC properties)
simplot(simset.31.sampled,  years = 2000:2040, show.individual.sims = F)

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

trace.plot(mcmc,"male.awareness.multiplier")

trace.plot(mcmc,"age.") 
trace.plot(mcmc,"age.",additional.burn = 1000)

trace.plot(mcmc, '*hiv.mortality')
trace.plot(mcmc, '*male.hiv.mortality')
trace.plot(mcmc, '*hiv.specific.mortality')

trace.plot(mcmc,"birth")
trace.plot(mcmc,"*fertility")
trace.plot(mcmc,"*aging.factor")

trace.plot(mcmc, "proportion.trate.change.by.3.5")

get.rhats(mcmc)

## NOW BACK TO OTHER PLOTS/FITS
simplot(simset.no.int.28.sampled, years=2000:2040, facet.by='age', data.types='incidence', show.individual.sims = F)
simplot(simset.no.int.28.2030, years=2000:2040, facet.by='age', data.types='incidence', show.individual.sims = F)
simplot(simset.no.int.28.2030, years=1980:2040, data.types='prevalence')
simplot(simset.no.int.28.sampled, sub.simset, years=2000:2040, facet.by='age', data.types='prevalence', show.individual.sims = F)
simplot(simset.no.int.25, years=1980:2020, facet.by=c('age',"sex"), ages = "15+", data.types='prevalence')
simplot(simset.no.int.28.sampled, sub.simset, years=1980:2040, facet.by=c('age',"sex"), ages = "15+", data.types='incidence', show.individual.sims = F)
simplot(simset.no.int.28.sampled, sub.simset, years=1980:2020, facet.by='age', data.types='hiv.mortality',proportion = T, show.individual.sims = F)
simplot(simset.29.sampled, years=2010:2040, data.types=c('awareness',"engagement","suppression"), proportion=T, show.individual.sims = F)
simplot(simset.no.int.28.sampled, sub.simset, years=2010:2040, data.types=c('awareness',"engagement","suppression"), facet.by=c('age','sex'), proportion=T, show.individual.sims = F)
simplot(simset.30.sampled, years=1980:2040, data.types='awareness', proportion=T)
simplot(simset.29.sampled, years=1980:2040, facet.by=c('age','sex'), data.types='awareness', proportion=T)
simplot(simset.29.sampled, years=1980:2040, facet.by=c('age','sex'), data.types='awareness', proportion=T, show.individual.sims = F)

simplot(simset, years=1980:2020, facet.by=c('age','sex'), data.types='engagement', proportion=T)
simplot(simset, years=1980:2020, facet.by=c('age','sex'), data.types='suppression', proportion=T)

simplot(simset.no.int.28.sampled, sub.simset, years=1980:2040, facet.by='age', data.types='population', show.individual.sims = F)
simplot(simset.no.int.23, years=1980:2030, facet.by='age', data.types='population', 
        ages = c("60-64","65-69","70-74","75-79","80 and over"), show.individual.sims = F)

# Can check last run of mcmc to change parameters manually 
params.0 = simset@parameters[simset@n.sim,] # get the last set of values for parameters
sim.0 = simset@simulations[[simset@n.sim]] # last simulation

simplot(sim.0,data.types = c("incidence"),facet.by = "age",years = 1980:2040)

sim.test = simset.no.int@simulations[[simset@n.sim]]
params.test = simset.no.int@parameters[simset@n.sim,]

simplot(sim.test,years=2000:2040, facet.by='age', data.types='prevalence')


# Check likelihood 
lik = create.likelihood(parameters=sim.test$parameters) 
lik.components = attr(lik,"components")
round(sapply(lik.components,function(sub.lik){exp(sub.lik(sim.better) - sub.lik(sim.worse))}),2) 
round(exp(lik(sim.level) - lik(sim.decreasing)),2) 
print(lik.components$hiv.mortality(sim.11,debug = T))

inc = sapply(simset.29.sampled@simulations, extract.incidence, years = 2040)

awareness.2025 = sapply(simset.29.sampled@simulations,extract.total.aware,years=2025)
awareness.2030 = sapply(simset.29.sampled@simulations,extract.total.aware,years=2030)
prevalence.2025 = sapply(simset.29.sampled@simulations,extract.prevalence,years=2025)
prevalence.2030 = sapply(simset.29.sampled@simulations,extract.prevalence,years=2030)
prop.aware.2025 = awareness.2025/prevalence.2025
prop.aware.2030 = awareness.2030/prevalence.2030

# only those with incidence not taking off and no decreasing awareness 
mask.1 = !is.na(inc) & !is.na(prop.aware.2025) & !is.na(prop.aware.2030) & inc<60000 & prop.aware.2025<prop.aware.2030

# only those with no decreasing awareness 
mask.2 = !is.na(prop.aware.2025) & !is.na(prop.aware.2030) & prop.aware.2025<prop.aware.2030


sum(!is.na(inc) & !is.na(prop.aware.2025) & !is.na(prop.aware.2030)
    & inc>60000  & prop.aware.2025>prop.aware.2030)/sum(!is.na(inc) & inc>60000)

sum(!is.na(prop.aware.2025) & !is.na(prop.aware.2030) & !is.na(inc) & prop.aware.2025>prop.aware.2030 & inc>60000) # mask 1
sum(!is.na(prop.aware.2025) & !is.na(prop.aware.2030) & prop.aware.2025>prop.aware.2030) # mask 2
sum(!is.na(inc) & inc>60000)

sub.simset.no.decreasing = subset.simset(simset.29.sampled,mask.2)
simplot(sub.simset.no.decreasing,  years = 2000:2040, show.individual.sims = F)

sub.simset.no.decreasing.no.high.incidence = subset.simset(simset.29.sampled,mask.1)
simplot(sub.simset.no.decreasing.no.high.incidence,  years = 2000:2040, show.individual.sims = F)
simplot(sub.simset.no.decreasing, sub.simset.no.decreasing.no.high.incidence,  years = 2000:2040, show.individual.sims = F)






# OLD COMPARISONS 
if(1==2){
    
    simset.28.for.paper = extract.simset(mcmc.28,
                                         additional.burn=500,  
                                         additional.thin=2) # for paper, cut to 1000 sims
    
    # this was the shorter version 
    simset.28 = extract.simset(mcmc.28.short,
                               additional.burn=100,  
                               additional.thin=7) 
    
    
    simset.27 = extract.simset(mcmc.27,
                               additional.burn=500,  
                               additional.thin=17) 
    
    simset.26 = extract.simset(mcmc.26.long,
                               additional.burn=500,  
                               additional.thin=16) 
    simset.26.short = extract.simset(mcmc.26.short,
                                     additional.burn=100,  
                                     additional.thin=8) 
    simset.25 = extract.simset(mcmc.25,
                               additional.burn=500,  
                               additional.thin=20) 
    simset.24 = extract.simset(mcmc.24,
                               additional.burn=500,  
                               additional.thin=15) 
    simset.23 = extract.simset(mcmc.23,
                               additional.burn=500,  
                               additional.thin=6) 
    simset.22 = extract.simset(mcmc.22,
                               additional.burn=500,  
                               additional.thin=20) 
    simset.21 = extract.simset(mcmc.21,
                               additional.burn=125,  
                               additional.thin=3) 
    simset.20 = extract.simset(mcmc.20,
                               additional.burn=500,  
                               additional.thin=20) 
    simset.19 = extract.simset(mcmc.19,
                               additional.burn=500,  
                               additional.thin=20) 
    
    simset.no.int.28.long.2035 = run.intervention.on.simset(simset.28.for.paper,
                                                            end.year = 2040,
                                                            intervention = NO.INTERVENTION)
    
    # Code to sample cascade.improvement.end.year
    simset.28.for.paper.sampled = add.parameters(simset.28.for.paper,
                                                 parameters = runif(simset.28.for.paper@n.sim,2030,2040),
                                                 parameter.names = "cascade.improvement.end.year",
                                                 parameter.lower.bounds = 2030,
                                                 parameter.upper.bounds = 2040)
    
    # with sampled cascade improvement end year
    simset.no.int.28.sampled = run.intervention.on.simset(simset.28.for.paper.sampled,
                                                          end.year = 2040,
                                                          intervention = NO.INTERVENTION)
    
    # set cascade improvement year to 2035
    simset.no.int.28.2035 = run.intervention.on.simset(simset.28,
                                                       end.year = 2040,
                                                       intervention = NO.INTERVENTION)
    
    # set cascade improvement year to 2030 
    simset.no.int.28.2030 = run.intervention.on.simset(simset.28,
                                                       end.year = 2040,
                                                       intervention = NO.INTERVENTION)
    
    # set cascade improvement year to 2040 
    simset.no.int.28.2040 = run.intervention.on.simset(simset.28,
                                                       end.year = 2040,
                                                       intervention = NO.INTERVENTION)
    
    # set cascade improvement year to 2030 
    simset.no.int.27.2030 = run.intervention.on.simset(simset.27,
                                                       end.year = 2040,
                                                       intervention = NO.INTERVENTION)
    
    # set cascade improvement year to 2040 
    simset.no.int.27.2040 = run.intervention.on.simset(simset.27,
                                                       end.year = 2040,
                                                       intervention = NO.INTERVENTION)
    
    
    
    # set cascade improvement year to 2030 
    simset.no.int.26.test = run.intervention.on.simset(simset.26.long,
                                                       end.year = 2040,
                                                       intervention = NO.INTERVENTION)
    
    # set cascade improvement year back to 2040 
    simset.no.int.26 = run.intervention.on.simset(simset.26.long,
                                                  end.year = 2040,
                                                  intervention = NO.INTERVENTION)
    
    # set cascade improvement year to 2030 
    simset.no.int.26.test.short = run.intervention.on.simset(simset.26.short,
                                                             end.year = 2040,
                                                             intervention = NO.INTERVENTION)
    
    # set cascade improvement year back to 2040 
    simset.no.int.26.short = run.intervention.on.simset(simset.26.short,
                                                        end.year = 2040,
                                                        intervention = NO.INTERVENTION)
    
    # set cascade improvement year to 2030 for test 1 and 2025 for test 2
    simset.no.int.23.test.2 = run.intervention.on.simset(simset.23,
                                                         end.year = 2040,
                                                         intervention = NO.INTERVENTION)
    
    simset.no.int.25 = run.intervention.on.simset(simset.25,
                                                  end.year = 2040,
                                                  intervention = NO.INTERVENTION)
    
    simset.no.int.24 = run.intervention.on.simset(simset.24,
                                                  end.year = 2040,
                                                  intervention = NO.INTERVENTION)
    
    simset.no.int.23 = run.intervention.on.simset(simset.23,
                                                  end.year = 2040,
                                                  intervention = NO.INTERVENTION)
    
    simset.no.int.22 = run.intervention.on.simset(simset.22,
                                                  end.year = 2040,
                                                  intervention = NO.INTERVENTION)
    
    simset.no.int.21 = run.intervention.on.simset(simset.21,
                                                  end.year = 2040,
                                                  intervention = NO.INTERVENTION)
    
    simset.no.int.20 = run.intervention.on.simset(simset.20,
                                                  end.year = 2040,
                                                  intervention = NO.INTERVENTION)
    
    simset.no.int.19 = run.intervention.on.simset(simset.19,
                                                  end.year = 2040,
                                                  intervention = NO.INTERVENTION)
    
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
    
    
    
    params.first = simset.29@parameters[1,]
    sim.first = simset.no.int.21@simulations[[1]]
    
    simplot(sim.last, sim.last.run, years=2010:2040, data.types=c('awareness',"engagement","suppression"), proportion=T)
    
    sim.last = simset.no.int.21@simulations[[simset.no.int.21@n.sim]]
    params.last = simset.no.int.21@parameters[simset.no.int.21@n.sim,]
    
    params.last.end.2040 = params.last
    params.last.end.2040["cascade.improvement.end.year"] = 2040
    
    params.last.new.trate.4 = params.last
    params.last.new.trate.4["trate.4"] = 0.5
    
    sim.last.end.2040 = run.model.for.parameters(params.first, end.year = 2040)
    sim.last.new.trate.4 = run.model.for.parameters(params.last.new.trate.4, end.year = 2040)
    
    simplot(sim.last.end.2040, sim.last.new.trate.4, years=2000:2040, facet.by='age', data.types='incidence')
    simplot(sim.last.end.2040, sim.last.new.trate.4, years=2010:2040, data.types=c('awareness',"engagement","suppression"), proportion=T)
    
    # checking sims with high trate.4
    qplot(simset.22@parameters[,"trate.4"])
    z= (1:simset.22@n.sim)[simset.22@parameters[,"trate.4"]>.4]
    
    qplot(simset.22@parameters[,"trate.4"]/simset.22@parameters[,"trate.3"])
    qplot(simset.22@parameters[z,"trate.4"]/simset.22@parameters[z,"trate.3"])
    
    
    x = sapply(simset.no.int.27.2040@simulations,extract.incidence,years=2040)
    
    
    simplot(sim.better,sim.worse,data.types = c("prevalence"),facet.by = "age",years = 2000:2020)
    simplot(sim.better,sim.worse,data.types = c("awareness","engagement","suppression"),proportion=T)
    simplot(sim.0,data.types = c("hiv.mortality"),facet.by="age",proportion = F,years=1980:2020) 
    simplot(sim.0,data.types = c("hiv.mortality"),facet.by="age",proportion = T,years=1980:2020)  # fixed this plot finally 
    simplot(sim.0,data.types = c("population"),facet.by = "age",years=1980:2020) 
}


