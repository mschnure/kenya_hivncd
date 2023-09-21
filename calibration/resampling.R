source("model/run_systematic.R")

print("loading mcmc.29")
load("~/Dropbox/Documents_local/Hopkins/PhD/Dissertation/ABM/kenya_hivncd/mcmcruns/mcmc_v29_2023-04-24.Rdata")

simset.29.large = extract.simset(mcmc.29,
                                 additional.burn=500)

print("creating new likelihood to use for sampling")
likelihood = create.likelihood.for.trend(data.type = "awareness",
                                                  year.1=2025,
                                                  year.2=2030,
                                                  probability.of.decrease=.01,
                                                  use.strata=F)

likelihood.values = sapply(simset.29.large@simulations,likelihood)


# SAMPLE FROM SIMSET
# choose 1000 simulations, with a probability proportional to this likelihood (bad simulations are 1/99 times as likely to be sample)
print("resampling based on new awareness trend likelihood")
set.seed(12345)
indices = sample(1:simset.29.large@n.sim,1000,replace = F,prob=exp(likelihood.values))
simset.final = subset.simset(simset.29.large,indices)

print(paste0("running simset.final to 2040, with ",simset.final@n.sim," sims"))
simset.final = add.parameters(simset.final,
                              parameters = runif(simset.final@n.sim,2030,2040), 
                              parameter.names = "cascade.improvement.end.year",
                              parameter.lower.bounds = 2030, 
                              parameter.upper.bounds = 2040) 

simset.final = run.intervention.on.simset(simset.final,
                                          end.year = 2040,
                                          intervention = NO.INTERVENTION)
print("simset.final complete")
#table(exp(likelihood.values))
# table(table(indices)) # how many times were the simulations sampled (if replace=F, all once)
# table(likelihood.values[indices]) # were any bad simulations sampled 
# simset.final.thin = subset.simset(simset.final,(1:100)*10)
# simset.final@n.sim
# simplot(simset.final,  years = 2000:2030, show.individual.sims = F)
# simplot(simset.final.thin.sampled,  years = 2000:2040, show.individual.sims = F)
# simplot(simset.final.thin.sampled,  years = 2000:2040, show.individual.sims = F)
# simplot(simset.final.thin.sampled, years=2000:2040, facet.by='age', data.types='incidence', show.individual.sims = F)
# simplot(simset.final.thin.sampled, years=2000:2040, facet.by='age', data.types='prevalence', show.individual.sims = F)
# simplot(simset.final.thin.sampled, years=2010:2040, data.types=c('awareness',"engagement","suppression"), facet.by=c('age','sex'), proportion=T, show.individual.sims = F)
