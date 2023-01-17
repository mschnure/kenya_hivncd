

library(bayesian.simulations)

mcmc = assemble.mcmc.from.cache('mcmc_cache', allow.incomplete = T,
                                chains = c(1,2))

#just for this one that we didn't thin as much as maybe we could have
mcmc = subset.mcmc(mcmc, additional.thin=20)




# melissa fill in
save(mcmc,file=paste0("mcmc_v10_", Sys.Date(), ".Rdata"))
