

library(bayesian.simulations)

mcmc.13 = assemble.mcmc.from.cache('mcmcruns/mcmc_cache', allow.incomplete = T,
                                chains = c(1,2))



# melissa fill in
save(mcmc.13,file=paste0("mcmcruns/mcmc_v13_", Sys.Date(), ".Rdata"))
