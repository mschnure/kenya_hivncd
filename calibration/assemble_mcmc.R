

library(bayesian.simulations)

mcmc.15 = assemble.mcmc.from.cache('mcmcruns/mcmc_cache', allow.incomplete = T,
                                chains = c(1,2))



# melissa fill in
save(mcmc.15,file=paste0("mcmcruns/mcmc_v13_", Sys.Date(), ".Rdata"))
