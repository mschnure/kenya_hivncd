

library(bayesian.simulations)

mcmc.12 = assemble.mcmc.from.cache('mcmcruns/mcmc_cache', allow.incomplete = T,
                                chains = c(1,2))



# melissa fill in
save(mcmc.12,file=paste0("mcmcruns/mcmc_v12_", Sys.Date(), ".Rdata"))
