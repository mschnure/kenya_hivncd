

MCMC.DIR = "R:melissa/mcmcruns"
library(bayesian.simulations)

mcmc.15 = assemble.mcmc.from.cache(file.path(MCMC.DIR, 'mcmc_cache'), allow.incomplete = T, chains = 1:4)



# melissa fill in
save(mcmc.15,file=paste0("mcmcruns/mcmc_v13_", Sys.Date(), ".Rdata"))
