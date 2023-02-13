

MCMC.DIR = "R:melissa/mcmcruns"
library(bayesian.simulations)

mcmc.17 = assemble.mcmc.from.cache(file.path(MCMC.DIR, 'mcmc_cache'), allow.incomplete = T, chains = 1:4)



# melissa fill in
save(mcmc.17,file=file.path(MCMC.DIR, paste0("mcmc_v15_", Sys.Date(), ".Rdata")))
