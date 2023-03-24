

MCMC.DIR = "R:melissa/mcmcruns"
library(bayesian.simulations)

mcmc.22 = assemble.mcmc.from.cache(file.path(MCMC.DIR, 'mcmc_cache'), allow.incomplete = T, chains = 1:4)



# melissa fill in
save(mcmc.22,file=file.path(MCMC.DIR, paste0("mcmc_v22_", Sys.Date(), ".Rdata")))
