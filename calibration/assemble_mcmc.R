

MCMC.DIR = "R:melissa/mcmcruns"
library(bayesian.simulations)

mcmc.20 = assemble.mcmc.from.cache(file.path(MCMC.DIR, 'mcmc_cache'), allow.incomplete = T, chains = 1:4)



# melissa fill in
save(mcmc.20,file=file.path(MCMC.DIR, paste0("mcmc_v20_", Sys.Date(), ".Rdata")))
