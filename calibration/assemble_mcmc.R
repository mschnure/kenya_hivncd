
source('calibration/file_settings.R')

library(bayesian.simulations)

mcmc.24 = assemble.mcmc.from.cache(file.path(MCMC.DIR, CACHE.NAME), allow.incomplete = T, chains = 1:4)



# melissa fill in
save(mcmc.24,file=file.path(MCMC.DIR, paste0("mcmc_v",MCMC.VERSION,"_", Sys.Date(), ".Rdata")))
