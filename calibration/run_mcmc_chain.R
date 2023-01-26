CHAIN = 1 # Todd will change this to 2-4 for other runs

MCMC.DIR = "R:melissa/mcmcruns"

library(bayesian.simulations)
library(ggplot2)

set.seed(1234) 

source("model/run_systematic.R")
BASE.PARAMETERS=create.model.parameters()


print(qplot(1,1) + ggtitle(paste0("CHAIN ", CHAIN)))

run.mcmc.from.cache(dir=file.path(MCMC.DIR, "mcmc_cache"),
                    chains=CHAIN,
                    update.frequency = 500)
