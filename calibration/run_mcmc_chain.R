CHAIN = 4 # Todd will change this to 2-4 for other runs

library(bayesian.simulations)
library(ggplot2)

source("model/run_systematic.R")
BASE.PARAMETERS=create.model.parameters()


print(qplot(1,1) + ggtitle(paste0("CHAIN ", CHAIN)))

run.mcmc.from.cache(dir="mcmc_cache",
                    chains=CHAIN,
                    update.frequency = 500)