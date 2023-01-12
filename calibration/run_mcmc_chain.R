CHAIN = 1 # Todd will change this to 2-4 for other runs

run.mcmc.from.cache(dir="mcmc_cache",
                    chains=CHAIN,
                    update.frequency = 500)