
# source other files - add in other code 

# runs chains in parallel
create.mcmc.cache(control = control,
                  n.iter = 100000,
                  starting.values = params.start.values, 
                  cache.frequency = 500,
                  cache.dir = "mcmc_cache")