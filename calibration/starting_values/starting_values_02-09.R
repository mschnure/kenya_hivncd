# load 2/9 MCMC run 
load("mcmcruns/mcmc_v15_2023-02-09.Rdata")

# Take final run 
simset.15 = extract.simset(mcmc.15,
                           additional.burn=500, 
                           additional.thin=17) 
sim.15 = simset.15@simulations[[simset.15@n.sim]]
params.15 = simset.15@parameters[simset.15@n.sim,]

params.start.values = params.15
params.start.values["trate.4"] = 0.15
save(params.start.values,file=("calibration/starting_values/starting_values_02-09.Rdata"))
