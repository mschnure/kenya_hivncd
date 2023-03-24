
MCMC.DIR = "R:melissa/mcmcruns"

N.CHAINS = 4

library(bayesian.simulations)
library(distributions)
library(ggplot2) 

source("model/run_systematic.R")
load("calibration/starting_values/starting_values_03-24.Rdata")

set.seed(1234) 

BASE.PARAMETERS=create.model.parameters()

simulation.function = function(sampled.parameters){
    run.model.for.parameters(variable.parameters = sampled.parameters,
                             parameters = BASE.PARAMETERS)
}

likelihood = create.likelihood(parameters = BASE.PARAMETERS)

transformations = unlist(sapply(prior@subdistributions,function(dist){
    
    if (.hasSlot(dist, 'transformations'))
        sapply(dist@transformations, function(tf){
            tf@name
        })
    else if(is.null(dist@transformation))
        "identity"
    else
        dist@transformation@name
    
}))
names(transformations) = prior@var.names

sds = get.sds(prior)
sds = sds[names(params.start.values)]

control = create.adaptive.blockwise.metropolis.control(var.names = prior@var.names,
                                                       simulation.function = simulation.function,
                                                       log.prior.distribution = get.density.function(prior,default.log = T),
                                                       log.likelihood = likelihood,
                                                       var.blocks = parameter.var.blocks,
                                                       transformations = transformations,
                                                       initial.covariance.mat = diag((sds/20)^2), # step size
                                                       burn = 0,
                                                       thin = 100) 



n.start.values = sapply(1:length(params.start.values), function(i){
        rnorm(N.CHAINS, params.start.values[i], sds[i]/64)
    })
dimnames(n.start.values)[[2]] = names(params.start.values)

# runs chains in parallel
create.mcmc.cache(control = control,
                  n.iter = 100000,
                  starting.values = n.start.values, 
                  cache.frequency = 500,
                  dir = file.path(MCMC.DIR, "mcmc_cache"))


