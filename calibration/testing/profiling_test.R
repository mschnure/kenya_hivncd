
library(bayesian.simulations)
library(distributions)
library(ggplot2) 

source("model/run_systematic.R")
source("calibration/starting_values.R")
set.seed(2020) 


N.ITER=100 # do 100 later on
BASE.PARAMETERS=create.model.parameters()

simulation.function = function(sampled.parameters){
    run.model.for.parameters(variable.parameters = sampled.parameters,
                             parameters = BASE.PARAMETERS)
}

likelihood = create.likelihood(parameters = BASE.PARAMETERS)

transformations = sapply(prior@subdistributions,function(dist){
    
    if(is.null(dist@transformation))
        "identity"
    else
        dist@transformation@name
    
})

sds = get.sds(prior)

control = create.adaptive.blockwise.metropolis.control(var.names = prior@var.names,
                                                       simulation.function = simulation.function,
                                                       log.prior.distribution = get.density.function(prior,default.log = T),
                                                       log.likelihood = likelihood,
                                                       var.blocks = parameter.var.blocks,
                                                       transformations = transformations,
                                                       initial.covariance.mat = diag((sds/20)^2), # step size
                                                       burn = 0,
                                                       thin = 5) 

print("starting profiler")
Rprof() # starts profiler

mcmc.test = run.mcmc(control = control,
                     n.iter = N.ITER,
                     starting.values = params.test,
                     cache.frequency = NA
)


Rprof(NULL) # ends profiler
print("stopping profiler")
summaryRprof()

