# library(devtools)
# install_github("tfojo1/bayesian.simulations")

library(bayesian.simulations)
library(distributions)

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

# Main things: prior, likelihood, function that maps
# Additional things: blocks to sample in, proposal distribution for values to sample - needs a covariance matrix for MVN distribution
create.adaptive.blockwise.metropolis.control(var.names = prior@var.names,
                                             simulation.function = simulation.function,
                                             log.prior.distribution = get.density.function(prior,default.log = T),
                                             log.likelihood = likelihood,
                                             var.blocks = parameter.var.blocks,
                                             transformations = transformations,
                                             initial.covariance.mat = diag((sds/20)^2), # step size
                                             burn = 0,
                                             thin = 5) 



