# library(devtools)
# install_github("tfojo1/bayesian.simulations")
# install_github("tfojo1/distributions")
library(bayesian.simulations)
library(distributions)
library(ggplot2) 

source("model/run_systematic.R")
# source("calibration/starting_values_12-10.R")
# load("calibration/starting_values_12-10.Rdata") - used these for mcmc.3 and mcmc.4
# load("calibration/starting_values_12-19.Rdata") - used these for mcmc.5 and mcmc.6
# load("calibration/starting_values_01-05.Rdata") # with aging rates added in - used these for mcmc.7 and mcmc.8
# load("calibration/starting_values_01-09.Rdata") # with all age transmission splines and male cascade multipliers - used these for mcmc.9-mcmc.12
# load("calibration/starting_values/starting_values_01-26.Rdata") # with male hiv mortality multiplier 
# load("calibration/starting_values/starting_values_02-09.Rdata") # with scaled calibration targets and starting with final values from mcmc.15 run
# load("calibration/starting_values/starting_values_03-24.Rdata") # added cascade.improvement.end.year
# load("calibration/starting_values/starting_values_03-24.Rdata") # removed cascade.improvement.end.year
load("calibration/starting_values/starting_values_03-27.Rdata") # added proportion.trate.change.by.3.5

set.seed(1234) 

# mcmc.1 (12/06) - (seed: 2020)
# mcmc.2 (12/08) - (seed: 2020)
# mcmc.3 (12/12) - (seed: 1234); run with 12/10 starting values
# mcmc.4 (12/19) - (seed: 2020); run with 12/10 starting values, all years equally weighted 

# include all spline points for hiv mortality multipliers by age: 
# mcmc.5 (12/20) - (seed: 1234); run with 12/19 starting values (splined hiv mortality multipliers by age), all years equally weighted 
# mcmc.6 (12/29) - (seed: 1234); run with 12/19 starting values, all years equally weighted, downweight hiv mortality likelihood (1/100000) 
# mcmc.7 (1/5) - (seed: 1234); run with 1/5 starting values, all years equally weighted, new aging rates (returned hiv mortality to original weight)
# mcmc.8 (1/6) - (seed: 1234); run with 1/5 starting values, all years equally weighted, new aging rates, hiv mortality downweighted to 1/100000

# include new splined age transmission multipliers and separate cascade multipliers: 
# mcmc.9 (1/11) - (seed: 1234); run with 1/9 starting values, hiv mortality downweighted to 1/16
# mcmc.10 (1/13) - (seed: 1234); run with 1/9 starting values, hiv mortality downweighted to 1/256 - RAN ON 2 CHAINS
# mcmc.11 (1/17) - (seed: 2020); new weights for all likelihoods (all *0.5; suppression*44; awareness and engagement*12)
# mcmc.12 (1/26) - (seed: 1234); same as mcmc.11, but prevalence weight *.25; joint trate distributions; Todd ran on 4 chains 
# mcmc.13 (1/30) - (seed: 1234); added male.hiv.mortality.multiplier; Todd ran
# mcmc.14 (2/3) - (seed: 1234); scaled prevalence, incidence, and hiv.mortality calibration targets by age/sex to match total 
# mcmc.15 (2/9) - (seed: 1234); same as 14, Todd running on multiple chains

# mcmc.16 (2/13) - (seed: 1234); using params at end of 15 as new starting values; separate sampling blocks; Todd running on multiple chains
# mcmc.17 (2/16) - (seed: 1234); changed year weighting; added trate.4
# mcmc.18 (2/17) - (seed: 1234); corrected trate.4 to be relative to trate.3 instead of trate.2
# mcmc.19 (3/6) - (seed: 1234); trate.3 at 2018 instead of 2015 - USED IN DRAFT V5
# mcmc.20 (3/20) - incidence weight x2; years 2018 and after weight x2; trate.4 to trate.3 SD widened to log(4)/2
# mcmc.21 (3/24) - moved trate.4 spline out to 2040; added cascade improvement end year
# mcmc.22 (3/27) - removed cascade improvement end year; added proportion.trate.change.by.3.5
# mcmc.23 (3/29) - added proportion.trate.change.by.3.5 to sampling; trate.4 to trate.3 SD narrowed back to log(2)/2
# mcmc.24 (4/03) - narrowed prior on proportion.trate.change.by.3.5; trate.4 to trate.3 SD widened back to log(4)/2
# mcmc.25 (4/03 - simultaneous with v24) - median trate.4.to.3 up to 1.25; trate.4 to trate.3 SD narrowed back to log(2)/2
# mcmc.26 (4/06) - weights after 2018 increased to 4 (fixed data point weighting); put median 4.to.3 back to 1; tightened prop change by 3.5 even more 
# mcmc.27 (4/10) - penalty for decreasing awareness (.05, no strata yet)
# mcmc.28 (4/11) - greater penalty for decreasing awareness (.01) - SHORT VERSION USED IN DRAFT V6
# mcmc.29 (4/24) - even greater penalty for decreasing awareness (.001) 
# mcmc.30 - fixed awareness issue (wasn't using proportion); set it back to 0.05 
# mcmc.31 - switched to stratum-specific, used 0.3 penalty
# mcmc.32 - back to total penalty, set to 0.1
# FINAL VERSION - USING MCMC.29 WITH RESAMPLING METHOD 

# run.mcmc.from.cache() - to resume running if I stop (need the cache directory)

BASE.PARAMETERS=create.model.parameters()

simulation.function = function(sampled.parameters){
    run.model.for.parameters(variable.parameters = sampled.parameters,
                             parameters = BASE.PARAMETERS)
}

likelihood = create.likelihood(parameters = BASE.PARAMETERS)

transformations = unlist(sapply(prior@subdistributions,function(dist){
    
    if(.hasSlot(dist,"transformations"))
        sapply(dist@transformations,function(tf){
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
                                                       thin = 5) 

# set starting.values 
mcmc.24 = run.mcmc.with.cache(control = control,
                              n.iter = 10000,
                              starting.values = params.start.values, 
                              update.frequency = 5,
                              cache.frequency = 500,
                              cache.dir = "mcmc_cache"
)
# mcmc.16 = run.mcmc.from.cache(dir="mcmc_cache",
#                     update.frequency = 5)


# run.mcmc.from.cache(dir = "mcmc_cache/")

save(mcmc.19,file=paste0("mcmcruns/mcmc",Sys.Date(),".Rdata"))


if(1==2)
{
    
    
}
