################################################################################################
# Description: Code that generates mixing proportions, i.e., the percent of an individual’s 
# partners who are in each age/sex stratum
################################################################################################

# These functions are called in succession in the parameters code when setting up transmission rates 
# (by multiplying the age/sex transmission array by an array of mixing proportions) 

# Functions
# 1. get.sex.mixing.proportions 
# 2. get.age.mixing.proportions 
# 3. calculate.pairing.proportions.by.age


# 1. Returns a vector with the proportion of an individual’s partners who are in each sex (for now 
#     hard-coded to opposite sex only) 
# 2. Called in map.model.parameters function in parameters code in order to create mixing proportions 
#     matrix to multiply by transmission rates
get.sex.mixing.proportions = function(sex.to,
                                      age.to,
                                      sexes,
                                      sampled.parameters){
    
    # returns a vector of length(sexes) that sums to 1, which is the proportion of the to stratum's partners who are in each sex
    
    rv = rep(1, length(sexes))
    names(rv) = sexes
    
    rv[sex.to] = 0 # hard coding in only pair with other sex
    
    rv
    
}

# 1. Returns a vector with the proportion of an individual’s partners who are in each age
# 2. Called in map.model.parameters function in parameters code in order to create mixing proportions 
#     matrix to multiply by transmission rates
get.age.mixing.proportions = function(sex.to,
                                      age.to,
                                      sex.from,
                                      ages,
                                      parameters,
                                      sampled.parameters){
    
    # returns a vector of length(ages) that sums to 1, which is the proportion of the to stratum's partners who are in each age 
    
    age.uppers = parameters$AGE.UPPERS
    age.lowers = parameters$AGE.LOWERS
    if (is.infinite(age.uppers[length(age.uppers)]))
        age.uppers[length(age.uppers)] = age.lowers[length(age.lowers)]+20

    if(sex.from=='female') {
        rv = calculate.pairing.proportions.by.age(age.lowers=age.lowers,
                                                  age.uppers=age.uppers,
                                                  min.sexually.active.age=parameters$min.sexually.active.age,
                                                  mean.age.diff.intercept=parameters$male.to.female.age.model$mean.age.diff.intercept,
                                                  mean.age.diff.slope=parameters$male.to.female.age.model$mean.age.diff.slope,
                                                  sd.age.diff.intercept=parameters$male.to.female.age.model$sd.age.diff.intercept,
                                                  sd.age.diff.slope=parameters$male.to.female.age.model$sd.age.diff.slope,
                                                  sd.mult=1/sampled.parameters['age.assortativity'])
    }
    
    else {
        rv = calculate.pairing.proportions.by.age(age.lowers=age.lowers,
                                                  age.uppers=age.uppers,
                                                  min.sexually.active.age=parameters$min.sexually.active.age,
                                                  mean.age.diff.intercept=parameters$female.to.male.age.model$mean.age.diff.intercept,
                                                  mean.age.diff.slope=parameters$female.to.male.age.model$mean.age.diff.slope,
                                                  sd.age.diff.intercept=parameters$female.to.male.age.model$sd.age.diff.intercept,
                                                  sd.age.diff.slope=parameters$female.to.male.age.model$sd.age.diff.slope,
                                                  sd.mult=1/sampled.parameters['age.assortativity'])
    }

    
    dimnames(rv)[[1]] = parameters$AGES
    
    rv[age.to,]
    
}

# Given regression coefficients for the mean age difference between partners (intercept and slope each 
# for mean difference and standard deviation of the mean difference), returns full matrix of pairing proportions by age
calculate.pairing.proportions.by.age <- function(age.lowers,
                                                 age.uppers,
                                                 min.sexually.active.age,
                                                 mean.age.diff.intercept,
                                                 mean.age.diff.slope,
                                                 sd.age.diff.intercept,
                                                 sd.age.diff.slope,
                                                 sd.mult){
    t(sapply(1:length(age.lowers), function(i){
        
        ages.in.bracket = (age.lowers[i]:age.uppers[i])[-1] - 0.5
        partner.dist.means = ages.in.bracket + mean.age.diff.intercept + mean.age.diff.slope * ages.in.bracket
        partner.dist.sds = (sd.age.diff.intercept + sd.age.diff.slope * ages.in.bracket) * sd.mult
        
        sexually.active = ages.in.bracket >= min.sexually.active.age
        if (!any(sexually.active))
            rep(0, length(age.lowers))
        else
        {
            p.age.of.ages.in.bracket = rep(1/sum(sexually.active), length(ages.in.bracket))
            p.age.of.ages.in.bracket[!sexually.active] = 0
            
            p.partner.ages = sapply(1:length(age.lowers), function(j){
                untruncated.ps.by.ego.age = pnorm(age.uppers[j], partner.dist.means, partner.dist.sds) - 
                    pnorm(age.lowers[j], partner.dist.means, partner.dist.sds)
                ps.by.ego.age = untruncated.ps.by.ego.age / (1-pnorm(min.sexually.active.age, partner.dist.means, partner.dist.sds))
                
                sum(ps.by.ego.age * p.age.of.ages.in.bracket)
            })
        }
    }))
}
