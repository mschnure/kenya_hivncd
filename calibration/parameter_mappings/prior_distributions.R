library(distributions)
library(boot) 

# for most of these, log normal is fine; maybe logit normal for some; types of parameters: rates, ratios
# if lognormal, being off by a factor in either direction is equivalent, rather than absolute value (half versus 2x are the same)
# log(x)/2 means 95% cred interval from 1/x*guess to x*guess

# mean value given to function is log of what I think the value should be (e.g., mean value of log(1) means I think the value is 1)
# without data, best guess would have mean at 0 (or 1) and then decide how far off I'm willing to be 
# log(4)/2 --> can be off by a factor of 4 (=log(2), just keeping this way for clarity)

make.joint.distribution = function(median.r2, # anchor on trate.2
                                   sd.r2,
                                   median.r0.to.r1, #0 relative to 1
                                   sd.r0.to.r1,
                                   median.r1.to.r2, #1 relative to 2
                                   sd.r1.to.r2,
                                   median.r3.to.r2, #3 relative to 2
                                   sd.r3.to.r2, 
                                   median.r4.to.r3, #4 relative to 3
                                   sd.r4.to.r3){
    
    mean.vector = log(c(median.r2,
                      median.r0.to.r1,
                      median.r1.to.r2,
                      median.r3.to.r2,
                      median.r4.to.r3))
    cov.mat = diag(c(sd.r2,
                     sd.r0.to.r1,
                     sd.r1.to.r2,
                     sd.r3.to.r2,
                     sd.r4.to.r3)^2)
    
    M = rbind(c(1,1,1,0,0), # which elements of the mean vector you multiply to get r0
              c(1,0,1,0,0), # " " r1
              c(1,0,0,0,0), # " " r2
              c(1,0,0,1,0), # " " r3
              c(1,0,0,1,1)) # " " r4 
    
    new.mean.vector = M %*% mean.vector
    new.cov.mat = M %*% cov.mat %*% t(M)
    
    Multivariate.Lognormal.Distribution(mu = new.mean.vector,
                                        sigma = new.cov.mat,
                                        var.names = c("trate.0","trate.1","trate.2","trate.3","trate.4"))
    
}



prior = join.distributions(
    
    # general 
    trates = make.joint.distribution(median.r2 = 0.25,
                                     sd.r2=log(4)/2,
                                     median.r0.to.r1 = 8,
                                     sd.r0.to.r1 = log(2)/2,
                                     median.r1.to.r2 = 1,
                                     sd.r1.to.r2 = log(2)/2,
                                     median.r3.to.r2 = 1,
                                     sd.r3.to.r2 = log(2)/2,
                                     median.r4.to.r3 = 1,
                                     sd.r4.to.r3 = log(2)/2),
    
    
    # trate.0 = Lognormal.Distribution(log(.5), log(8)/2), # ORIGINALLY 1; (log(1) = 0, but leaving this way for clarity)
    # trate.1 = Lognormal.Distribution(log(.25), log(8)/2), # ORIGINALLY 1
    # trate.2 = Lognormal.Distribution(log(.25), log(8)/2), # ORIGINALLY 1
    # trate.3 = Lognormal.Distribution(log(.25), log(8)/2), # ORIGINALLY 1
    
    # sex transmission multipliers
    female.to.male.multiplier = Lognormal.Distribution(log(1), log(4)/2), 
    
    # age transmission multipliers
    age.15.to.19.transmission.multiplier.0 = Lognormal.Distribution(log(1), log(4)/2),
    age.15.to.19.transmission.multiplier.1 = Lognormal.Distribution(log(1), log(4)/2),
    age.15.to.19.transmission.multiplier.2 = Lognormal.Distribution(log(1), log(4)/2),
    age.15.to.19.transmission.multiplier.3 = Lognormal.Distribution(log(1), log(4)/2),
    
    age.20.to.29.transmission.multiplier.0 = Lognormal.Distribution(log(1), log(4)/2),
    age.20.to.29.transmission.multiplier.1 = Lognormal.Distribution(log(1), log(4)/2),
    age.20.to.29.transmission.multiplier.2 = Lognormal.Distribution(log(1), log(4)/2),
    age.20.to.29.transmission.multiplier.3 = Lognormal.Distribution(log(1), log(4)/2),
    
    age.40.to.49.transmission.multiplier.0 = Lognormal.Distribution(log(1), log(4)/2),
    age.40.to.49.transmission.multiplier.1 = Lognormal.Distribution(log(1), log(4)/2),
    age.40.to.49.transmission.multiplier.2 = Lognormal.Distribution(log(1), log(4)/2),
    age.40.to.49.transmission.multiplier.3 = Lognormal.Distribution(log(1), log(4)/2),
    
    age.50.and.over.transmission.multiplier.0 = Lognormal.Distribution(log(1), log(4)/2),
    age.50.and.over.transmission.multiplier.1 = Lognormal.Distribution(log(1), log(4)/2),
    age.50.and.over.transmission.multiplier.2 = Lognormal.Distribution(log(1), log(4)/2),
    age.50.and.over.transmission.multiplier.3 = Lognormal.Distribution(log(1), log(4)/2), 
    
    # other transmission multipliers
    age.assortativity = Lognormal.Distribution(log(1), log(1.5)/2), # probably need to be as tight as log(1.5)/2
    
    birth.transmission.risk.0 = Logitnormal.Distribution(logit(0.42), log(3)/2), 
    birth.transmission.risk.1 = Logitnormal.Distribution(logit(0.3), log(3)/2), 
    # because logit, this means off by an *OR* of 4 (as opposed to just a multiplier of 4)
    # can arbitrarily pick SD to include what's included in the paper 
    # because birth transmission risk is a proportion, either logit normal or beta 
    # (for any beta, there is a logit normal that approximates)
    
    # cascade parameters
    log.OR.testing.intercept = Normal.Distribution(0, log(4)/2), 
    log.OR.testing.slope = Normal.Distribution(0, log(4)/2),
    
    log.OR.engagement.intercept = Normal.Distribution(0, log(4)/2),
    log.OR.engagement.pre.universal.slope = Normal.Distribution(0, log(4)/2),
    log.OR.engagement.intermediate.slope = Normal.Distribution(0, log(4)/2),
    log.OR.engagement.post.universal.slope = Normal.Distribution(0, log(4)/2),
    
    unsuppressed.disengagement.rates = Lognormal.Distribution(log(0.1392621), log(4)/2),
    suppressed.disengagement.rates = Lognormal.Distribution(log(0.1025866), log(4)/2),
    
    suppression.rate.0 = Lognormal.Distribution(log(0.6732885), log(4)/2),
    suppression.rate.1 = Lognormal.Distribution(log(0.6732885), log(4)/2),
    unsuppression.rates = Lognormal.Distribution(log(0.1971601), log(4)/2),
    
    male.awareness.multiplier = Lognormal.Distribution(log(1), log(4)/2),
    male.engagement.multiplier = Lognormal.Distribution(log(1), log(4)/2),
    male.suppression.multiplier = Lognormal.Distribution(log(1), log(4)/2),
    
    # mortality/fertility parameters 
    age.45.to.65.mortality.intercept.multiplier = Lognormal.Distribution(log(1), log(4)/2),
    age.45.to.65.mortality.slope.multiplier = Lognormal.Distribution(log(1), log(4)/2),
    over.65.mortality.intercept.multiplier = Lognormal.Distribution(log(1), log(4)/2),
    over.65.mortality.slope.multiplier = Lognormal.Distribution(log(1), log(4)/2),
    
    hiv.specific.mortality.rates.0 = Lognormal.Distribution(log(0.04), log(4)/2),
    hiv.specific.mortality.rates.1 = Lognormal.Distribution(log(0.07), log(4)/2),
    hiv.specific.mortality.rates.2 = Lognormal.Distribution(log(0.018), log(4)/2),
    
    male.hiv.mortality.multiplier.0 = Lognormal.Distribution(log(1), log(4)/2),
    male.hiv.mortality.multiplier.1 = Lognormal.Distribution(log(1), log(4)/2),
    male.hiv.mortality.multiplier.2 = Lognormal.Distribution(log(1), log(4)/2),
    
    age.0.to.14.hiv.mortality.multiplier.0 = Lognormal.Distribution(log(1), log(4)/2),
    age.0.to.14.hiv.mortality.multiplier.1 = Lognormal.Distribution(log(1), log(4)/2),
    age.0.to.14.hiv.mortality.multiplier.2 = Lognormal.Distribution(log(1), log(4)/2),
    
    age.15.to.24.hiv.mortality.multiplier.0 = Lognormal.Distribution(log(1), log(4)/2),
    age.15.to.24.hiv.mortality.multiplier.1 = Lognormal.Distribution(log(1), log(4)/2),
    age.15.to.24.hiv.mortality.multiplier.2 = Lognormal.Distribution(log(1), log(4)/2),
    
    over.50.hiv.mortality.multiplier.0 = Lognormal.Distribution(log(1), log(4)/2),
    over.50.hiv.mortality.multiplier.1 = Lognormal.Distribution(log(1), log(4)/2),
    over.50.hiv.mortality.multiplier.2 = Lognormal.Distribution(log(1), log(4)/2),
    
    fertility.multiplier = Lognormal.Distribution(log(1), log(4)/2),
    
    age.15.to.19.base.aging.rate=Lognormal.Distribution(log(0.25), log(4)/2),
    age.20.to.24.base.aging.rate=Lognormal.Distribution(log(0.25), log(4)/2),
    age.15.to.19.aging.factor=Lognormal.Distribution(log(2), log(4)/2),
    age.20.to.24.aging.factor=Lognormal.Distribution(log(2), log(4)/2),
    age.25.to.50.aging.factor=Lognormal.Distribution(log(2), log(4)/2),
    over.50.aging.factor=Lognormal.Distribution(log(2), log(4)/2)
    
    
)


parameter.var.blocks = list(
    
    trate0 = "trate.0",
    trate1 = "trate.1",
    trate2 = "trate.2",
    trate3 = "trate.3", 
    trate4 = "trate.4", 
    
    sex.transmission.multiplier = c("female.to.male.multiplier"),
    
    age.transmission.multipliers.00 = c("age.15.to.19.transmission.multiplier.0"),
    
    age.transmission.multipliers.01 = c("age.20.to.29.transmission.multiplier.0"),
    
    age.transmission.multipliers.02 = c("age.40.to.49.transmission.multiplier.0"),
    
    
    age.transmission.multipliers.1 = c("age.15.to.19.transmission.multiplier.1",
                                       "age.15.to.19.transmission.multiplier.2",
                                       "age.15.to.19.transmission.multiplier.3"),
    
    age.transmission.multipliers.2 = c("age.20.to.29.transmission.multiplier.1",
                                       "age.20.to.29.transmission.multiplier.2",
                                       "age.20.to.29.transmission.multiplier.3"),
    
    age.transmission.multipliers.3 = c("age.40.to.49.transmission.multiplier.1",
                                       "age.40.to.49.transmission.multiplier.2",
                                       "age.40.to.49.transmission.multiplier.3"),
    
    age.transmission.multipliers.4 = c("age.50.and.over.transmission.multiplier.0",
                                       "age.50.and.over.transmission.multiplier.1",
                                       "age.50.and.over.transmission.multiplier.2",
                                       "age.50.and.over.transmission.multiplier.3"),
    
    
    age.assortativity = c("age.assortativity"),
    
    birth.transmission = c("birth.transmission.risk.0",
                           "birth.transmission.risk.1"),
    
    testing = c("log.OR.testing.intercept",
                "log.OR.testing.slope"),
    
    engagement = c("log.OR.engagement.intercept",
                   "log.OR.engagement.pre.universal.slope",
                   "log.OR.engagement.intermediate.slope",
                   "log.OR.engagement.post.universal.slope"),
    
    
    disengagement = c("unsuppressed.disengagement.rates",
                      "suppressed.disengagement.rates"),
    
    suppression = c("suppression.rate.0",
                    "suppression.rate.1",
                    "unsuppression.rates"),
    
    male.cascade.multiplier = c("male.awareness.multiplier",
                                "male.engagement.multiplier",
                                "male.suppression.multiplier"),
    
    general.mortality = c("age.45.to.65.mortality.intercept.multiplier",
                          "age.45.to.65.mortality.slope.multiplier",
                          "over.65.mortality.intercept.multiplier",
                          "over.65.mortality.slope.multiplier"),
    
    hiv.mortality = c("hiv.specific.mortality.rates.0",
                      "hiv.specific.mortality.rates.1",
                      "hiv.specific.mortality.rates.2"),
    
    hiv.mortality.sex = c("male.hiv.mortality.multiplier.0",
                          "male.hiv.mortality.multiplier.1",
                          "male.hiv.mortality.multiplier.2"),
    
    hiv.mortality.age.multipliers.infant = c("age.0.to.14.hiv.mortality.multiplier.0",
                                             "age.0.to.14.hiv.mortality.multiplier.1",
                                             "age.0.to.14.hiv.mortality.multiplier.2"),
    hiv.mortality.age.multipliers.young = c("age.15.to.24.hiv.mortality.multiplier.0",
                                            "age.15.to.24.hiv.mortality.multiplier.1",
                                            "age.15.to.24.hiv.mortality.multiplier.2"),
    
    hiv.mortality.age.multipliers.old = c("over.50.hiv.mortality.multiplier.0",
                                          "over.50.hiv.mortality.multiplier.1",
                                          "over.50.hiv.mortality.multiplier.2"),
    
    fertility = c("fertility.multiplier"),
    
    aging.1 = c("age.15.to.19.base.aging.rate",
                "age.20.to.24.base.aging.rate"),
    
    aging.2 = c("age.15.to.19.aging.factor",
                "age.20.to.24.aging.factor",
                "age.25.to.50.aging.factor",
                "over.50.aging.factor")
)

# checking for missing variables
if(1==2){
    
    setdiff(prior@var.names,unlist(parameter.var.blocks)) # everything that is in the first set but not the second - order matters
    setdiff(unlist(parameter.var.blocks),prior@var.names)
    
}






