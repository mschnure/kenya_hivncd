library(distributions)
library(boot) # is this the right package for the logit function? 

# for most of these, log normal is fine; maybe logit normal for some; types of parameters: rates, ratios
# if lognormal, being off by a factor in either direction is equivalent, rather than absolute value (half versus 2x are the same)
# log(x)/2 means 95% cred interval from 1/x*guess to x*guess

# mean value given to function is log of what I think the value should be 
# without data, best guess would have mean at 0 and then decide how far off I'm willing to be 


# for now, filled in with values that I landed at that made curves fit pretty well - need to find actual data-supported values though
prior = join.distributions(
    
    # general
    trate.0 = Lognormal.Distribution(log(0.7), log(4)/2), # can be off by a factor of 4 (=log(2), just keeping this way for clarity)
    trate.1 = Lognormal.Distribution(log(0.2), log(4)/2),
    trate.2 = Lognormal.Distribution(log(0.15), log(4)/2),
    
    # sex transmission multipliers
    female.to.male.multiplier = Lognormal.Distribution(log(1.03), log(4)/2),
    
    # age transmission multipliers
    age.15.to.19.transmission.multiplier = Lognormal.Distribution(log(0.67), log(4)/2),
    age.20.to.29.transmission.multiplier = Lognormal.Distribution(log(1.23), log(4)/2),
    age.40.to.49.transmission.multiplier = Lognormal.Distribution(log(1.1), log(4)/2),
    age.50.and.over.transmission.multiplier.0 = Lognormal.Distribution(log(0.55), log(4)/2),
    age.50.and.over.transmission.multiplier.1 = Lognormal.Distribution(log(0.35), log(4)/2),
    age.50.and.over.transmission.multiplier.2 = Lognormal.Distribution(log(0.28), log(4)/2),
    
    # other transmission multipliers
    age.assortativity = Lognormal.Distribution(log(0.8), log(4)/2),
    birth.transmission.risk = Logitnormal.Distribution(logit(0.8), logit(4)/2), # is this right? 
    # because birth transmission risk is a proportion, either logit normal or beta 
    # (for any beta, there is a logit normal that approximates)
    
    # cascade parameters
    testing.rate.1 = Lognormal.Distribution(log(0.5), log(4)/2),
    engagement.rate.2 = Lognormal.Distribution(log(1.5), log(4)/2),
    suppression.rate.0 = Lognormal.Distribution(log(0.7), log(4)/2),
    suppression.rate.1 = Lognormal.Distribution(log(4), log(4)/2),
    unsuppression.rates = Lognormal.Distribution(log(.05), log(4)/2),
    male.cascade.multiplier = Lognormal.Distribution(log(0.6), log(4)/2),
    
    # mortality/fertility parameters 
    age.45.to.65.mortality.intercept.multiplier = Lognormal.Distribution(log(2.3), log(4)/2),
    age.45.to.65.mortality.slope.multiplier = Lognormal.Distribution(log(1.01), log(4)/2),
    over.65.mortality.intercept.multiplier = Lognormal.Distribution(log(1.0), log(4)/2),
    over.65.mortality.slope.multiplier = Lognormal.Distribution(log(1.01), log(4)/2),
    
    hiv.specific.mortality.rates.1 = Lognormal.Distribution(log(0.025), log(4)/2),
    hiv.specific.mortality.rates.2 = Lognormal.Distribution(log(0.1), log(4)/2),
    hiv.specific.mortality.rates.3 = Lognormal.Distribution(log(0.03), log(4)/2),
    
    age.15.to.24.hiv.mortality.multiplier = Lognormal.Distribution(log(0.4), log(4)/2),
    over.50.hiv.mortality.multiplier = Lognormal.Distribution(log(3), log(4)/2),
    age.0.to.14.hiv.mortality.multiplier.1 = Lognormal.Distribution(log(12), log(4)/2),
    
)
