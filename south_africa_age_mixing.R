##################################################################################################
# Description: Code to return regression coefficients for mean partner age differences from South 
# Africa paper: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5479933/ 
##################################################################################################

# Must be sourced before parameters

# Functions 
#     1. get.male.to.female.age.model
#     2. get.female.to.male.age.model


# 1. Using data on female ages and the mean difference in age of their partner, regresses on both the mean 
#     difference and the standard deviation of the mean differences, returns intercept and slope for each
# 2. Uses the data exactly from the paper; HIV positive women in male-female clusters with HIV-positive male; 
#     presumed male to female transmission 
get.male.to.female.age.model  = function(){
    
    # Use "HIV-positive women in male–female clusters" column of Table 3 for both mean/SD of male to female
    data = data.frame(
        mean.diff=c(11.5,7.0,1.5,1.7,-0.5),
        age = c(18,23,28,33,38),
        n=c(18,31,18,14,18),
        sd.diff = c(7.8,6.7,9.1,NA,NA)
    )
    
    mean.fit = lm(mean.diff ~ age, data=data, weights=data$n)
    sd.fit = lm(sd.diff ~ age, data=data, weights=data$n)
    
    rv = list(mean.age.diff.intercept = mean.fit$coefficients[1],
              mean.age.diff.slope = mean.fit$coefficients[2],
              sd.age.diff.intercept = sd.fit$coefficients[1],
              sd.age.diff.slope = sd.fit$coefficients[2]) 
    
    rv
}

# 1. Same process as above 
# 2. Because the paper does not report the reverse partnerships (female to male transmission), used data on 
#     HIV-positive women from the community survey for the mean difference regression and used the male to 
#     female data for the standard deviation regression   
get.female.to.male.age.model  = function(){
    
    # Use "HIV-positive women from the community survey" column of Table 3 for mean value of female to male, 
    # but use "HIV-positive women in male–female clusters" for SD as above (paper doesn't give SD for this data)
    mean.diff.for.mean = -c(4.4,6.7,4.8,6.3,2.7) 
    data.for.mean = data.frame(
        mean.diff=mean.diff.for.mean,
        age = c(18,23,28,33,38)-mean.diff.for.mean,
        n=c(140,436,566,523,497) 
    )
    
    mean.diff.for.sd = -c(11.5,7.0,1.5,1.7,-0.5) # not using this
    data.for.sd = data.frame(
        age = c(18,23,28,33,38)-mean.diff.for.mean, #changed this to mean.diff.for.mean because it wasn't working with other one 
        n=c(18,31,18,14,18),
        sd.diff = c(7.8,6.7,9.1,NA,NA)
    )

    mean.fit = lm(mean.diff ~ age, data=data.for.mean, weights=data.for.mean$n)
    sd.fit = lm(sd.diff ~ age, data=data.for.sd, weights=data.for.sd$n)
    
    rv = list(mean.age.diff.intercept = mean.fit$coefficients[1],
              mean.age.diff.slope = mean.fit$coefficients[2],
              sd.age.diff.intercept = sd.fit$coefficients[1],
              sd.age.diff.slope = sd.fit$coefficients[2]) 
    
    rv
}




## Old code for plotting/checking
if(1==2){
    sd.fit$coefficients
    qplot(age, sd.fit$coefficients[1] + sd.fit$coefficients[2]*age)
    
    sd.mult = 1
    
    partner.age = age +  mean.fit$coefficients[1] + mean.fit$coefficients[2]*age
    sd.diff = (sd.fit$coefficients[1] + sd.fit$coefficients[2]*age)*sd.mult
    
    sim.df = data.frame(
        ego.age=age,
        partner.age = partner.age,
        ci.lower = pmax(0,partner.age - 1.96 * sd.diff),
        ci.upper = pmax(0,partner.age + 1.96 * sd.diff)
    )
    
    ggplot(sim.df) + geom_ribbon(aes(age, ymin=ci.lower, ymax=ci.upper), alpha=0.2) +
        geom_line(aes(age, partner.age)) + ylim(0,NA) + geom_abline(intercept = 0, slope = 1, linetype='dotted')
    
    
    age.lowers = c(15,20,25,30,40,50,60,70,80)
    age.uppers = c(age.lowers[-1],100)
    
    mean.fit.slope.skew = 0
    
    round(100*calculate.pairing.proportions.by.age(age.lowers = age.lowers,
                                                   age.uppers = age.uppers,
                                                   min.sexually.active.age = 15,
                                                   mean.age.diff.intercept = mean.fit$coefficients[1],
                                                   mean.age.diff.slope = mean.fit$coefficients[2],
                                                   sd.age.diff.intercept = sd.fit$coefficients[1],
                                                   sd.age.diff.slope = sd.fit$coefficients[2],
                                                   sd.mult=2))
}

