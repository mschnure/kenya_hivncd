
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5479933/ 

data = data.frame(
    mean.diff=c(11.5,7.0,1.5,1.7,-0.5),
    age = c(18,23,28,33,38),
    n=c(18,31,18,14,18),
    sd.diff = c(7.8,6.7,9.1,NA,NA)
)


mean.fit = lm(mean.diff ~ age, data=data, weights=data$n)
mean.fit$coefficients

age = 15:80
qplot(age, mean.fit$coefficients[1] + mean.fit$coefficients[2]*age)

sd.fit = lm(sd.diff ~ age, data=data, weights=data$n)
sd.fit$coefficients
qplot(age, sd.fit$coefficients[1] + sd.fit$coefficients[2]*age)

partner.age = age +  mean.fit$coefficients[1] + mean.fit$coefficients[2]*age
sd.diff = sd.fit$coefficients[1] + sd.fit$coefficients[2]*age

sim.df = data.frame(
    ego.age=age,
    partner.age = partner.age,
    ci.lower = partner.age - 1.96 * sd.diff,
    ci.upper = partner.age + 1.96 * sd.diff
)

ggplot(sim.df) + geom_ribbon(aes(age, ymin=ci.lower, ymax=ci.upper), alpha=0.2) +
    geom_line(aes(age, partner.age)) + ylim(0,NA) + geom_abline(intercept = 0, slope = 1, linetype='dotted')


age.lowers = c(15,20,25,30,40,50,60,70,80)
age.uppers = c(age.lowers[-1],100)

mean.fit.slope.skew = 0
sd.mult = 1

pairing.proportions = t(sapply(1:length(age.lowers), function(i){
    sim.ages = runif(2000, age.lowers[i], age.uppers[i])
    sim.partner.ages = sim.ages + rnorm(1000,
                                        mean.fit$coefficients[1] + 
                                            (mean.fit$coefficients[2]+mean.fit.slope.skew)*sim.ages,
                                        sd.mult*(sd.fit$coefficients[1] + 
                                                     sd.fit$coefficients[2]*sim.ages))
    
    partners = sapply(1:length(age.lowers), function(j){
        sum(sim.partner.ages >= age.lowers[j] & sim.partner.ages <= age.uppers[j])
    })
    partners / sum(partners)
}))

round(100*pairing.proportions)


# the more efficient way to do it using normal mixtures

# returns
calculate.pairing.proportions.by.age <- function(age.lowers,
                                                 age.uppers,
                                                 min.sexually.active.age,
                                                 mean.age.diff.intercept,
                                                 mean.age.diff.slope,
                                                 sd.age.diff.intercept,
                                                 sd.age.diff.slope,
                                                 sd.mult)
{
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
round(100*calculate.pairing.proportions.by.age(age.lowers = age.lowers,
                                               age.uppers = age.uppers,
                                               min.sexually.active.age = 15,
                                               mean.age.diff.intercept = mean.fit$coefficients[1],
                                               mean.age.diff.slope = mean.fit$coefficients[2],
                                               sd.age.diff.intercept = sd.fit$coefficients[1],
                                               sd.age.diff.slope = sd.fit$coefficients[2],
                                               sd.mult=1))
