source('model/age_mappings.R')

model.ages = parse.age.brackets(age.cutoffs = MODEL.AGE.CUTOFFS)
midpoint.model.ages = sapply(1:length(model.ages$labels), function(x){
    mean(c(model.ages$uppers[[x]],model.ages$lowers[[x]]))}) 
midpoint.model.ages[length(midpoint.model.ages)] = 85

testing.sexes = c("female","male")

testing.anchor.year = 2000
testing.data.years =c(2003,2008,2014)-testing.anchor.year
testing.data.ages = c(17,22,27,35,45)

prop.2003.female = c(.041,.093,.087,.074,.039)
prop.2003.male = c(.036,.091,.107,.093,.063)
prop.2008.female = c(.178,.386,.402,.304,.194)
prop.2008.male = c(.131,.254,.311,.263,.212)
prop.2014.female = c(.353,.64,.634,.544,.431)
prop.2014.male = c(.266,.536,.578,.5,.419)

max.proportion = 0.85 # ARBITRARY FOR NOW

master.df = data.frame(year=rep(testing.data.years,each=2*length(testing.data.ages)),
                       prop = c(prop.2003.female,prop.2003.male,
                                prop.2008.female,prop.2008.male,
                                prop.2014.female,prop.2014.male),
                       age = rep(testing.data.ages,2),
                       sex=rep(c("female","male"),each=length(testing.data.ages)))

intercepts = sapply(testing.sexes,function(sex){
    
    fit = suppressWarnings(glm(prop ~ year*age, family=binomial, data = master.df[master.df$sex==sex & master.df$age!=17,])) # Betas will be intercept, year, age, year:age
    fit.young = suppressWarnings(glm(prop ~ year, family=binomial, data = master.df[master.df$sex==sex & master.df$age==17,]))
    
    sapply(midpoint.model.ages,function(age){
        if(age>18){
            fit$coefficients[1] + fit$coefficients["age"]*age  
        } else {
            fit.young$coefficients[1]    
        }
    })
})

dim(intercepts) = c(dim(intercepts),1)
dimnames(intercepts)=list(age=model.ages$labels, 
                          sex=testing.sexes,
                          subgroups="all")


slopes = sapply(testing.sexes,function(sex){
    
    fit = suppressWarnings(glm(prop ~ year*age, family=binomial, data = master.df[master.df$sex==sex & master.df$age!=17,])) 
    fit.young = suppressWarnings(glm(prop ~ year, family=binomial, data = master.df[master.df$sex==sex & master.df$age==17,]))
    
    sapply(midpoint.model.ages,function(age){
        if(age>18){
            fit$coefficients["year"] + fit$coefficients["year:age"]*age 
        } else {
            fit.young$coefficients["year"]
        }
    })
})

dim(slopes) = c(dim(slopes),1)
dimnames(slopes)=list(age=model.ages$labels,
                          sex=testing.sexes,
                          subgroups="all")

TESTING.MODEL = list(intercepts=intercepts,
                     slopes=slopes,
                     anchor.year=testing.anchor.year,
                     max.proportion=max.proportion)


if(1==2){
    # For parameters table 
    # Young
    sex="female"
    fit.young = suppressWarnings(glm(prop ~ year, family=binomial, data = master.df[master.df$sex==sex & master.df$age==17,]))
    year = 2015
    young.log.odds  = fit.young$coefficients[1] + 
        fit.young$coefficients["year"]*(2015-testing.anchor.year)
    young.odds = exp(young.log.odds)
    lower.young.odds = young.odds/4
    upper.young.odds = young.odds*4
    young.p = young.odds/(1+young.odds)
    young.p = young.p*TESTING.MODEL$max.proportion
    
    # Old
    fit.old = suppressWarnings(glm(prop ~ year*age, family=binomial, data = master.df[master.df$sex==sex & master.df$age!=17,])) 
    old.age=35
    old.log.odds = fit.old$coefficients[1] + # intercept 
        fit.old$coefficients["year"]*(2015-testing.anchor.year) + # old*year
        fit.old$coefficients["age"]*old.age + # old*age
        fit.old$coefficients["year:age"]*(2015-testing.anchor.year)*old.age # old*year*age
    old.odds = exp(old.log.odds)
    lower.old.odds = old.odds/4
    upper.old.odds = old.odds*4
    old.p = old.odds/(1+old.odds)
    old.p = old.p*TESTING.MODEL$max.proportion
    
    
    # For parameters table 
    # probability, age 35-59
    c(old.p, (lower.old.odds/(1+lower.old.odds))*TESTING.MODEL$max.proportion, (upper.old.odds/(1+upper.old.odds))*TESTING.MODEL$max.proportion) 
    # OR by year for older age 
    old.year.OR = exp(fit.old$coefficients["year"])*exp(fit.old$coefficients["year:age"]*old.age) 
    c(old.year.OR,old.year.OR/4,old.year.OR*4)
    # OR by age for older age
    old.age.OR = exp(fit.old$coefficients["age"]) 
    c(old.age.OR, old.age.OR/4, old.age.OR*4)
    # probability, age 17 and under
    c(young.p, (lower.young.odds/(1+lower.young.odds))*TESTING.MODEL$max.proportion, (upper.young.odds/(1+upper.young.odds))*TESTING.MODEL$max.proportion) 
    # OR by year for younger age
    young.year.OR = exp(fit.young$coefficients["year"]) 
    c(young.year.OR,young.year.OR/4,young.year.OR*4)
    
    x = NULL
    for(i in 1:simset@n.sim){
        x[i] = simset@simulations[[i]]$parameters$time.varying.parameters$TESTING.RATES$values[41][[1]]["35-39","female",]
    }
    quantile(x,probs = c(.025,.5,.975))
    
}




# old version with linear splines to three models 
if(1==2){
    get.testing.projection = function(sex,
                                      year){
        if(sex=="female"){
            if(year=="2014"){
                ages = c(17,22,27,35,45)
                prop = c(.353,.64,.634,.544,.431)
            } else if(year=="2008"){
                ages = c(17,22,27,35,45)
                prop = c(.178,.386,.402,.304,.194)
            } else if(year=="2003"){
                ages = c(17,22,27,35,45)
                prop = c(.041,.093,.087,.074,.039)
            }
            
        } else if(sex=="male"){
            if(year=="2014"){
                ages = c(17,22,27,35,45,52)
                prop = c(.266,.536,.578,.5,.419,.37)
            } else if(year=="2008"){
                ages = c(17,22,27,35,45,52)
                prop = c(.131,.254,.311,.263,.212,.151)
            } else if(year=="2003"){
                ages = c(17,22,27,35,45)
                prop = c(.036,.091,.107,.093,.063)
            }
        }
        
        rates = -log(1-prop)
        log.odds = log(prop)-(log(1-prop))
        model = lm(log.odds[-1] ~ ages[-1])
        
        rv = list(intercept = model$coefficients[1],
                  slope = model$coefficients[2])
        
        if(1==2){
            qplot(ages,log.odds) + geom_abline(intercept = rv$intercept, slope=rv$slope)
        }
        
        rv
    }
    
    FEMALE.TESTING.2003 = get.testing.projection(sex="female",year="2003")
    MALE.TESTING.2003 = get.testing.projection(sex="male",year="2003")
    
    FEMALE.TESTING.2008 = get.testing.projection(sex="female",year="2008")
    MALE.TESTING.2008 = get.testing.projection(sex="male",year="2008")
    
    FEMALE.TESTING.2014 = get.testing.projection(sex="female",year="2014")
    MALE.TESTING.2014 = get.testing.projection(sex="male",year="2014")
}

# scratch code 1
if(1==2){
    intercept = 1.5169
    slope = -.0391
    
    # This will be done within the parameters file 
    ages.to.model = 12+5*(1:15) # fill this in with actual midpoints
    projected.log.odds = intercept + slope*ages.to.model
    projected.proportions = 1/(1+exp(-projected.log.odds)) # re-transform
    projected.rates = -log(1-projected.proportions)
    
    qplot(ages.to.model,projected.proportions) + ylim(0,1) 
}

# scratch code 2
if(1==2){
    testing.df = data.frame(year = c(2003,2008,2014)-2000,
                            prop = c(.107,.311,.578),
                            prop2 = c(.107,.311,.578)/.8) # max proportion of 0.8
    
    lm(log(prop)-(log(1-prop)) ~ year, data = testing.df) # linear regression of a log odds 
    
    fit1 = glm(prop ~ year, data = testing.df, family = binomial) # logistic regression - probably more precise
    fit2 = glm(prop2 ~ year, data = testing.df, family = binomial) # logistic regression - probably more precise
    
    years.to.project = (2000:2030)-2000
    
    expit = function(x){1/(1+exp(-x))}
    
    projections1 = expit(fit1$coefficients[1] + fit1$coefficients[2]*years.to.project)
    projections2 = 0.8*expit(fit2$coefficients[1] + fit2$coefficients[2]*years.to.project)
    
    qplot(c(years.to.project,years.to.project)+2000,c(projections1,projections2),
          color = rep(c(1,2),each=length(years.to.project)))
}

