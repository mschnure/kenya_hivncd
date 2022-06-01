# Original non-HIV mortality
deaths.age.sex = calculate.all.death.rates(data.manager = DATA.MANAGER, 
                                           keep.dimensions = c('year','sex','age'), 
                                           model.age.cutoffs = MODEL.AGE.CUTOFFS)

anchor.year = 2020
years = as.numeric(dimnames(deaths.age.sex)$year) - anchor.year
years.label = as.numeric(dimnames(deaths.age.sex)$year) # for plotting

desired.years = c(years,max(years)+c(5,10)) # future years to predict
smoothed.years.label = desired.years + anchor.year # for plotting
mask = rep(T,length(years)) # use this to remove years

# Smoothed non-HIV mortality: fit regression on desired years only (using mask above)
smooth.deaths.age.sex = apply(deaths.age.sex,c('age','sex'),function(rates){
        
        fit = lm(log(rates[mask]) ~ years[mask]) # can change from log, which years, etc.
        
        exp(fit$coefficients[1] + fit$coefficients[2]*desired.years) #gives projections; exponentiate if log
        
})
dim.names = list(year = smoothed.years.label,
                 age = dimnames(smooth.deaths.age.sex)[2]$age,
                 sex = dimnames(smooth.deaths.age.sex)[3]$sex)
dim(smooth.deaths.age.sex) = sapply(dim.names, length)
dimnames(smooth.deaths.age.sex) = dim.names

# AIDS mortality 
AIDS.years = as.numeric(dimnames(DATA.MANAGER$AIDSmortality)$year)
AIDS.mortality = data.frame(year = AIDS.years, value = as.numeric(DATA.MANAGER$AIDSmortality))
AIDS.mortality$id = "AIDS"

# FIFTH AGE GROUP, MALE ONLY 
# age groups:"0-9","10-14","15-19","20-24","25-29","30-39","40-49","50-59","60-69","70-79","80 and over"
original.data = data.frame(year = years.label, value = as.numeric(deaths.age.sex[,5,1])) # [year,age,sex]
smoothed.data = data.frame(year = smoothed.years.label, value = as.numeric(smooth.deaths.age.sex[,5,1])) # [year,age,sex]
original.data$id = "original"
smoothed.data$id = "smoothed"

ggplot() + 
        geom_point(data = original.data, aes(x = year, y = value, color = id)) +
        geom_point(data = smoothed.data, aes(x = year, y = value, color = id)) +
        geom_point(data = AIDS.mortality, aes(x = year, y = value, color = id)) +
        ylim(0,NA)


# qplot(deaths.years,deaths.age.sex[,1,1], xlab = "year")+ylim(0,.05) 
# qplot(years.label,smooth.deaths.age.sex[,1,1], xlab = "year")+ylim(0,.05)

