#################################################################################################
# Description: Functions to set up the parameter object with constant and time-varying parameters
#################################################################################################

# Core functions 
#     1. create.model.parameters 
#     2. get.default.parameters 
#     3. map.model.parameters 
#     4. add.time.varying.parameter.value
#     5. compute.time.varying.parameters
# Other/helper functions 
#     1. calculate.all.death.rates 
#     2. get.initial.population 
#     3. make.transmission.array
#     4. map.birth.rates (no longer in use)

library(splines)

##--------------------##
##-- CORE FUNCTIONS --##
##--------------------##

# Sets up the basic parameters (sexes, ages, subgroups, HIV status); 
# call this function with no arguments when first setting up a test case 
create.model.parameters <- function(age.cutoffs=MODEL.AGE.CUTOFFS, #the lower limit for each bracket
                                    sexes = c('female','male'),
                                    subgroups = 'all',
                                    min.sexually.active.age=14){
    parameters = list()
    
    #-- SET UP THE BASICS --#
    
    # sex, risk, subpop
    parameters$SEXES = sexes 
    parameters$SUBGROUPS = subgroups
    
    # ages
    parsed.ages = parse.age.brackets(age.cutoffs)
    
    parameters$AGES = parsed.ages$labels
    parameters$AGE.SPANS = parsed.ages$spans
    parameters$AGE.LOWERS = parsed.ages$lowers
    names(parameters$AGE.LOWERS) = parameters$AGES
    parameters$AGE.UPPERS = parsed.ages$uppers
    names(parameters$AGE.UPPERS) = parameters$AGES
    
    
    # hiv status
    parameters$HIV.STATUS = c('hiv_negative','undiagnosed','diagnosed_unengaged','engaged_unsuppressed','engaged_suppressed')
    parameters$HIV.STATES = c('undiagnosed','diagnosed_unengaged','engaged_unsuppressed','engaged_suppressed') 
    parameters$DIAGNOSED.STATES = c('diagnosed_unengaged','engaged_unsuppressed','engaged_suppressed')
    parameters$ENGAGED.STATES = c('engaged_unsuppressed','engaged_suppressed')
    
    parameters$min.sexually.active.age = min.sexually.active.age
    parameters$male.to.female.age.model = get.male.to.female.age.model()
    parameters$female.to.male.age.model = get.female.to.male.age.model()
    
    #- Return --#
    parameters  
}

# Sets default values for parameters we will sample; 
# called in “run_systematic” code with the option to change values
get.default.parameters = function(){
    rv = c(
        ## Transmission parameters ##
        # general
        start.time=1975,
        time.0=1990,
        time.1=1997,
        time.2=2008, 
        time.3=2015,
        trate.0=0.7,
        trate.1=0.2,
        trate.2=0.2, 
        trate.3=0.15,
        # sex transmission multipliers
        male.to.male.multiplier=1,
        female.to.male.multiplier=1,
        # age transmission multipliers
        age.15.to.19.transmission.multiplier.0=1,
        age.15.to.19.transmission.multiplier.1=1,
        age.15.to.19.transmission.multiplier.2=1,
        age.15.to.19.transmission.multiplier.3=1,
        
        age.20.to.29.transmission.multiplier.0=1,
        age.20.to.29.transmission.multiplier.1=1,
        age.20.to.29.transmission.multiplier.2=1,
        age.20.to.29.transmission.multiplier.3=1,
        
        age.40.to.49.transmission.multiplier.0=1,
        age.40.to.49.transmission.multiplier.1=1,
        age.40.to.49.transmission.multiplier.2=1,
        age.40.to.49.transmission.multiplier.3=1,
        
        age.50.and.over.transmission.multiplier.0=1,
        age.50.and.over.transmission.multiplier.1=1,
        age.50.and.over.transmission.multiplier.2=1,
        age.50.and.over.transmission.multiplier.3=1,
        # other transmission multipliers
        relative.transmission.from.diagnosis=0.33, 
        age.assortativity=1, 
        birth.transmission.time.0=1990,
        birth.transmission.time.1=2020,
        birth.transmission.risk.0=0.42,
        birth.transmission.risk.1=0.30,
        
        ## Cascade parameters ##
        log.OR.testing.intercept=0, # 0 because on log scale
        log.OR.testing.slope=0,
        log.OR.engagement.intercept=0,
        log.OR.engagement.pre.universal.slope=0,
        log.OR.engagement.intermediate.slope=0,
        log.OR.engagement.post.universal.slope=0,
        unsuppressed.disengagement.rates=0.1392621, # Lee et al
        suppressed.disengagement.rates=0.1025866, # Lee et al
        suppression.time.0=1993,
        suppression.time.1=2003,
        suppression.rate.0=1.118678, # Njuguna et al
        suppression.rate.1=1.118678, # Njuguna et al
        unsuppression.rates=0.1971601, # Maina et al
        male.awareness.multiplier=1,
        male.engagement.multiplier=1,
        male.suppression.multiplier=1,
        
        ## Mortality/fertility parameters ##
        # multiplies intercept or slope before projecting
        age.45.to.65.mortality.intercept.multiplier=1,
        age.45.to.65.mortality.slope.multiplier=1,
        over.65.mortality.intercept.multiplier=1, 
        over.65.mortality.slope.multiplier=1, 
        hiv.mortality.time.0=1990,
        hiv.mortality.time.1=2005,
        hiv.mortality.time.2=2020,
        hiv.specific.mortality.rates.0=0.04,  
        hiv.specific.mortality.rates.1=0.07,  
        hiv.specific.mortality.rates.2=0.018,
        male.hiv.mortality.multiplier.0=1,
        male.hiv.mortality.multiplier.1=1,
        male.hiv.mortality.multiplier.2=1,
        age.0.to.14.hiv.mortality.multiplier.0=1,
        age.0.to.14.hiv.mortality.multiplier.1=1,
        age.0.to.14.hiv.mortality.multiplier.2=1,
        age.15.to.24.hiv.mortality.multiplier.0=1,
        age.15.to.24.hiv.mortality.multiplier.1=1,
        age.15.to.24.hiv.mortality.multiplier.2=1,
        over.50.hiv.mortality.multiplier.0=1,
        over.50.hiv.mortality.multiplier.1=1,
        over.50.hiv.mortality.multiplier.2=1,
        fertility.multiplier=1,
        
        # Aging rates
        age.15.to.19.base.aging.rate=0.25,
        age.20.to.24.base.aging.rate=0.25,
        age.15.to.19.aging.factor=2,
        age.20.to.24.aging.factor=2,
        age.25.to.50.aging.factor=2,
        over.50.aging.factor=2
    ) 
}



#-- MAP ALL PARAMETERS --#
#   1. Uses parameters object (set up via create.model.parameters) and sampled parameters 
#       (set up via get.default.parameters) to set up full set of parameters needed for diffeq 
#       (all dimensions of age/sex, etc., all years) 
#   2. Types of parameters: fertility, aging, mortality (HIV/non-HIV), diagnoses, transmission rates, 
#       infectiousness, engagement/disengagement, suppression/unsuppression
#   3. Everything technically added as a time-varying parameter even if it doesn’t vary 
map.model.parameters <- function(parameters,
                                 sampled.parameters=get.default.parameters(),
                                 age.cutoffs=MODEL.AGE.CUTOFFS,
                                 project.to.year=2040){
    
    #-- SET UP DIMENSIONS --#
    state.dim.names = list(age=parameters$AGES, 
                           sex=parameters$SEXES,
                           subgroup=parameters$SUBGROUPS,
                           hiv.status=parameters$HIV.STATUS)
    
    n.states = prod(sapply(state.dim.names, length))
    
    trans.dim.names = list(age=parameters$AGES, 
                           sex=parameters$SEXES,
                           subgroup=parameters$SUBGROUPS)
    
    n.trans.states = prod(sapply(trans.dim.names, length))
    
    #-- BIRTH --# 
    
    age.specific.fertility = get.surveillance.data(data.manager = DATA.MANAGER, 
                                                   data.type = "fertility", 
                                                   years = DATA.MANAGER$fertility$YEARS, 
                                                   keep.dimensions = 'age')
    
    full.dim.names = list(year = DATA.MANAGER$fertility$YEARS,
                          age = parameters$AGES,
                          sex = parameters$SEXES)
    
    all.fertility = array(0,
                          dim = sapply(full.dim.names, length),
                          dimnames = full.dim.names)
    
    all.fertility[,dimnames(age.specific.fertility)$age,"female"] = age.specific.fertility
    
    for (year in dimnames(all.fertility)$year){
        rv = array(all.fertility[year,,]*sampled.parameters["fertility.multiplier"], #added a tuning parameter to match population
                   dim = sapply(state.dim.names, length),
                   dimnames = state.dim.names)
        
        parameters = add.time.varying.parameter.value(parameters,
                                                      parameter.name='FERTILITY.RATES',
                                                      value = rv,
                                                      time = as.numeric(year))
    }
    
    # old version - crude birth rate for entire population
    if(1==2){
        births.age.sex = map.birth.rates(data.manager = DATA.MANAGER,
                                         model.age.cutoffs = MODEL.AGE.CUTOFFS)
        
        for (year in dimnames(births.age.sex)$year){
            rv = array(births.age.sex[year,,],
                       dim = sapply(state.dim.names, length),
                       dimnames = state.dim.names)
            
            parameters = add.time.varying.parameter.value(parameters,
                                                          parameter.name='FERTILITY.RATES',
                                                          value = rv,
                                                          time = as.numeric(year))
        }
    }
    
    
    parameters = add.time.varying.parameter.value(parameters,
                                                  parameter.name='MALE.BIRTHS',
                                                  value = 0.5,
                                                  time = 2000)
    
    #-- AGING --#
    
    base.aging.rates = array((1/parameters$AGE.SPANS),
                        dim=sapply(state.dim.names, length),
                        dimnames=state.dim.names)
    
    # allow 15-19 and 20-24 to have different/higher base aging rates,
    # because we assume most of the HIV infections in this age group are among those on the older side who are sexually active
    # (and therefore will increase the proportion aging out )
    base.aging.rates["15-19",,,] = sampled.parameters["age.15.to.19.base.aging.rate"] 
    base.aging.rates["20-24",,,] = sampled.parameters["age.20.to.24.base.aging.rate"]
    
    get.aging.rates = function(times,
                               base.rate, # base aging rate, i.e., 0.2
                               factor, # factor to either divide or multiply the aging rate by
                               pre.time, # a few years before the low time
                               low.time, # when aging rate in that age span is lowest; i.e., when bulk of the age span is youngest and not aging out
                               high.time, # when aging rate in that age span is highest; i.e., when bulk of the age span is oldest and aging out
                               post.time) # a few years after the high time 
    {
        rv = spline(x=c(pre.time, low.time, high.time, post.time),
                    y=c(base.rate, base.rate/factor, base.rate*factor, base.rate),
                    method='natural',
                    xout = times)$y
        rv[times<pre.time | times>post.time] = base.rate
        rv
    }
    
    aging.years=1980:2030
    low.time.for.age = c("15-19" = 1992,"20-24" = 1996,"25-29" = 2000,"30-34" = 2004,"35-39" = 2008,"40-44" = 2012,
                         "45-49" = 2016,"50-54" = 2020,"55-59" = 2024,"60-64" = 2028,"65-69" = 2032,"70-74" = 2036,
                         "75-79" = 2040)
    
    age.brackets.to.update = parameters$AGES[c(-1,-2,-3)]
    age.brackets.to.update = age.brackets.to.update[-length(age.brackets.to.update)] # remove 80 and over
    
    rates.per.age = lapply(age.brackets.to.update, function(age){
        age.span=5
        low.time = low.time.for.age[age]
        high.time = low.time + age.span-1 
        pre.time = low.time-3
        post.time = high.time + 3
        
        # Get base rate
        base.rate = 1/age.span
        if (age=='15-19')
            base.rate = sampled.parameters['age.15.to.19.base.aging.rate']
        else if (age=='20-24')
            base.rate = sampled.parameters['age.20.to.24.base.aging.rate']
        
        # Get factor by age
        factor=2
        age.25.to.50.age.brackets = get.age.brackets.in.range(lower = 25, 
                                                              upper = 50) 
        over.50.age.brackets = get.age.brackets.in.range(lower = 50, 
                                                         upper = Inf) 
        if(age=="15-19")
            factor=sampled.parameters['age.15.to.19.aging.factor']
        else if (age=="20-24")
            factor=sampled.parameters['age.20.to.24.aging.factor']
        else if (age %in% age.25.to.50.age.brackets)
            factor=sampled.parameters['age.25.to.50.aging.factor']
        else if (age %in% over.50.age.brackets)
            factor=sampled.parameters['over.50.aging.factor']

        get.aging.rates(times=aging.years,
                        base.rate=base.rate,
                        factor=factor,
                        pre.time=pre.time,
                        low.time=low.time,
                        high.time=high.time,
                        post.time=post.time)
    })
    
    for (year in 1:length(aging.years))
    {
        aging.rates = base.aging.rates
        for (age in 1:length(age.brackets.to.update)){
            age.name = age.brackets.to.update[age]
            aging.rates[age.name,,,] = rates.per.age[[age]][year]
        }
        
        parameters = add.time.varying.parameter.value(parameters,
                                                      parameter.name='AGING.RATES',
                                                      value = aging.rates,
                                                      time = year)
    }
    
    
    #-- MORTALITY --#
    ## HIV MORTALITY ## 
    age.0.to.14.age.brackets = get.age.brackets.in.range(lower = 0, 
                                                         upper = 15) 
    age.15.to.24.age.brackets = get.age.brackets.in.range(lower = 15, 
                                                         upper = 25) 
    over.50.age.brackets = get.age.brackets.in.range(lower = 50, 
                                                    upper = Inf) 
    # Set up initial HIV mortality rates 
    HIV.MORTALITY.RATES.0=array(0, #assume 0 for non-hiv states
                              dim=sapply(state.dim.names, length),
                              dimnames=state.dim.names)
    HIV.MORTALITY.RATES.2 = HIV.MORTALITY.RATES.1 = HIV.MORTALITY.RATES.0
    
    # Set up time-specific HIV mortality rates 
    HIV.MORTALITY.RATES.0[,,,c('undiagnosed', 'diagnosed_unengaged', 'engaged_unsuppressed')] = 
        sampled.parameters['hiv.specific.mortality.rates.0']
    HIV.MORTALITY.RATES.0[,"male",,] = HIV.MORTALITY.RATES.0[,"male",,]*sampled.parameters["male.hiv.mortality.multiplier.0"]
    
    HIV.MORTALITY.RATES.1[,,,c('undiagnosed', 'diagnosed_unengaged', 'engaged_unsuppressed')] = 
        sampled.parameters['hiv.specific.mortality.rates.1']
    HIV.MORTALITY.RATES.1[,"male",,] = HIV.MORTALITY.RATES.1[,"male",,]*sampled.parameters["male.hiv.mortality.multiplier.1"]
    
    HIV.MORTALITY.RATES.2[,,,c('undiagnosed', 'diagnosed_unengaged', 'engaged_unsuppressed')] = 
        sampled.parameters['hiv.specific.mortality.rates.2']
    HIV.MORTALITY.RATES.2[,"male",,] = HIV.MORTALITY.RATES.2[,"male",,]*sampled.parameters["male.hiv.mortality.multiplier.2"]
    
    # Set up age- and time-specific HIV mortality multipliers 
    HIV.MORTALITY.RATES.0[age.15.to.24.age.brackets,,,] = HIV.MORTALITY.RATES.0[age.15.to.24.age.brackets,,,]*
        sampled.parameters["age.15.to.24.hiv.mortality.multiplier.0"]
    HIV.MORTALITY.RATES.1[age.15.to.24.age.brackets,,,] = HIV.MORTALITY.RATES.1[age.15.to.24.age.brackets,,,]*
        sampled.parameters["age.15.to.24.hiv.mortality.multiplier.1"]
    HIV.MORTALITY.RATES.2[age.15.to.24.age.brackets,,,] = HIV.MORTALITY.RATES.2[age.15.to.24.age.brackets,,,]*
        sampled.parameters["age.15.to.24.hiv.mortality.multiplier.2"]
    
    HIV.MORTALITY.RATES.0[over.50.age.brackets,,,] = HIV.MORTALITY.RATES.0[over.50.age.brackets,,,]*
        sampled.parameters["over.50.hiv.mortality.multiplier.0"]
    HIV.MORTALITY.RATES.1[over.50.age.brackets,,,] = HIV.MORTALITY.RATES.1[over.50.age.brackets,,,]*
        sampled.parameters["over.50.hiv.mortality.multiplier.1"]
    HIV.MORTALITY.RATES.2[over.50.age.brackets,,,] = HIV.MORTALITY.RATES.2[over.50.age.brackets,,,]*
        sampled.parameters["over.50.hiv.mortality.multiplier.2"]
    
    HIV.MORTALITY.RATES.0[age.0.to.14.age.brackets,,,] = HIV.MORTALITY.RATES.0[age.0.to.14.age.brackets,,,]*
        sampled.parameters["age.0.to.14.hiv.mortality.multiplier.0"]
    HIV.MORTALITY.RATES.1[age.0.to.14.age.brackets,,,] = HIV.MORTALITY.RATES.1[age.0.to.14.age.brackets,,,]*
        sampled.parameters["age.0.to.14.hiv.mortality.multiplier.1"]
    HIV.MORTALITY.RATES.2[age.0.to.14.age.brackets,,,] = HIV.MORTALITY.RATES.2[age.0.to.14.age.brackets,,,]*
        sampled.parameters["age.0.to.14.hiv.mortality.multiplier.2"]
    
    parameters = add.time.varying.parameter.value(parameters,
                                                  parameter.name='HIV.MORTALITY.RATES',
                                                  value = HIV.MORTALITY.RATES.0,
                                                  time = sampled.parameters['hiv.mortality.time.0'])
    parameters = add.time.varying.parameter.value(parameters,
                                                  parameter.name='HIV.MORTALITY.RATES',
                                                  value = HIV.MORTALITY.RATES.1,
                                                  time = sampled.parameters['hiv.mortality.time.1'])
    parameters = add.time.varying.parameter.value(parameters,
                                                  parameter.name='HIV.MORTALITY.RATES',
                                                  value = HIV.MORTALITY.RATES.2,
                                                  time = sampled.parameters['hiv.mortality.time.2'])
    
    
    
    ## NON-HIV MORTALITY ## 
    deaths.age.sex = calculate.all.death.rates(data.manager = DATA.MANAGER, 
                                               keep.dimensions = c('year','sex','age'), 
                                               model.age.cutoffs = MODEL.AGE.CUTOFFS)
    
    #setting up code to smooth/project death rate into future 
    anchor.year = 1980
    years = as.numeric(dimnames(deaths.age.sex)$year) - anchor.year
    years.label = as.numeric(dimnames(deaths.age.sex)$year) # for plotting
    
    project.years = (max(years)+1):(project.to.year-anchor.year)-max(years)
    project.years = project.years[project.years%%5==0] # only including multiples of 5
    desired.years = c(years,max(years)+project.years) # future years to predict
    smoothed.years.label = desired.years + anchor.year # for plotting
    mask = rep(T,length(years)) # use this to remove years
    mask = years.label<1987 
    
    mortality.intercepts.slopes.age.sex = apply(deaths.age.sex,c('age','sex'),function(rates){
        
        fit = lm(log(rates[mask]) ~ years[mask])
        
        rv = fit$coefficients
        names(rv) = c("intercept","slope")
        
        rv
    })

    age.45.to.65.age.brackets = get.age.brackets.in.range(lower = 45, 
                                                     upper = 65) 
    over.65.age.brackets = get.age.brackets.in.range(lower = 65, 
                                                     upper = Inf) 
    
    mortality.intercepts.slopes.age.sex["intercept",age.45.to.65.age.brackets,] = mortality.intercepts.slopes.age.sex["intercept",age.45.to.65.age.brackets,] +
        log(sampled.parameters['age.45.to.65.mortality.intercept.multiplier'])
    
    mortality.intercepts.slopes.age.sex["slope",age.45.to.65.age.brackets,] = mortality.intercepts.slopes.age.sex["slope",age.45.to.65.age.brackets,] +
        log(sampled.parameters['age.45.to.65.mortality.slope.multiplier'])
    
    mortality.intercepts.slopes.age.sex["intercept",over.65.age.brackets,] = mortality.intercepts.slopes.age.sex["intercept",over.65.age.brackets,] +
        log(sampled.parameters['over.65.mortality.intercept.multiplier'])
    
    mortality.intercepts.slopes.age.sex["slope",over.65.age.brackets,] = mortality.intercepts.slopes.age.sex["slope",over.65.age.brackets,] +
        log(sampled.parameters['over.65.mortality.slope.multiplier'])
    
    # Smoothed non-HIV mortality: fit regression on desired years only (using mask above)
    smooth.deaths.age.sex = apply(mortality.intercepts.slopes.age.sex,c('age','sex'),function(intercept.slope){
        
        exp(intercept.slope[1] + intercept.slope[2]*desired.years) #gives projections; exponentiate if log
        
    })
    dim.names = list(year = smoothed.years.label,
                     age = dimnames(smooth.deaths.age.sex)[2]$age,
                     sex = dimnames(smooth.deaths.age.sex)[3]$sex)
    dim(smooth.deaths.age.sex) = sapply(dim.names, length)
    dimnames(smooth.deaths.age.sex) = dim.names
    
    for (year in dimnames(smooth.deaths.age.sex)$year){
        rv = array(smooth.deaths.age.sex[year,,],
                   dim = sapply(state.dim.names, length),
                   dimnames = state.dim.names)
        
        parameters = add.time.varying.parameter.value(parameters,
                                                      parameter.name='NON.HIV.MORTALITY.RATES',
                                                      value = rv,
                                                      time = as.numeric(year))
    }
    
    
    
    #-- DIAGNOSES --#
    # Set testing to 0 at start.time 
    parameters = add.time.varying.parameter.value(parameters,
                                                  parameter.name='TESTING.RATES',
                                                  value = 0,
                                                  time = (sampled.parameters['start.time']))
    
    # Set testing based on projection
    testing.years.to.project = c(1976:2030)
    for(year in testing.years.to.project){
        projected.log.odds = (TESTING.MODEL$intercepts+sampled.parameters['log.OR.testing.intercept'])+
            ((TESTING.MODEL$slopes+sampled.parameters['log.OR.testing.slope'])*(year-TESTING.MODEL$anchor.year))
        
        projected.p = 1/(1+exp(-projected.log.odds))
        
        projected.p = projected.p*TESTING.MODEL$max.proportion
        
        projected.rate = -log(1-projected.p)
        projected.rate[,"male",] = projected.rate[,"male",]*sampled.parameters["male.awareness.multiplier"]
        
        parameters = add.time.varying.parameter.value(parameters,
                                                      parameter.name='TESTING.RATES',
                                                      value = projected.rate,
                                                      time = year)
        
    }
    
    #-- NEW INFECTIONS --#
    transmission.dim.names = list(age.to=parameters$AGES, 
                                  sex.to=parameters$SEXES,
                                  subgroup.to=parameters$SUBGROUPS,
                                  age.from=parameters$AGES, 
                                  sex.from=parameters$SEXES,
                                  subgroup.from=parameters$SUBGROUPS)
    
    #previous global transmission rate
    if(1==2){
        transmission.rates = array(sampled.parameters['global.transmission.rate']/n.trans.states,
                                   dim=sapply(transmission.dim.names, length),
                                   dimnames=transmission.dim.names)
        dim(transmission.rates) = c(n.trans.states, n.trans.states)
        transmission.rates = as.matrix(transmission.rates)
        
        parameters = add.time.varying.parameter.value(parameters,
                                                      parameter.name='TRANSMISSION.RATES',
                                                      value = transmission.rates,
                                                      time = 2000)
    }
    
    
    # Mixing proportions: array where every combo of age.to, sex.to sums to 1 (capturing all of their partners)
    mixing.proportions.0 = sapply(parameters$SEXES, function(sex.to){
        sapply(parameters$AGES, function(age.to){
            
            # for this age bracket/sex.to, what proportion of that sexes partners are in the other sexes
            sex.proportions = get.sex.mixing.proportions(sex.to = sex.to,
                                                         age.to=age.to,
                                                         sexes=parameters$SEXES,
                                                         sampled.parameters = sampled.parameters)      
            
            # then, for that sex-sex combination what proportion are in each age group
            sapply(parameters$SEXES, function(sex.from){
                sex.proportions[sex.from]*get.age.mixing.proportions(parameters=parameters,
                                                                     sex.to=sex.to,
                                                                     age.to=age.to,
                                                                     sex.from=sex.from,
                                                                     ages=parameters$AGES,
                                                                     sampled.parameters=sampled.parameters)
            })
        })
    })
    
    dim(mixing.proportions.0) = c(n.trans.states, n.trans.states)
    mixing.proportions.0 = t(mixing.proportions.0)
    dim(mixing.proportions.0) = sapply(transmission.dim.names, length)
    dimnames(mixing.proportions.0) = transmission.dim.names
    
    
    # Set transmission to 0 until start.time (setting at 1980 for now)
    parameters = add.time.varying.parameter.value(parameters,
                                                  parameter.name='TRANSMISSION.RATES',
                                                  value = 0,
                                                  time = (sampled.parameters['start.time']-0.001))
    
    
    # Set transmission rate to a high level (trate.0) at start.time, don't use age multipliers
    transmission.rates.0 = make.transmission.array(parameters = parameters,
                                                   global.trate = sampled.parameters["trate.0"],
                                                   male.to.male.multiplier = sampled.parameters["male.to.male.multiplier"], 
                                                   female.to.male.multiplier = sampled.parameters["female.to.male.multiplier"],
                                                   female.age.multipliers =c(rep(0,length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 0, upper = 15))),
                                                                             rep(sampled.parameters["age.15.to.19.transmission.multiplier.0"],
                                                                                 length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 15, upper = 20))),
                                                                             rep(sampled.parameters["age.20.to.29.transmission.multiplier.0"],
                                                                                 length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 20, upper = 30))),
                                                                             rep(1,length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 30, upper = 40))),
                                                                             rep(sampled.parameters["age.40.to.49.transmission.multiplier.0"],
                                                                                 length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 40, upper = 50))),
                                                                             rep(sampled.parameters["age.50.and.over.transmission.multiplier.0"],
                                                                                 length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 50, upper = Inf)))),
                                                   male.age.multipliers =c(rep(0,length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 0, upper = 15))),
                                                                           rep(sampled.parameters["age.15.to.19.transmission.multiplier.0"],
                                                                               length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 15, upper = 20))),
                                                                           rep(sampled.parameters["age.20.to.29.transmission.multiplier.0"],
                                                                               length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 20, upper = 30))),
                                                                           rep(1,length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 30, upper = 40))),
                                                                           rep(sampled.parameters["age.40.to.49.transmission.multiplier.0"],
                                                                               length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 40, upper = 50))),
                                                                           rep(sampled.parameters["age.50.and.over.transmission.multiplier.0"],
                                                                               length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 50, upper = Inf)))))

    transmission.rates.0=transmission.rates.0*mixing.proportions.0
    
    dim(transmission.rates.0) = c(n.trans.states, n.trans.states)
    transmission.rates.0 = as.matrix(transmission.rates.0)
    
    
    parameters = add.time.varying.parameter.value(parameters,
                                                  parameter.name='TRANSMISSION.RATES',
                                                  value = transmission.rates.0,
                                                  time = sampled.parameters['start.time'])
    
    # End high transmission rate at time.0 (1990 for now) - have to set it again so that this is the year it interpolates from
    parameters = add.time.varying.parameter.value(parameters,
                                                  parameter.name='TRANSMISSION.RATES',
                                                  value = transmission.rates.0,
                                                  time = sampled.parameters['time.0'])
    
    
    # Set transmission rate to a lower level (trate.1) at time.1, use 2003 multipliers
    transmission.rates.1 = make.transmission.array(parameters = parameters,
                                                   global.trate = sampled.parameters["trate.1"],
                                                   male.to.male.multiplier = sampled.parameters["male.to.male.multiplier"], 
                                                   female.to.male.multiplier = sampled.parameters["female.to.male.multiplier"],
                                                   female.age.multipliers =c(rep(0,length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 0, upper = 15))),
                                                                             rep(sampled.parameters["age.15.to.19.transmission.multiplier.1"],
                                                                                 length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 15, upper = 20))),
                                                                             rep(sampled.parameters["age.20.to.29.transmission.multiplier.1"],
                                                                                 length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 20, upper = 30))),
                                                                             rep(1,length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 30, upper = 40))),
                                                                             rep(sampled.parameters["age.40.to.49.transmission.multiplier.1"],
                                                                                 length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 40, upper = 50))),
                                                                             rep(sampled.parameters["age.50.and.over.transmission.multiplier.1"],
                                                                                 length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 50, upper = Inf))))*
                                                       FEMALE.AGE.MULTIPLIERS.2003,
                                                   male.age.multipliers =c(rep(0,length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 0, upper = 15))),
                                                                           rep(sampled.parameters["age.15.to.19.transmission.multiplier.1"],
                                                                               length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 15, upper = 20))),
                                                                           rep(sampled.parameters["age.20.to.29.transmission.multiplier.1"],
                                                                               length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 20, upper = 30))),
                                                                           rep(1,length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 30, upper = 40))),
                                                                           rep(sampled.parameters["age.40.to.49.transmission.multiplier.1"],
                                                                               length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 40, upper = 50))),
                                                                           rep(sampled.parameters["age.50.and.over.transmission.multiplier.1"],
                                                                               length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 50, upper = Inf))))*
                                                       MALE.AGE.MULTIPLIERS.2003)
    
    mixing.proportions.1 = mixing.proportions.0 # keep this the same for now
    transmission.rates.1=transmission.rates.1*mixing.proportions.1
    
    dim(transmission.rates.1) = c(n.trans.states, n.trans.states)
    transmission.rates.1 = as.matrix(transmission.rates.1)
    
    
    parameters = add.time.varying.parameter.value(parameters,
                                                  parameter.name='TRANSMISSION.RATES',
                                                  value = transmission.rates.1,
                                                  time = sampled.parameters['time.1'])
    
    # Set transmission rate to another level (trate.2) at time.2
    # this is actually the same as trate.1, just using this spline point for a new age multiplier (2008)
    transmission.rates.2 = make.transmission.array(parameters = parameters,
                                                   global.trate = sampled.parameters["trate.2"],
                                                   male.to.male.multiplier = sampled.parameters["male.to.male.multiplier"], 
                                                   female.to.male.multiplier = sampled.parameters["female.to.male.multiplier"],
                                                   female.age.multipliers =c(rep(0,length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 0, upper = 15))),
                                                                             rep(sampled.parameters["age.15.to.19.transmission.multiplier.2"],
                                                                                 length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 15, upper = 20))),
                                                                             rep(sampled.parameters["age.20.to.29.transmission.multiplier.2"],
                                                                                 length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 20, upper = 30))),
                                                                             rep(1,length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 30, upper = 40))),
                                                                             rep(sampled.parameters["age.40.to.49.transmission.multiplier.2"],
                                                                                 length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 40, upper = 50))),
                                                                             rep(sampled.parameters["age.50.and.over.transmission.multiplier.2"],
                                                                                 length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 50, upper = Inf))))*
                                                       FEMALE.AGE.MULTIPLIERS.2008,
                                                   male.age.multipliers =c(rep(0,length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 0, upper = 15))),
                                                                           rep(sampled.parameters["age.15.to.19.transmission.multiplier.2"],
                                                                               length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 15, upper = 20))),
                                                                           rep(sampled.parameters["age.20.to.29.transmission.multiplier.2"],
                                                                               length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 20, upper = 30))),
                                                                           rep(1,length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 30, upper = 40))),
                                                                           rep(sampled.parameters["age.40.to.49.transmission.multiplier.2"],
                                                                               length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 40, upper = 50))),
                                                                           rep(sampled.parameters["age.50.and.over.transmission.multiplier.2"],
                                                                               length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 50, upper = Inf))))*
                                                       MALE.AGE.MULTIPLIERS.2008)
    
    mixing.proportions.2 = mixing.proportions.0 # keep this the same for now
    transmission.rates.2=transmission.rates.2*mixing.proportions.2
    
    dim(transmission.rates.2) = c(n.trans.states, n.trans.states)
    transmission.rates.2 = as.matrix(transmission.rates.2)
    
    
    parameters = add.time.varying.parameter.value(parameters,
                                                  parameter.name='TRANSMISSION.RATES',
                                                  value = transmission.rates.2,
                                                  time = sampled.parameters['time.2'])
    
    # Set transmission rate to another level (trate.3) at time.3, use 2014 age multipliers
    transmission.rates.3 = make.transmission.array(parameters = parameters,
                                                   global.trate = sampled.parameters["trate.3"],
                                                   male.to.male.multiplier = sampled.parameters["male.to.male.multiplier"], 
                                                   female.to.male.multiplier = sampled.parameters["female.to.male.multiplier"],
                                                   female.age.multipliers =c(rep(0,length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 0, upper = 15))),
                                                                             rep(sampled.parameters["age.15.to.19.transmission.multiplier.3"],
                                                                                 length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 15, upper = 20))),
                                                                             rep(sampled.parameters["age.20.to.29.transmission.multiplier.3"],
                                                                                 length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 20, upper = 30))),
                                                                             rep(1,length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 30, upper = 40))),
                                                                             rep(sampled.parameters["age.40.to.49.transmission.multiplier.3"],
                                                                                 length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 40, upper = 50))),
                                                                             rep(sampled.parameters["age.50.and.over.transmission.multiplier.3"],
                                                                                 length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 50, upper = Inf))))*
                                                       FEMALE.AGE.MULTIPLIERS.2014,
                                                   male.age.multipliers =c(rep(0,length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 0, upper = 15))),
                                                                           rep(sampled.parameters["age.15.to.19.transmission.multiplier.3"],
                                                                               length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 15, upper = 20))),
                                                                           rep(sampled.parameters["age.20.to.29.transmission.multiplier.3"],
                                                                               length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 20, upper = 30))),
                                                                           rep(1,length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 30, upper = 40))),
                                                                           rep(sampled.parameters["age.40.to.49.transmission.multiplier.3"],
                                                                               length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 40, upper = 50))),
                                                                           rep(sampled.parameters["age.50.and.over.transmission.multiplier.3"],
                                                                               length(get.age.brackets.in.range(age.cutoffs = age.cutoffs, lower = 50, upper = Inf))))*
                                                       MALE.AGE.MULTIPLIERS.2014)
    
    mixing.proportions.3 = mixing.proportions.0 # keep this the same for now
    transmission.rates.3=transmission.rates.3*mixing.proportions.3
    
    dim(transmission.rates.3) = c(n.trans.states, n.trans.states)
    transmission.rates.3 = as.matrix(transmission.rates.3)
    
    parameters = add.time.varying.parameter.value(parameters,
                                                  parameter.name='TRANSMISSION.RATES',
                                                  value = transmission.rates.3,
                                                  time = sampled.parameters['time.3'])
    
    infectiousness.h = array(0,
                             dim=sapply(state.dim.names, length),
                             dimnames=state.dim.names)
    
    infectiousness.h[,,,'undiagnosed'] = 1
    infectiousness.h[,,,'diagnosed_unengaged'] = sampled.parameters['relative.transmission.from.diagnosis']
    infectiousness.h[,,,'engaged_unsuppressed'] = sampled.parameters['relative.transmission.from.diagnosis']
    
    parameters = add.time.varying.parameter.value(parameters,
                                                  parameter.name='INFECTIOUSNESS.H',
                                                  value = infectiousness.h,
                                                  time = 2000)
    
    # New HIV births
    maternal.fetal.transmission.0 = array(0,
                                        dim=sapply(state.dim.names, length),
                                        dimnames=state.dim.names)
    
    maternal.fetal.transmission.1 = maternal.fetal.transmission.0
    
    maternal.fetal.transmission.0[,,,c('undiagnosed','diagnosed_unengaged','engaged_unsuppressed')] = sampled.parameters['birth.transmission.risk.0']
    maternal.fetal.transmission.1[,,,c('undiagnosed','diagnosed_unengaged','engaged_unsuppressed')] = sampled.parameters['birth.transmission.risk.1']
    
    parameters = add.time.varying.parameter.value(parameters,
                                                  parameter.name='MATERNAL.FETAL.TRANSMISSION',
                                                  value = maternal.fetal.transmission.0,
                                                  time = sampled.parameters['birth.transmission.time.0'])
    
    parameters = add.time.varying.parameter.value(parameters,
                                                  parameter.name='MATERNAL.FETAL.TRANSMISSION',
                                                  value = maternal.fetal.transmission.1,
                                                  time = sampled.parameters['birth.transmission.time.1'])
    
    
    #-- ENGAGEMENT/DISENGAGEMENT --#
    engagement.years.to.project = c(1975:2030)
    
    for(year in engagement.years.to.project){
        if(year<2016 | year>2017){
            projected.log.odds = (ENGAGEMENT.MODEL$intercept+sampled.parameters['log.OR.engagement.intercept'])+
                ((ENGAGEMENT.MODEL$pre.universal.slope+sampled.parameters['log.OR.engagement.pre.universal.slope'])*(year-ENGAGEMENT.MODEL$anchor.year))+
                ((ENGAGEMENT.MODEL$post.universal.slope+sampled.parameters['log.OR.engagement.post.universal.slope'])*pmax(0,(year-2015)))
        } else if(year==2016 | year==2017){
            projected.log.odds = (ENGAGEMENT.MODEL$intercept+sampled.parameters['log.OR.engagement.intercept'])+
                ((ENGAGEMENT.MODEL$pre.universal.slope+sampled.parameters['log.OR.engagement.pre.universal.slope'])*(year-ENGAGEMENT.MODEL$anchor.year))+
                ((ENGAGEMENT.MODEL$intermediate.slope.2016.2017+sampled.parameters['log.OR.engagement.intermediate.slope'])*pmax(0,(year-2015)))+
                ((ENGAGEMENT.MODEL$post.universal.slope+sampled.parameters['log.OR.engagement.post.universal.slope'])*pmax(0,(year-2015)))
        }
        projected.p = 1/(1+exp(-projected.log.odds)) # didn't do a max proportion here - okay? 
        projected.rate = -log(1-projected.p)
        
        projected.rate.age.sex = array(projected.rate,
                                       dim=sapply(trans.dim.names, length),
                                       dimnames=trans.dim.names)
        
        
        projected.rate.age.sex[,"male",] = projected.rate.age.sex[,"male",]*sampled.parameters["male.engagement.multiplier"]
        
        if(any(is.infinite(projected.rate.age.sex)))
            browser()
        
        parameters = add.time.varying.parameter.value(parameters,
                                                      parameter.name='ENGAGEMENT.RATES',
                                                      value = projected.rate.age.sex,
                                                      time = year)
        
    }
    
    parameters = add.time.varying.parameter.value(parameters,
                                                  parameter.name='UNSUPPRESSED.DISENGAGEMENT.RATES',
                                                  value = array(sampled.parameters['unsuppressed.disengagement.rates'],
                                                                dim=sapply(trans.dim.names, length),
                                                                dimnames=trans.dim.names),
                                                  time = 2000)
    
    parameters = add.time.varying.parameter.value(parameters,
                                                  parameter.name='SUPPRESSED.DISENGAGEMENT.RATES',
                                                  value = array(sampled.parameters['suppressed.disengagement.rates'],
                                                                dim=sapply(trans.dim.names, length),
                                                                dimnames=trans.dim.names),
                                                  time = 2000)
    
    if(1==2){# Before 2014, initiate at CD4 <350
        engagement.rates.0 = array(sampled.parameters['engagement.rate.0'],
                                   dim=sapply(trans.dim.names, length),
                                   dimnames=trans.dim.names)
        engagement.rates.0[,"male",] = engagement.rates.0[,"male",]*sampled.parameters["male.engagement.multiplier"]
        
        parameters = add.time.varying.parameter.value(parameters,
                                                      parameter.name='ENGAGEMENT.RATES',
                                                      value = engagement.rates.0,
                                                      time = sampled.parameters['engagement.time.0'])
        
        # 2014-2016, initiate at CD4 <500
        engagement.rates.1 = array(sampled.parameters['engagement.rate.1'],
                                   dim=sapply(trans.dim.names, length),
                                   dimnames=trans.dim.names)
        engagement.rates.1[,"male",] = engagement.rates.1[,"male",]*sampled.parameters["male.engagement.multiplier"]
        parameters = add.time.varying.parameter.value(parameters,
                                                      parameter.name='ENGAGEMENT.RATES',
                                                      value = engagement.rates.1,
                                                      time = sampled.parameters['engagement.time.1'])
        
        # Initiation of test and treat 
        engagement.rates.2 = array(sampled.parameters['engagement.rate.2'],
                                   dim=sapply(trans.dim.names, length),
                                   dimnames=trans.dim.names)
        engagement.rates.2[,"male",] = engagement.rates.2[,"male",]*sampled.parameters["male.engagement.multiplier"]
        parameters = add.time.varying.parameter.value(parameters,
                                                      parameter.name='ENGAGEMENT.RATES',
                                                      value = engagement.rates.2,
                                                      time = sampled.parameters['engagement.time.2'])
        
}
    
    
    #-- SUPPRESSION/UNSUPPRESSION --#
    # Set suppression to 0 before ART
    parameters = add.time.varying.parameter.value(parameters,
                                                  parameter.name='SUPPRESSION.RATES',
                                                  value = 0,
                                                  time = (sampled.parameters['suppression.time.0']-0.001))
    
    # First ART available; slower to suppression
    suppression.rates.0 = array(sampled.parameters['suppression.rate.0'],
                               dim=sapply(trans.dim.names, length),
                               dimnames=trans.dim.names)
    suppression.rates.0[,"male",] = suppression.rates.0[,"male",]*sampled.parameters["male.suppression.multiplier"]
    parameters = add.time.varying.parameter.value(parameters,
                                                  parameter.name='SUPPRESSION.RATES',
                                                  value = suppression.rates.0,
                                                  time = sampled.parameters['suppression.time.0'])
    
    # More effective ART available
    suppression.rates.1 = array(sampled.parameters['suppression.rate.1'],
                                dim=sapply(trans.dim.names, length),
                                dimnames=trans.dim.names)
    suppression.rates.1[,"male",] = suppression.rates.1[,"male",]*sampled.parameters["male.suppression.multiplier"]
    parameters = add.time.varying.parameter.value(parameters,
                                                  parameter.name='SUPPRESSION.RATES',
                                                  value = suppression.rates.1,
                                                  time = sampled.parameters['suppression.time.1'])
     
    
    
    parameters = add.time.varying.parameter.value(parameters,
                                                  parameter.name='UNSUPPRESSION.RATES',
                                                  value = array(sampled.parameters['unsuppression.rates'],
                                                                dim=sapply(trans.dim.names, length),
                                                                dimnames=trans.dim.names),
                                                  time = 2000)
    
    
    
    
    
    
    
    #-- RETURN --#
    
    parameters
}


#-- TIME VARYING PARAMETERS --#
# parameters$time.varying.parameters: list of time varying parameters

# Adds a time point and a value to the spline for a parameter; added to parameters$time.varying.parameters;
# cannot have multiple entries for the same time
add.time.varying.parameter.value <- function(parameters,
                                             parameter.name,
                                             time,
                                             value){
    # Get or make the entry
    if (is.null(parameters$time.varying.parameters[[parameter.name]]))
        param = list(
            times = numeric(),
            values = list()
        )
    else
        param = parameters$time.varying.parameters[[parameter.name]]
    
    # Check that the time is not already in there
    if (any(param$times==time))
        stop(paste0("The time ", time, " has already been entered as a spline point for parameter '", parameter.name, "'"))
    
    # Append the new values
    param$times = c(param$times, time)
    param$values = c(param$values, list(value)) 
    
    # Sort by time
    o = order(param$times)
    param$times = param$times[o]
    param$values = param$values[o]
    
    # Plug it in and return
    parameters$time.varying.parameters[[parameter.name]] = param
    parameters
}


# Computes the parameter value at a specific time; called at beginning of compute.dx function in diffeq code; 
# applied across all parameters$time.varying.parameters
compute.time.varying.parameters <- function(parameters, time){
    lapply(parameters$time.varying.parameters, function(params){
        # params is a list with two components
        # $times - a vector of times
        # $values - a list of values
        # assume these are ordered
        
        n.times = length(params$times)
        
        if (time <= params$times[1])
            params$values[[1]] #return first value
        else if (time >= params$times[n.times])
            params$values[[n.times]] #return last value
        else 
        {#we need to interpolate (linearly)
            index1 = (1:n.times)[params$times<=time] 
            index1 = index1[length(index1)] #find the index of last 'time' entry which is <= selected time
            index2 = index1 + 1 #the index of the first 'time' entry which is > selected time
            
            if (time==params$times[index1])
                params$values[[index1]] #return the value
            else #linear interpolation:
            {
                time1 = params$times[index1]
                time2 = params$times[index2]
                
                value1 = params$values[[index1]]
                value2 = params$values[[index2]]
                
                slope = (value2 - value1) / (time2 - time1)
                value1 + slope * (time - time1) #final value that is returned
            }
        }
        
    })
}



##---------------------------##
#-- OTHER/HELPER FUNCTIONS --#
##---------------------------##

# Calculates death rates for correct model age stratifications, based on surveillance data
calculate.all.death.rates = function(data.manager,
                                     keep.dimensions = c('year','age','sex'),
                                     model.age.cutoffs){
    
    POPULATION.AGE.MAPPING = map.population.ages(data.manager = data.manager,
                                                 data.type = "population.full",
                                                 model.age.cutoffs = model.age.cutoffs)
    
    years.by.five = data.manager$deaths$YEARS
    deaths.ages = data.manager$deaths$AGES
    deaths.ages.rev = c(deaths.ages[-length(deaths.ages)],"95-99","100 and over")
    deaths.sexes = data.manager$deaths$SEXES
    start.years = as.numeric(substr(years.by.five,1,4))
    end.years = as.numeric(substr(years.by.five,8,11))
    mid.years = (start.years + (end.years-1))/2
    
    age.dim.names = list(year = years.by.five,
                         age = deaths.ages.rev)
    
    full.dim.names = list(year = years.by.five,
                          age = deaths.ages.rev,
                          sex = deaths.sexes)
    
    ## Pull deaths
    deaths = get.surveillance.data(data.manager = data.manager,
                                   data.type = "deaths",
                                   years = years.by.five,
                                   keep.dimensions = keep.dimensions)
    
    ## Pull population
    pop = get.surveillance.data(data.manager = data.manager,
                                data.type = "population.full",
                                years = 1950:2020,
                                keep.dimensions = keep.dimensions) 
    
    # Adding '100 and over' age group to deaths (set to 0) 
    if (setequal(keep.dimensions, c('year','age'))){
        deaths.100 = rep(0, length(years.by.five))
        deaths = cbind(deaths,deaths.100)
        dimnames(deaths) = age.dim.names
        
    }
    if (setequal(keep.dimensions, c('year','age','sex'))){
        deaths.100 = rep(0, length(years.by.five))
        male = deaths[,,"male"]
        female = deaths[,,"female"]
        male = cbind(male,deaths.100)
        dimnames(male) = age.dim.names
        female = cbind(female,deaths.100)
        dimnames(female) = age.dim.names
        deaths = array(c(male,female),
                       dim = sapply(full.dim.names, length),
                       dimnames = full.dim.names)
    } 
    
    ## Dim names
    if(length(keep.dimensions)==1){
        death.rate.dim.names = list(year = years.by.five)
    }
    
    if (setequal(keep.dimensions, c('year','age'))){
        death.rate.dim.names = age.dim.names
    }
    
    if (setequal(keep.dimensions, c('year','sex'))){
        death.rate.dim.names = list(year = years.by.five,
                                    sex = deaths.sexes)
    }
    
    if (setequal(keep.dimensions, c('year','age','sex'))){
        death.rate.dim.names = full.dim.names
    }
    
    ## Aggregate population for every five years (e.g., 1950-1954)
    five.year.age.groups = array(0,
                                 dim = sapply(death.rate.dim.names, length),
                                 dimnames = death.rate.dim.names)
    
    if(length(keep.dimensions)==1){
        for (i in 1:length(start.years)){
            five.year.age.groups[i] = sum(pop[(i*5-4):(i*5)])
        }
    }
    
    if (setequal(keep.dimensions, c('year','age')) | setequal(keep.dimensions, c('year','sex')) ){
        for (i in 1:length(start.years)){
            five.year.age.groups[i,] = colSums(pop[(i*5-4):(i*5),])
        }
    }
    
    if (setequal(keep.dimensions, c('year','age','sex'))){
        for (i in 1:length(start.years)){
            five.year.age.groups[i,,] = colSums(pop[(i*5-4):(i*5),,])
        }
    }
    
    ## Divide deaths by population, mapping to model age brackets 
    if(length(keep.dimensions)==1){
        rv = sapply(years.by.five, function(year){
            (deaths[year])/(five.year.age.groups[year])
        })
        
        new.dim.names = list(year = mid.years)
    }
    
    if (setequal(keep.dimensions, c('year','age'))){
        rv = sapply(1:length(POPULATION.AGE.MAPPING), function(age){
            sapply(1:length(years.by.five), function(year){
                age.to = names(POPULATION.AGE.MAPPING)[age] # names of mapping are the model ages - what I want to map TO
                ages.from = POPULATION.AGE.MAPPING[[age]] # list elements are the population ages - what I want to map FROM
                sum(deaths[year,ages.from])/sum(five.year.age.groups[year,ages.from])
            })
        })
        
        new.dim.names = list(year = mid.years,
                             age = names(POPULATION.AGE.MAPPING))
    }
    
    if (setequal(keep.dimensions, c('year','sex')) ){
        rv = sapply(deaths.sexes, function(sex){
            sapply(1:length(years.by.five), function(year){
                (deaths[year,sex])/(five.year.age.groups[year,sex])
            })
        })
        
        new.dim.names = list(year = mid.years,
                             sex = deaths.sexes)
    }
    
    if (setequal(keep.dimensions, c('year','age','sex'))){
        rv = sapply(deaths.sexes, function(sex){
            sapply(1:length(POPULATION.AGE.MAPPING), function(age){
                sapply(1:length(years.by.five), function(year){
                    age.to = names(POPULATION.AGE.MAPPING)[age] # names of mapping are the model ages - what I want to map TO
                    ages.from = POPULATION.AGE.MAPPING[[age]] # list elements are the population ages - what I want to map FROM
                    sum(deaths[year,ages.from,sex])/sum(five.year.age.groups[year,ages.from,sex])
                })
            })
        })
        
        new.dim.names = list(year = mid.years,
                             age = names(POPULATION.AGE.MAPPING),
                             sex = deaths.sexes)
    }
    
    dim(rv) = sapply(new.dim.names, length)
    dimnames(rv) = new.dim.names
    
    rv
    
}


# Sets up initial population by mapping surveillance data age brackets to model age brackets (using 
# map.population.ages function); adds all initial population to HIV negative status in 1970 except
# for specified seed cases 
get.initial.population = function(year,
                                  data.manager,
                                  parameters,
                                  model.age.cutoffs,
                                  ages,
                                  sexes,
                                  seed.to.ages,
                                  seed.to.sexes,
                                  seed.n){
    
    pop = get.surveillance.data(data.manager = data.manager,
                                data.type = "population.full",
                                years = 1970,
                                keep.dimensions = c('year','age','sex')) 
    
    pop.ages = dimnames(pop)$age
    pop.ages = c(pop.ages[-length(pop.ages)],"100 and over")
    dimnames(pop)$age = pop.ages
    
    POPULATION.AGE.MAPPING = map.population.ages(data.manager = data.manager,
                                                 data.type = "population.full",
                                                 model.age.cutoffs = model.age.cutoffs)
    
    # sum up population from surveillance data in to correct model age brackets
    initial.pop = sapply(sexes, function(sex){
        sapply(1:length(POPULATION.AGE.MAPPING), function(age){
            age.to = names(POPULATION.AGE.MAPPING)[age] # names of mapping are the model ages - what I want to map TO
            ages.from = POPULATION.AGE.MAPPING[[age]] # list elements are the population ages - what I want to map FROM
            sum(pop[,ages.from,sex])
        })
    })
    
    # correct the dimensions on initial.pop
    new.dim.names = list(year = "1970",
                         age = names(POPULATION.AGE.MAPPING),
                         sex = sexes)
    
    dim(initial.pop) = sapply(new.dim.names, length)
    dimnames(initial.pop) = new.dim.names
    
    # set up array for model, indexed [age,sex,subgroup,hiv status]
    state.dim.names = list(age=parameters$AGES, 
                           sex=parameters$SEXES,
                           subgroup=parameters$SUBGROUPS,
                           hiv.status=parameters$HIV.STATUS)
    
    rv = array(0,
               dim = sapply(state.dim.names, length),
               dimnames = state.dim.names)
    
    # add in initial population to hiv negative 
    rv[,,,'hiv_negative'] = initial.pop
    
    #puts n hiv cases in each of those brackets (probably middle age bracket, one male/female)
    rv[seed.to.ages,seed.to.sexes,,'undiagnosed'] = seed.n 
    
    rv
}

# Multiplies global transmission rate by age and sex multipliers; called in map.model.parameters function 
# when setting up transmission rates; age and sex multipliers come from sampled.parameters 
make.transmission.array = function(parameters,
                                   global.trate,
                                   male.to.male.multiplier,
                                   female.to.male.multiplier,
                                   female.to.female.multiplier = 0,
                                   female.age.multipliers,
                                   male.age.multipliers){
    
    if(length(parameters$AGES)!=length(female.age.multipliers)) 
        stop("incorrect number of female age multipliers")
    
    if(length(parameters$AGES)!=length(male.age.multipliers)) 
        stop("incorrect number of male age multipliers")
    
    transmission.dim.names = list(age.to=parameters$AGES, 
                                  sex.to=parameters$SEXES,
                                  subgroup.to=parameters$SUBGROUPS,
                                  age.from=parameters$AGES, 
                                  sex.from=parameters$SEXES,
                                  subgroup.from=parameters$SUBGROUPS)
    
    rv = array(global.trate,
               sapply(transmission.dim.names, length),
               dimnames = transmission.dim.names)
    
    rv[,"female",,,,] = rv[,"female",,,,]*female.age.multipliers # transmission to females
    rv[,"male",,,,] = rv[,"male",,,,]*male.age.multipliers #transmission to males
    
    rv[,"male",,,"male",] = rv[,"male",,,"male",]*male.to.male.multiplier
    rv[,"male",,,"female",] = rv[,"male",,,"female",]*female.to.male.multiplier
    rv[,"female",,,"female",] = rv[,"female",,,"female",]*female.to.female.multiplier
    
    rv
}


# Create crude birth rate array - NO LONGER USING (using age-specific fertility)
map.birth.rates = function(data.manager,
                           model.age.cutoffs){
    
    years.by.five = data.manager$births$YEARS
    start.years = as.numeric(substr(years.by.five,1,4))
    end.years = as.numeric(substr(years.by.five,6,9))
    mid.years = (start.years + (end.years-1))/2
    
    POPULATION.AGE.MAPPING = map.population.ages(data.manager = data.manager,
                                                 data.type = "population.full",
                                                 model.age.cutoffs = model.age.cutoffs)
    
    ages = names(POPULATION.AGE.MAPPING)
    sexes = c("male","female")
    
    full.dim.names = list(year = mid.years,
                          age = ages,
                          sex = sexes)
    
    ## Pull births
    births = get.surveillance.data(data.manager = data.manager,
                                   data.type = "births",
                                   years = years.by.five,
                                   keep.dimensions = 'year')
    
    
    rv = array(births,
               dim = sapply(full.dim.names, length),
               dimnames = full.dim.names)
    
    rv
    
}
