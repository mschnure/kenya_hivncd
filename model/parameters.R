################################################################################################
#Description: Functions to set up the parameter object with constant and time-varying parameters
################################################################################################

# age.cutoffs - the lower limit for each bracket
create.model.parameters <- function(age.cutoffs=MODEL.AGE.CUTOFFS,
                                    sexes = c('female','male'),
                                    subgroups = 'all'){
    parameters = list()
    
    #-- SET UP THE BASICS --#
    
    # sex, risk, subpop
    parameters$SEXES = sexes 
    parameters$SUBGROUPS = subgroups
    
    # ages
    parsed.ages = parse.age.brackets(age.cutoffs)
    
    parameters$AGES = parsed.ages$labels
    parameters$AGE.SPANS = parsed.ages$spans
    
    # hiv status
    parameters$HIV.STATUS = c('hiv_negative','undiagnosed','diagnosed_unengaged','engaged_unsuppressed','engaged_suppressed')
    parameters$HIV.STATES = c('undiagnosed','diagnosed_unengaged','engaged_unsuppressed','engaged_suppressed') 
    parameters$DIAGNOSED.STATES = c('diagnosed_unengaged','engaged_unsuppressed','engaged_suppressed')
    parameters$ENGAGED.STATES = c('engaged_unsuppressed','engaged_suppressed')
    
    
    #- Return --#
    parameters  
}

get.default.parameters = function(){
    rv = c(fertility.multiplier=1.2,
           over.80.mortality.multiplier=1.5,
           hiv.mortality.rates.suppressed=0.0, # will need to update
           hiv.mortality.rates.unsuppressed=0.0, #update 
           # non.hiv.mortality.rates=0.01, 
           testing.rates=0.8, 
           engagement.rates=3,
           unsuppressed.disengagement.rates=0.2,
           suppressed.disengagement.rates=0.2,
           suppression.rates=3,
           unsuppression.rates=0.1,
           # global.transmission.rate=6, #the average number of infections from one undiagnosed HIV+ person per year 
           start.time=1975,
           time.0=1990,
           time.1=1998,
           trate.0=1.3,
           trate.1=0.27,
           male.to.male.multiplier=1,
           female.to.male.multiplier=1,
           age.15.to.19.multiplier=1,
           age.20.to.29.multiplier=1,
           age.40.to.49.multiplier=1,
           age.50.and.over.multiplier=1,
           relative.transmission.from.diagnosis=0.33) #repeat this
}

#-- Map all parameters --#
map.model.parameters <- function(parameters,
                                 sampled.parameters=get.default.parameters(),
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
    
    aging.rates = array((1/parameters$AGE.SPANS),
                        dim=sapply(state.dim.names, length),
                        dimnames=state.dim.names)
    
    parameters = add.time.varying.parameter.value(parameters,
                                                  parameter.name='AGING.RATES',
                                                  value = aging.rates,
                                                  time = 2000)
    
    
    #-- MORTALITY --#
    HIV.MORTALITY.RATES=array(0, #assume 0 for non-hiv states
                              dim=sapply(state.dim.names, length),
                              dimnames=state.dim.names)
    
    HIV.MORTALITY.RATES[,,,'engaged_suppressed'] = sampled.parameters['hiv.mortality.rates.suppressed']
    HIV.MORTALITY.RATES[,,,c('undiagnosed', 'diagnosed_unengaged', 'engaged_unsuppressed')] = sampled.parameters['hiv.mortality.rates.unsuppressed']
    
    
    parameters = add.time.varying.parameter.value(parameters,
                                                  parameter.name='HIV.MORTALITY.RATES',
                                                  value = HIV.MORTALITY.RATES,
                                                  time = 2000)
    
    
    deaths.age.sex = calculate.all.death.rates(data.manager = DATA.MANAGER, 
                                               keep.dimensions = c('year','sex','age'), 
                                               model.age.cutoffs = MODEL.AGE.CUTOFFS)
    
    #setting up code to smooth/project death rate into future 
    anchor.year = 2020
    years = as.numeric(dimnames(deaths.age.sex)$year) - anchor.year
    years.label = as.numeric(dimnames(deaths.age.sex)$year) # for plotting
    
    project.years = (max(years)+1):(project.to.year-anchor.year)-max(years)
    project.years = project.years[project.years%%5==0] # only including multiples of 5
    desired.years = c(years,max(years)+project.years) # future years to predict
    smoothed.years.label = desired.years + anchor.year # for plotting
    mask = rep(T,length(years)) # use this to remove years
    mask = years.label<1987 
    
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
    
    for (year in dimnames(smooth.deaths.age.sex)$year){
        rv = array(smooth.deaths.age.sex[year,,],
                   dim = sapply(state.dim.names, length),
                   dimnames = state.dim.names)
        
        # tune 80+ mortality
        rv["80 and over",,,] = rv["80 and over",,,]*sampled.parameters["over.80.mortality.multiplier"]
        
        parameters = add.time.varying.parameter.value(parameters,
                                                      parameter.name='NON.HIV.MORTALITY.RATES',
                                                      value = rv,
                                                      time = as.numeric(year))
    }
    
    
    
    #-- DIAGNOSES --#
    parameters = add.time.varying.parameter.value(parameters,
                                                  parameter.name='TESTING.RATES',
                                                  value = array(sampled.parameters['testing.rates'],
                                                                dim=sapply(trans.dim.names, length),
                                                                dimnames=trans.dim.names),
                                                  time = 2000)
    
    # Add more testing rates (e.g., testing.rates.1) to have splines - same thing for suppression; engagement
    # parameters = add.time.varying.parameter.value(parameters,
    #                                               parameter.name='TESTING.RATES',
    #                                               value = array(sampled.parameters['testing.rates'],
    #                                                             dim=sapply(trans.dim.names, length),
    #                                                             dimnames=trans.dim.names),
    #                                               time = 2000)
    
    
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
                sex.proportions[sex.from]*get.age.mixing.proportions(sex.to=sex.to,
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
    
    
    
    # Set transmission rate to a high level (trate.0) at start.time
    transmission.rates.0 = make.transmission.array(parameters = parameters,
                                                   global.trate = sampled.parameters["trate.0"],
                                                   male.to.male.multiplier = sampled.parameters["male.to.male.multiplier"], # add this above
                                                   female.to.male.multiplier = sampled.parameters["female.to.male.multiplier"],
                                                   age.multipliers = c(0,0,
                                                                       sampled.parameters["age.15.to.19.multiplier"],
                                                                       sampled.parameters["age.20.to.29.multiplier"],
                                                                       sampled.parameters["age.20.to.29.multiplier"],
                                                                       1,
                                                                       sampled.parameters["age.40.to.49.multiplier"],
                                                                       sampled.parameters["age.50.and.over.multiplier"],
                                                                       sampled.parameters["age.50.and.over.multiplier"],
                                                                       sampled.parameters["age.50.and.over.multiplier"],
                                                                       sampled.parameters["age.50.and.over.multiplier"]))
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
    
    
    # Set transmission rate to a low level (trate.1) at time.1
    transmission.rates.1 = make.transmission.array(parameters = parameters,
                                                   global.trate = sampled.parameters["trate.1"],
                                                   male.to.male.multiplier = sampled.parameters["male.to.male.multiplier"], # add this above
                                                   female.to.male.multiplier = sampled.parameters["female.to.male.multiplier"],
                                                   age.multipliers = c(0,0,
                                                                       sampled.parameters["age.15.to.19.multiplier"],
                                                                       sampled.parameters["age.20.to.29.multiplier"],
                                                                       sampled.parameters["age.20.to.29.multiplier"],
                                                                       1,
                                                                       sampled.parameters["age.40.to.49.multiplier"],
                                                                       sampled.parameters["age.50.and.over.multiplier"],
                                                                       sampled.parameters["age.50.and.over.multiplier"],
                                                                       sampled.parameters["age.50.and.over.multiplier"],
                                                                       sampled.parameters["age.50.and.over.multiplier"]))
    
    mixing.proportions.1 = mixing.proportions.0 # keep this the same for now
    transmission.rates.1=transmission.rates.1*mixing.proportions.1
    
    dim(transmission.rates.1) = c(n.trans.states, n.trans.states)
    transmission.rates.1 = as.matrix(transmission.rates.1)
    
    
    parameters = add.time.varying.parameter.value(parameters,
                                                  parameter.name='TRANSMISSION.RATES',
                                                  value = transmission.rates.1,
                                                  time = sampled.parameters['time.1'])
    
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
    
    
    #-- ENGAGEMENT/DISENGAGEMENT --#
    parameters = add.time.varying.parameter.value(parameters,
                                                  parameter.name='ENGAGEMENT.RATES',
                                                  value = array(sampled.parameters['engagement.rates'],
                                                                dim=sapply(trans.dim.names, length),
                                                                dimnames=trans.dim.names),
                                                  time = 2000)
    
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
    
    #-- SUPPRESSION/UNSUPPRESSION --#
    parameters = add.time.varying.parameter.value(parameters,
                                                  parameter.name='SUPPRESSION.RATES',
                                                  value = array(sampled.parameters['suppression.rates'],
                                                                dim=sapply(trans.dim.names, length),
                                                                dimnames=trans.dim.names),
                                                  time = 2000)
    
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

# This function adds a time point and value to the spline for a parameter
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


# This function computes the parameter value at a specific time
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

## Calculates death rates (total, by age, by sex, or by age/sex)
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


get.initial.population = function(year,
                                  data.manager,
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

make.transmission.array = function(parameters,
                                   global.trate,
                                   male.to.male.multiplier,
                                   female.to.male.multiplier,
                                   female.to.female.multiplier = 0,
                                   age.multipliers){
    
    transmission.dim.names = list(age.to=parameters$AGES, 
                                  sex.to=parameters$SEXES,
                                  subgroup.to=parameters$SUBGROUPS,
                                  age.from=parameters$AGES, 
                                  sex.from=parameters$SEXES,
                                  subgroup.from=parameters$SUBGROUPS)
    
    rv = array(global.trate*age.multipliers,
               sapply(transmission.dim.names, length),
               dimnames = transmission.dim.names)
    
    rv[,"male",,,"male",] = rv[,"male",,,"male",]*male.to.male.multiplier
    rv[,"male",,,"female",] = rv[,"male",,,"female",]*female.to.male.multiplier
    rv[,"female",,,"female",] = rv[,"female",,,"female",]*female.to.female.multiplier
    
    rv
}





## Create crude birth rate array - NO LONGER USING (using age-specific fertility)
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
