################################################################################################
################
#Description: Functions to set up the parameter object with constant and time-varying parameters
################
################################################################################################


# age.cutoffs - the lower limit for each bracket
create.model.parameters <- function(age.cutoffs=MODEL.AGE.CUTOFFS,
                                   sexes = c('female','male'),
                                   subgroups = 'all')
{
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


#-- Map all parameters --#
map.model.parameters <- function(parameters,
                                 sampled.parameters=c(birth.rates=0.01, 
                                                      aging.rates=0.10,
                                                      hiv.mortality.rates.suppressed=0.01, 
                                                      hiv.mortality.rates.unsuppressed=0.02,
                                                      non.hiv.mortality.rates=0.01, 
                                                      testing.rates=1.5, 
                                                      engagement.rates=3,
                                                      unsuppressed.disengagement.rates=0.2,
                                                      suppressed.disengagement.rates=0.2,
                                                      suppression.rates=3,
                                                      unsuppression.rates=0.1,
                                                      global.transmission.rate=60000,
                                                      relative.transmission.from.diagnosis=0.33
                                                      ))
{
    
    #-- SET UP DIMENSIONS --#
    state.dim.names = list(age=parameters$AGES, 
                           sex=parameters$SEXES,
                           subgroup=parameters$SUBGROUPS,
                           hiv.status=parameters$HIV.STATUS)
    
    trans.dim.names = list(age=parameters$AGES, 
                               sex=parameters$SEXES,
                               subgroup=parameters$SUBGROUPS)
    
    #-- BIRTH --# 
    parameters = add.time.varying.parameter.value(parameters,
                                                  parameter.name='FERTILITY.RATES',
                                                  value = array(sampled.parameters['birth.rates'],
                                                                dim=sapply(state.dim.names, length),
                                                                dimnames=state.dim.names),
                                                  time = 2000)
    
    # call actual birth rates; start with crude birth rate; can change to fertility rates or other options
    
    
    parameters = add.time.varying.parameter.value(parameters,
                                                  parameter.name='MALE.BIRTHS',
                                                  value = 0.5,
                                                  time = 2000)
    
    #-- AGING --#
    parameters = add.time.varying.parameter.value(parameters,
                                                  parameter.name='AGING.RATES',
                                                  value = array(sampled.parameters['aging.rates'],
                                                                dim=sapply(state.dim.names, length),
                                                                dimnames=state.dim.names),
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
    
    #this will be within the for loop
    parameters = add.time.varying.parameter.value(parameters,
                                                  parameter.name='NON.HIV.MORTALITY.RATES',
                                                  value = array(sampled.parameters['non.hiv.mortality.rates'],
                                                                dim=sapply(state.dim.names, length),
                                                                dimnames=state.dim.names), #change this to the big.array thing below
                                                  time = 2000) # will actually need age-specific mortality (will depend on age brackets)
    
    # This doesn't have the right age brackets/set up - need to fix below function
    deaths.age.sex = calculate.all.death.rates(data.manager = DATA.MANAGER, keep.dimensions = c('year','sex','age'))
    
    # Notes from 4/15 meeting
    # Get array of death rates from below; for every year, call below function
    # hydrate array up from 2D to 4D (including subgroup and HIV status)
    #create an array by calling 2D array and then give dimnames/dimensions that I want - i.e., below 
    
    # for every value of the for loop, there will be a large array (for every year) 
    
    # big.array = array(small.array,
    #                   dim = sapply(),
    #                   dimnames = big.array.dim.names)
    # 
    
    
    #-- DIAGNOSES --#
    parameters = add.time.varying.parameter.value(parameters,
                                                  parameter.name='TESTING.RATES',
                                                  value = array(sampled.parameters['testing.rates'],
                                                                dim=sapply(trans.dim.names, length),
                                                                dimnames=trans.dim.names),
                                                  time = 2000)
    
    #-- NEW INFECTIONS --#
    transmission.dim.names = list(age.to=parameters$AGES, 
                                  sex.to=parameters$SEXES,
                                  subgroup.to=parameters$SUBGROUPS,
                                  age.from=parameters$AGES, 
                                  sex.from=parameters$SEXES,
                                  subgroup.from=parameters$SUBGROUPS)
    
    transmission.rates = array(sampled.parameters['global.transmission.rate']/length(parameters$AGES)/length(parameters$SEXES)/length(parameters$SUBGROUPS),
                               dim=sapply(transmission.dim.names, length),
                               dimnames=transmission.dim.names)
    
    parameters = add.time.varying.parameter.value(parameters,
                                                  parameter.name='TRANSMISSION.RATES',
                                                  value = transmission.rates,
                                                  time = 2000)
    
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
                                             value)
{
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


# This function computes the parameter value at a sepecific time
compute.time.varying.parameters <- function(parameters, time)
{
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
# this uses fixed age groups from the deaths data; need to use model age groups
calculate.all.death.rates = function(data.manager,
                                     keep.dimensions){
        
        years.by.five = data.manager$deaths$YEARS
        deaths.ages = data.manager$deaths$AGES
        deaths.sexes = data.manager$deaths$SEXES
        start.years = as.numeric(substr(years.by.five,1,4))
        end.years = as.numeric(substr(years.by.five,8,11))
        mid.years = (start.years + (end.years-1))/2
        
        ## Pull deaths (I'm using the getter function here, but above I pulled the years straight from the data manager...)
        deaths = get.surveillance.data(data.manager = data.manager,
                                       data.type = "deaths",
                                       years = years.by.five,
                                       keep.dimensions = keep.dimensions)
        
        ## Pull population
        pop = get.surveillance.data(data.manager = data.manager,
                                    data.type = "population",
                                    years = 1950:2020,
                                    keep.dimensions = keep.dimensions) 
        
        # combine 95-99 and 100+ into 95+ --> very hacky 
        if (setequal(keep.dimensions, c('year','age'))){
                pop = cbind(pop[,1:(ncol(pop)-2)],cbind(rowSums(pop[,(ncol(pop)-1):(ncol(pop))])))
                colnames(pop) = deaths.ages
        }
        if (setequal(keep.dimensions, c('year','age','sex'))){
                male = pop[,,"male"]
                female = pop[,,"female"]
                male = cbind(male[,1:(ncol(male)-2)],cbind(rowSums(male[,(ncol(male)-1):(ncol(male))])))
                female = cbind(female[,1:(ncol(female)-2)],cbind(rowSums(female[,(ncol(female)-1):(ncol(female))])))
                
                pop = pop[,-21,]
                pop[,,"male"] = male
                pop[,,"female"] = female
                dimnames(pop)[2][[1]] = deaths.ages
        } 
        
        ## Dim names
        if(length(keep.dimensions)==1){
                death.rate.dim.names = list(year = years.by.five)
        }
        
        if (setequal(keep.dimensions, c('year','age'))){
                death.rate.dim.names = list(year = years.by.five,
                                            age = deaths.ages)
        }
        
        if (setequal(keep.dimensions, c('year','sex'))){
                death.rate.dim.names = list(year = years.by.five,
                                            sex = deaths.sexes)
        }
        
        if (setequal(keep.dimensions, c('year','age','sex'))){
                death.rate.dim.names = list(year = years.by.five,
                                            age = deaths.ages,
                                            sex = deaths.sexes)
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
        
        # BEFORE THIS STEP, NEED TO CHANGE TO MODEL AGE GROUPS USING MAPPING
        DEATH.RATE = 1000*deaths/five.year.age.groups 
        death.rate.dim.names$year = as.character(mid.years) # e.g., change from "1950 - 1955" to 1952
        dimnames(DEATH.RATE) = death.rate.dim.names
        
        DEATH.RATE
}

# HOW WE SET UP PREVIOUSLY 
# calculate.death.rates = function(data.manager,
#                                  ages, #specify the model ages here - will have to pass in the model age upper and lower (see other function)
#                                  sexes,
#                                  year){
#         
#         # return a list, each value corresponds to rates; indexed age, sex
#         # indexed age, sex
#         
#         rv = sapply(sexes, function(sex){
#                 sapply(ages, function(age){
#                         
#                         #from surveillance manager, pull all the deaths for any surveillance age bracket that falls into given model age bracket
#                         # (use code in ages file) - started a new generic function 
#                         # pull the population for any ages that fall in the age bracket and any years in that period, then divide
#                         
#                         #outer sapply is columns; inner is rows
#                         
#                 })
#         })
#         
# }

# similar function for birth rates


get.initial.population = function(year,
                                  data.manager,
                                  ages,
                                  sexes,
                                  seed.to.ages,
                                  seed.to.sexes,
                                  seed.n){
    
    #returns an array indexed age, sex, subgroup (hard code in one subgroup), hiv status
    # rv [,,,'hiv.negative'] = (population from the surveillance data in that year)
    
    rv[seed.to.ages,seed.to.sexes,,'undiagnosed'] = seed.n #puts n hiv cases in each of those brackets (probably middle age bracket, one male/female)
    
}


