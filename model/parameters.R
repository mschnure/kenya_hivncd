
# sets up the constants
# age.cutoffs - the lower limit for each bracket
create.model.parameters <- function(age.cutoffs=c(10,25,55),
                                   sexes = c('female','male'),
                                   subgroups = 'all')
{
    parameters = list()
    
    #-- SET UP THE BASICS --#
    
    # sex, risk, subpop
    parameters$SEXES = sexes 
    parameters$SUBGROUPS = subgroups
    
    # hiv status
    parameters$HIV.STATUS = c('uninfected','undiagnosed','diagnosed.unsuppressed','diagnosed.suppressed')
    
    # ages
    age.names = c(paste0(age.cutoffs[-length(age.cutoffs)], " to ", age.cutoffs[-1]),
                  paste0(age.cutoffs[length(age.cutoffs)], "+"))
    parameters$AGES = age.names
    parameters$AGE.SPANS = c(age.cutoffs[-1] - age.cutoffs[-length(age.cutoffs)],
                             Inf)
    
    #- Return --#
    parameters
}


map.model.parameters <- function(parameters,
                                 sampled.parameters=c(birth.rate=0.01))
{
    
    #-- SET UP DIMENSIONS --#
    state.dim.names = list(age=parameters$AGES, 
                           sex=parameters$SEXES,
                           subgroup=parameters$SUBGROUPS,
                           hiv.status=parameters$HIV.STATUS)
    
    incidence.dim.names = list(age=parameters$AGES, 
                               sex=parameters$SEXES,
                               subgroup=parameters$SUBGROUPS)
    
    #-- BIRTH RATE --#
    
    parameters = add.time.varying.parameter.value(parameters,
                                                  parameter.name='FERTILITY.RATE',
                                                  value = array(sampled.parameters['birth.rate'],
                                                                dim=sapply(state.dim.names, length),
                                                                dimnames=dim.names),
                                                  time = 2000)
    
    
    parameters = add.time.varying.parameter.value(parameters,
                                                  parameter.name='MALE.BIRTHS',
                                                  value = 0.5,
                                                  time = 2000)
    
    # @melissa
    # come up with a value for every thing in diffeq that is references pp$<whatever>
    # with the right dimensions
    
    #-- RETURN --#
    
    parameters
}


# This function adds a time point and value to the spline for a parameter
# cannot have two times time same
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


compute.time.varying.parameters <- function(parameters, time)
{
    lapply(parameters$time.varying.parameters, function(params){
        # params is a list with two components
        # $times - a vector of times
        # $values - a list of values
        #  assume these are ordered
        
        n.times = length(times)
        
        if (time <= params$times[1])
            params$values[[1]]
        else if (time >= params$times[n.times])
            params$values[[n.times]]
        else #we need to interpolate
        {
            index1 = (1:n.times)[params$times<=time]
            index1 = index1[length(index1)]
            index2 = index1 + 1
            
            if (time==params$times[index1])
                params$values[[index1]]
            else
            {
                time1 = params$times[index1]
                time2 = params$times[index2]
                
                value1 = params$values[[index1]]
                value2 = params$values[[index2]]
                
                slope = (value2 - value1) / (time2 - time1)
                value1 + slope * (time - time1)
            }
        }
        
        # Goal: Linearly interpolate
    })
}