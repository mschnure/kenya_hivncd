# array set up
# [age,sex,risk,subpopulation,hiv.status]

library(odeintr)


##---------------------------------------##
##-- THE COMPUTE DIFFERENTIAL FUNCTION --##
##---------------------------------------##
# This function is called at every itteration
# y is 1-D aray including the model state and main transitions that we are interested in (incidence, diagnosis, hiv/non-hiv mortality)
compute.dx <- function(time,
                       y, #the vector-form model state at this time
                       parameters)
{
    ##---------------------------------##
    ##-- GET TIME-VARYING PARAMETERS --##
    ##---------------------------------##
    
    pp = compute.time.varying.parameters(parameters, time)
    # now pp is a list
    
    ##----------------------------##
    ##-- PARSE THE MODEL STATE  --##
    ##----------------------------##
    #state #indexed [age, sex, subgroup, hiv-status]
    #incidence #indexed [age, sex, subgroup]
    #diagnoses #indexed [age, sex, subgroup]
    #hiv.mortality #indexed [age, sex, subgroup, hiv-status]  
    #non.hiv.mortality #indexed [age, sex, subgroup, hiv-status]
    
    state.dim.names = list(age=parameters$AGES, 
                           sex=parameters$SEXES,
                           subgroup=parameters$SUBGROUPS,
                           hiv.status=parameters$HIV.STATUS)
    
    trans.dim.names = list(age=parameters$AGES, 
                           sex=parameters$SEXES,
                           subgroup=parameters$SUBGROUPS)
    
    state.length = prod(sapply(state.dim.names, length))
    
    state = array(y[1:state.length], 
                  dim = sapply(state.dim.names, length), 
                  dimnames = state.dim.names) 
    # we dont need the stats, so we dont parse them here 
    
    
    ##----------------------##
    ##-- SET UP DX ARRAYS --##
    ##----------------------##
    
    dx.state = array(0, 
                     dim = sapply(state.dim.names, length), 
                     dimnames = state.dim.names)#indexed [age, sex, subgroup, hiv-status]
    dx.incidence = array(0, 
                         dim = sapply(trans.dim.names, length), 
                         dimnames = trans.dim.names)#indexed [age, sex, subgroup]
    dx.diagnoses = array(0, 
                         dim = sapply(trans.dim.names, length), 
                         dimnames = trans.dim.names)#indexed [age, sex, subgroup]
    dx.hiv.mortality = array(0, 
                             dim = sapply(state.dim.names, length), 
                             dimnames = state.dim.names)#indexed [age, sex, subgroup, hiv-status]
    dx.non.hiv.mortality = array(0, 
                                 dim = sapply(state.dim.names, length), 
                                 dimnames = state.dim.names)#indexed [age, sex, subgroup, hiv-status]
    
    ##----------------------------------##
    ##-- COMPUTE THE CHANGES IN STATE --##
    ##----------------------------------##
    
    n.ages=length(parameters$AGES)
    
    #-- BIRTH --#
    births = state * pp$FERTILITY.RATES
    births = apply(births, 3, sum) # assuming the subgroups represent various locations
    
    dx.state[1,'male',,'hiv_negative'] = births*pp$MALE.BIRTHS
    dx.state[1,'female',,'hiv_negative'] = births*(1-pp$MALE.BIRTHS)
    
    
    #-- AGING --#
    aging = state * pp$AGING.RATES #indexed [age, sex, subgroup, hiv-status]
    dx.state = dx.state - aging #aging out
    dx.state[-1,,,] = dx.state[-1,,,] + aging[-n.ages,,,]  #-1 skips the first row (people aging into next agegroup); -n.ages skips the last row (these individuals die out of model)
    
    dx.non.hiv.mortality[n.ages,,,] = dx.non.hiv.mortality[n.ages,,,] + aging[n.ages,,,]
    
    
    #-- MORTALITY --#
    # hiv mortality 
    hiv.mortality = state * pp$HIV.MORTALITY.RATES #indexed [age, sex, subgroup, hiv-status]
    dx.state = dx.state - hiv.mortality
    dx.hiv.mortality = dx.hiv.mortality + hiv.mortality
    
    # non hiv mortality
    non.hiv.mortality = state * pp$NON.HIV.MORTALITY.RATES #indexed [age, sex, subgroup, hiv-status]
    dx.state = dx.state - non.hiv.mortality
    dx.non.hiv.mortality = dx.non.hiv.mortality + non.hiv.mortality
    
    #-- CHANGES in SEX --#
    # (assuming sex is immutable)
    
    #-- CHANGES in SUB-GROUP --#
    # (we are ignoring this for now)
    
    #-- CHANGES in HIV STATUS --#
    
    #-- DIAGNOSES --#
    #Note: When using a multidimension array, if a dimension is set to 1 and we return the array, R will authomaticaly remove that dimention (e.g., a=array(c(1:6),c(3,2,1)) is a 3-dim array; a[1,,] retunrs a 2-D array and excludes the 3rd dimension) 
    #using as.numeric will allow up to multiply the returned value from the state even if a dimension is droped. otherwise, the multiplication can generate an error
    diagnosed = pp$TESTING.RATES * as.numeric(state[,,,'undiagnosed'])  
    dx.state[,,,'undiagnosed'] = as.numeric(dx.state[,,,'undiagnosed']) - diagnosed
    dx.state[,,,'diagnosed_unengaged'] = as.numeric(dx.state[,,,'diagnosed_unengaged']) + diagnosed
    dx.diagnoses = dx.diagnoses + diagnosed
    
    #-- NEW INFECTIONS --#
    incidence = 20     # Using this for now
    dx.state[,,,'hiv_negative'] = as.numeric(dx.state[,,,'hiv_negative']) - incidence
    dx.state[,,,'undiagnosed'] = as.numeric(dx.state[,,,'undiagnosed']) + incidence
    dx.incidence = dx.incidence + incidence
    
    #-- ENGAGEMENT --#
    engaged = pp$ENGAGEMENT.RATES * as.numeric(state[,,,'diagnosed_unengaged'])
    dx.state[,,,'diagnosed_unengaged'] = as.numeric(dx.state[,,,'diagnosed_unengaged']) - engaged
    dx.state[,,,'engaged_unsuppressed'] = as.numeric(dx.state[,,,'engaged_unsuppressed']) + engaged
    
    #-- DISENGAGED FROM UNSUPPRESSED --#
    disengaged.unsuppressed = pp$UNSUPPRESSED.DISENGAGEMENT.RATES * as.numeric(state[,,,'engaged_unsuppressed'])
    dx.state[,,,'engaged_unsuppressed'] = as.numeric(dx.state[,,,'engaged_unsuppressed']) - disengaged.unsuppressed
    dx.state[,,,'diagnosed_unengaged'] = as.numeric(dx.state[,,,'diagnosed_unengaged']) + disengaged.unsuppressed
    
    #-- DISENGAGED FROM SUPPRESSED --#
    disengaged.suppressed = pp$SUPPRESSED.DISENGAGEMENT.RATES * as.numeric(state[,,,'engaged_suppressed'])
    dx.state[,,,'engaged_suppressed'] = as.numeric(dx.state[,,,'engaged_suppressed']) - disengaged.suppressed
    dx.state[,,,'diagnosed_unengaged'] = as.numeric(dx.state[,,,'diagnosed_unengaged']) + disengaged.suppressed
    
    #-- SUPPRESSION --#
    suppressed = pp$SUPPRESSION.RATES * as.numeric(state[,,,'engaged_unsuppressed'])
    dx.state[,,,'engaged_unsuppressed'] = as.numeric(dx.state[,,,'engaged_unsuppressed']) - suppressed
    dx.state[,,,'engaged_suppressed'] = as.numeric(dx.state[,,,'engaged_suppressed']) + suppressed
    
    #-- UNSUPPRESSION --#
    unsuppressed = pp$UNSUPPRESSION.RATES * as.numeric(state[,,,'engaged_suppressed'])
    dx.state[,,,'engaged_suppressed'] = as.numeric(dx.state[,,,'engaged_suppressed']) - unsuppressed
    dx.state[,,,'engaged_unsuppressed'] = as.numeric(dx.state[,,,'engaged_unsuppressed']) + unsuppressed
    
    
    #-- INCIDENCE --#
    #we need an array force of infection to each stratum of [age,sex,subgroup,hiv.status]
    
    # x.to: strata that receives the new infection
    # x.from: strata that transmits HIV
    # for each 'x.to' strata, loop through all 'x.from' stratas and compute the new transmissions based on transmission rate, prevalence of HIV in the 'x.from' strata, and the proportion of partnerships between the two stratas
    
    if (1==2)  
    {
        for (a.to in parameters$AGES)
        {
            for (s.to in parameters$SEXES)
            {
                for (r.to in parameters$SUBGROUPS)
                {
                    for (a.from in parameters$AGES)
                    {
                        for (s.from in parameters$SEXES)
                        {
                            for (r.from in parameters$SUBGROUPS)
                            {
                                dx.incidence[a.to, s.to, r.to] = dx.incidence[a.to, s.to, r.to] +
                                    proportion.of.tos.partners.in.from * transmission.rate * prevalence.of.infections.in.from
                            }
                        }
                    }
                }
            }
        }
    }
    
    
    ##------------------------------##
    ##-- PACKAGE IT UP AND RETURN --##
    ##------------------------------##
    #flatten out all our arrays
    #return a vector of length = length(y) that represents the change in each element in y
    # when data elements have different dimensions, using as.numeric makes sure that we can collapse them into a 1D vector
    c(as.numeric(dx.state),   
      as.numeric(dx.incidence), 
      as.numeric(dx.diagnoses), 
      as.numeric(dx.hiv.mortality), 
      as.numeric(dx.non.hiv.mortality))
    
    
}


# Building up the initial vector, including the initial.states and placeholder for the statistics that are recorded (e.g.,incidence, diagnosis, etc)
set.up.initial.diffeq.vector <- function(initial.state,
                                         parameters)
{
    # y is 1-D aray including the model state and main transitions that we are interested in (incidence, diagnosis, hiv/non-hiv mortality)
    state.length = length(parameters$AGES)*length(parameters$SEXES)*length(parameters$SUBGROUPS)*length(parameters$HIV.STATUS)
    trans.length = length(parameters$AGES)*length(parameters$SEXES)*length(parameters$SUBGROUPS)
    
    total.length = 3 * state.length + #state plus two mortality arrays
        2 * trans.length #incidence plus new diagnoses
    
    rv = numeric(total.length) #opens a 1-D vector of 0s to this length
    rv[1:state.length] = as.numeric(initial.state)  
    
    rv
}


run.model <- function(parameters,
                      initial.state,
                      start.year,
                      end.year,
                      keep.years)
{
    #This is just a stub
    ode.results = odeintr::integrate_sys(sys=function(x,t){compute.dx(time=t,y=x,parameters=parameters)},
                                         init=set.up.initial.diffeq.vector(initial.state, parameters),
                                         start=start.year,
                                         duration=end.year+1-start.year,
                                         step_size=1)
    
    # We need to process the results
    process.ode.results(ode.results,
                        parameters=parameters,
                        start.year=start.year,
                        end.year=end.year,
                        keep.years=keep.years)
}


## --break out the epi data we want-- ##
# keep.years: ##is this an array of years to keep?
process.ode.results <- function(ode.results,
                                parameters,
                                start.year,
                                end.year,
                                keep.years)
{
    
    # ode.result is a 2-D array with rows representing time, and columns representing the outputs (vector y)
  # the first row represents the initail state of the model (time = 0)
  # the first column shows times (we drop it from the results)
    ode.results=as.matrix(ode.results)[,-1]
    
    dimnames(ode.results)[[1]]=(start.year-1):end.year  
    
    previous.years=as.character(keep.years-1)
    keep.years=as.character(keep.years)
    
    state.dim.names = list(year=keep.years,
                           age=parameters$AGES, 
                           sex=parameters$SEXES,
                           subgroup=parameters$SUBGROUPS,
                           hiv.status=parameters$HIV.STATUS)
    
    trans.dim.names = list(year=keep.years,
                           age=parameters$AGES, 
                           sex=parameters$SEXES,
                           subgroup=parameters$SUBGROUPS)
    
    state.length = length(parameters$AGES)* length(parameters$SEXES) * length(parameters$SUBGROUPS) * length(parameters$HIV.STATUS)
    trans.length =  length(parameters$AGES)* length(parameters$SEXES) * length(parameters$SUBGROUPS)
    
    #using a list will allow us to have elements of different size
    rv=list(years=as.numeric(keep.years),
            AGES=parameters$AGES,
            SEXES=parameters$SEXES,
            SUBGROUPS=parameters$SUBGROUPS,
            HIV.STATUS=parameters$HIV.STATUS,
            HIV.STATES=parameters$HIV.STATES, 
            DIAGNOSED.STATES=parameters$DIAGNOSED.STATES, 
            ENGAGED.STATES=parameters$ENGAGED.STATES
    )
    
    index=0
    
    #states
    rv$population=array(ode.results[keep.years,index+1:state.length],
                        dim = sapply(state.dim.names,length),
                        dimnames = state.dim.names)
    index=index+state.length
    
    #incidence (reporeted as cumulative value)
    rv$incidence=array(ode.results[keep.years,index+1:trans.length]-ode.results[previous.years,index+1:trans.length],
                       dim = sapply(trans.dim.names,length),
                       dimnames = trans.dim.names)
    index=index+trans.length
    
    #diagnosis (reporeted as cumulative value)
    rv$diagnoses=array(ode.results[keep.years,index+1:trans.length]-ode.results[previous.years,index+1:trans.length],
                       dim = sapply(trans.dim.names,length),
                       dimnames = trans.dim.names)
    index=index+trans.length
    
    #hiv mortality (reporeted as cumulative value)
    rv$hiv.mortality=array(ode.results[keep.years,index+1:state.length]-ode.results[previous.years,index+1:state.length],
                           dim = sapply(state.dim.names,length),
                           dimnames = state.dim.names)
    index=index+state.length
    
    #non.hiv mortality (reporeted as cumulative value)
    rv$non.hiv.mortality=array(ode.results[keep.years,index+1:state.length]-ode.results[previous.years,index+1:state.length], 
                               dim = sapply(state.dim.names,length),
                               dimnames = state.dim.names)
    index=index+state.length
    
    
    class(rv)="hiv_simulation" 
    
    rv
}
