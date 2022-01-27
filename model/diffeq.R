# array set up
# [age,sex,risk,subpopulation,hiv.status]

library(odeintr)


##---------------------------------------##
##-- THE COMPUTE DIFFERENTIAL FUNCTION --##
##---------------------------------------##

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
    
    state.dim.names = list(age=parameters$AGES, 
                           sex=parameters$SEXES,
                           subgroup=parameters$SUBGROUPS,
                           hiv.status=parameters$HIV.STATUS)
    
    incidence.dim.names = list(age=parameters$AGES, 
                               sex=parameters$SEXES,
                               subgroup=parameters$SUBGROUPS)
    
    n.ages=length(parameters$AGES)
    
    state.length = prod(sapply(state.dim.names, length))
    
    state = array(y[1:state.length], 
                  dim = sapply(state.dim.names, length), 
                  dimnames = state.dim.names) #indexed [age, sex, subgroup, hiv-status]
    
    #incidence #indexed [age, sex, subgroup]
    #diagnoses #indexed [age, sex, subgroup]
    #hiv.mortality #indexed [age, sex, subgroup]
    #nonhiv.mortality #indexed [age, sex, subgroup]
    
    ##----------------------##
    ##-- SET UP DX ARRAYS --##
    ##----------------------##
    
    dx.state = array(0, 
                     dim = sapply(state.dim.names, length), 
                     dimnames = state.dim.names)#indexed [age, sex, subgroup, hiv-status]
    dx.incidence = array(0, 
                         dim = sapply(incidence.dim.names, length), 
                         dimnames = incidence.dim.names)#indexed [age, sex, subgroup]
    dx.diagnoses = array(0, 
                         dim = sapply(incidence.dim.names, length), 
                         dimnames = incidence.dim.names)#indexed [age, sex, subgroup]
    dx.hiv.mortality = array(0, 
                               dim = sapply(state.dim.names, length), 
                               dimnames = state.dim.names)#indexed [age, sex, subgroup, hiv-status]
    dx.nonhiv.mortality = array(0, 
                                dim = sapply(state.dim.names, length), 
                                dimnames = state.dim.names)#indexed [age, sex, subgroup, hiv-status]
    
    ##----------------------------------##
    ##-- COMPUTE THE CHANGES IN STATE --##
    ##----------------------------------##
    
    #-- CHANGES in AGE --#
    
    births = state * pp$FERTILITY.RATE
    births = apply(births, 3, sum)
    
    #births
    dx.state[1,'male',,'hiv.negative'] = births*pp$MALE.BIRTHS
    dx.state[1,'female',,'hiv.negative'] = births*(1-pp$MALE.BIRTHS)
    
    #aging
    ageing = state * pp$AGEING.RATE #indexed [age, sex, subgroup, hiv-status]
    dx.state = dx.state - ageing
    dx.state[-1,,,] = dx.state[-1,,,] + ageing[-n.ages,,,]
    
    dx.nonhiv.mortality[n.ages,,,] = dx.nonhiv.mortality[n.ages,,,] + ageing[n.ages,,,]
    
    #-- MORTALITY --#
    # hiv mortality 
    hiv.mortality = state * pp$HIV.MORTALITY.RATE #indexed [age, sex, subgroup, hiv-status]
    dx.state = dx.state - hiv.mortality
    dx.hiv.mortality = dx.hiv.mortality + hiv.mortality
    
    # non hiv mortality
    non.hiv.mortality = state * pp$NON.HIV.MORTALITY.RATE #indexed [age, sex, subgroup, hiv-status]
    dx.state = dx.state - non.hiv.mortality
    dx.non.hiv.mortality = dx.non.hiv.mortality + non.hiv.mortality
    
    #-- CHANGES in SEX --#
    # (assuming sex is immutable)
    
    #-- CHANGES in SUB-GROUP --#
    # (we are ignoring this for now)
    
    #-- CHANGES in HIV STATUS --#
    
    #-- DIAGNOSES --#
    diagnosed = pp$TESTING.RATES * state[,,,'undiagnosed']
    dx.state[,,,'undiagnosed'] = dx.state[,,,'undiagnosed'] - diagnosed
    dx.state[,,,'diagnosed_unengaged'] = dx.state[,,,'diagnosed_unengaged'] + diagnosed
    dx.diagnoses = dx.diagnosed + diagnosed

    #-- NEW INFECTIONS --#
    incidence = 10     # Using this for now
    dx.state[,,,'hiv_negative'] = dx.state[,,,'hiv_negative'] - incidence
    dx.state[,,,'undiagnosed'] = dx.state[,,,'undiagnosed'] + incidence
    dx.incidence = dx.incidence + incidence
    
    #-- ENGAGEMENT --#
    engaged = pp$ENGAGEMENT.RATES * state[,,,'diagnosed_unengaged']
    dx.state[,,,'diagnosed_unengaged'] = dx.state[,,,'diagnosed_unengaged'] - engaged
    dx.state[,,,'engaged_unsuppressed'] = dx.state[,,,'engaged_unsuppressed'] + engaged
    
    #-- SUPPRESSION --#
    suppressed = pp$SUPPRESSION.RATES * state[,,,'engaged_unsuppressed']
    dx.state[,,,'engaged_unsuppressed'] = dx.state[,,,'engaged_unsuppressed'] - suppressed
    dx.state[,,,'engaged_suppressed'] = dx.state[,,,'engaged_suppressed'] + suppressed
    
    #-- DISENGAGED FROM UNSUPPRESSED --#
    
    #-- DISENGAGED FROM SUPPRESSED --#
    
    #-- UNSUPPRESSION --#
    
    
    
    if (1==2)
    {
        #we need an array force of infection to each stratum of [age,sex,risk,subpop,hiv.status]
        for (a.to in parameters$AGES)
        {
            for (s.to in parameters$SEXES)
            {
                for (r.to in parameters$RISKS)
                {
                    for (p.to in parameters$SUBPOPULATIONS)
                    {
                        for (a.from in parameters$AGES)
                        {
                            for (s.from in parameters$SEXES)
                            {
                                for (r.from in parameters$RISKS)
                                {
                                    for (p.from in parameters$SUBPOPULATIONS)
                                    {
                                        dx.incidence[a.to, s.to, r.to, p.to] = dx.incidence[a.to, s.to, r.to, p.to] +
                                            proportion.of.tos.partners.in.from * transmission.rate * prevalence.of.infections.in.from
                                    }
                                }
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
    
    #return a vector of length = length(y) that represents the change in each element in y
    c(as.numeric(dx.state), as.numeric(dx.incidence), as.numeric(dx.diagnoses), as.numeric(dx.hiv.mortality), as.numeric(dx.non.hiv.mortality))
    
    
    #flatten out all our arrays
}

run.model <- function(parameters,
                      start.state,
                      start.year,
                      end.year)
{
    #This is just a stub
    ode.results = odeintr::integrate_sys(sys=function(x,t){model.dx.Rcpp(time=t,y=x,parameters=parameters)},
                                         init=init,
                                         start=start.year,
                                         duration=end.year+1-start.year,
                                         step_size=1)
    
    # We need to process the results
    process.ode.results(ode.results)
}

# break out the epi data we want
process.ode.results <- function(ode.results)
{
    ode.results #for now
}