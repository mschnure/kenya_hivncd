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
    
    state #indexed [age, sex, subgroup, hiv-status]
    incidence #indexed [age, sex, subgroup]
    diagnoses #indexed [age, sex, subgroup]
    hiv.mortality #indexed [age, sex, subgroup]
    nonhiv.mortality #indexed [age, sex, subgroup]
    
    ##----------------------##
    ##-- SET UP DX ARRAYS --##
    ##----------------------##
    
    dx.state #indexed [age, sex, subgroup, hiv-status]
    dx.incidence
    dx.diagnoses
    dx.hiv.mortality
    dx.nonhiv.mortality
    
    ##----------------------------------##
    ##-- COMPUTE THE CHANGES IN STATE --##
    ##----------------------------------##
    
    #-- CHANGES in AGE --#
    
    #births
    dx.state[1,,,] = births
    
    #aging
    ageing = state * pp$AGEING.RATE #indexed [age, sex, subgroup, hiv-status]
    dx.state = dx.state - ageing
    dx.state[-1,,,] = dx.state[-1,,,] + ageing[-n.ages,,,]
    
    dx.nonhiv.mortality[n.ages,,,] = dx.nonhiv.mortality[n.ages,,,] + ageing[n.ages,,,]
    
    #-- MORTALITY --#
    
    #-- CHANGES in SEX --#
    # (assuming sex is immutable)
    
    #-- CHANGES in SUB-GROUP --#
    # (we are ignoring this for now)
    
    #-- CHANGES in HIV STATUS --#
    
    diagnosed = pp$testing.rates * dx.state[,,,'undiagnosed']
    dx.state[,,,'undiagnosed'] = dx.state[,,,'undiagnosed'] - diagnosed
    dx.state[,,,'diagnosed_unsuppressed'] = dx.state[,,,'diagnosed_unsuppressed'] + diagnosed
    dx.diagnoses = dx.diagnosed + diagnosed
    
    #@melissa - fill in here
    
    #-- NEW INFECTIONS --#
    # SAVED FOR LATER #
    
    incidence = 10
    
    
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