# array set up
# [age,sex,risk,subpopulation,hiv.status]

library(odeintr)


compute.dx <- function(time, y, parameters)
{
    ##---------------------------------##
    ##-- GET TIME-VARYING PARAMETERS --##
    ##---------------------------------##
    
    pp = compute.time.varying.parameters(parameters, time)
    # now pp is a list
    
    ##---------------------------##
    ##-- PULL THE MODEL STATE  --##
    ##---------------------------##
    
    state.dim.names = list(age=parameters$AGES,
                           sex=parameters$SEXES,
                           risk=parameters$RISKS,
                           subpopulation=parameters$SUBPOPULATIONS,
                           hiv.status=parameters$HIV.STATUS)
    state.dim = sapply(state.dim.names, length)
    state.length = prod(state.dim)
    
    state = array(y[1:state.length],
                  dim=state.dim,
                  dimnames = state.dim.names)
    
    ##----------------------##
    ##-- SET UP DX ARRAYS --##
    ##----------------------##
    
    dx.state = array(0, dim=state.dim, dimnames=state.dim.names)
    
    incidence.dim.names = list(age=parameters$AGES,
                               sex=parameters$SEXES,
                               risk=parameters$RISKS,
                               subpopulation=parameters$SUBPOPULATIONS)
    dx.incidence = array(0, dim=sapply(incidence.dim.names, length), dimnames=incidence.dim.names)
    
    dx.nonhiv.mortality = array(0, dim=state.dim, dimnames=state.dim.names)
    
    hiv.mortality.dim.names = list(age=parameters$AGES,
                                   sex=parameters$SEXES,
                                   risk=parameters$RISKS,
                                   subpopulation=parameters$SUBPOPULATIONS,
                                   hiv.status=setdiff(parameters$HIV.STATUS, 'uninfected'))
    dx.hiv.mortality = array(0, dim=state.dim, dimnames=state.dim.names)
    
    ##------------------------------------------##
    ##-- COMPUTE THE CHANGES IN STATE/TALLIES --##
    ##------------------------------------------##
    
    #-- CHANGES BY AGE --#
    
    #-- CHANGES IN SEX --#
    # (assuming sex is immutable)
    
    #-- CHANGES IN RISK --#
    # (we are ignoring this for now)
    
    #-- CHANGES IN SUBPOPULATION --#
    # (we are ignoring this for now)
    
    #-- NEW INFECTIONS --#
    
    
    number.of.infections.in.to = sum_over_all_strata_from { infections.from.from.to.to }
    
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
    
    #-- DIAGNOSES --#
    
    #-- SUPPRESSION/UNSUPPRESSION --#
    
    #-- HIV MORTALITY --#
    dx.hiv.mortality = state[,,,,c('undiagnosed','diagnosed.unsuppressed','diagnosed.suppressed')] * pp$hiv.mortality
    dx.state[,,,,c('undiagnosed','diagnosed.unsuppressed','diagnosed.suppressed')] = dx.state[,,,,c('undiagnosed','diagnosed.unsuppressed','diagnosed.suppressed')] - dx.hiv.mortality
    
    #-- NON-HIV MORALITY --#
    dx.nonhiv.mortality = state * pp$general.mortality.rate
    dx.state = dx.state - dx.nonhiv.mortality
    
    ##------------------------------##
    ##-- PACKAGE IT UP AND RETURN --##
    ##------------------------------##
    
    #return a vector of length = length(y) that represents the change in each element in y
    c(as.numeric(dx.state),
      as.numeric(dx.incidence),
      as.numeric(dx.nonhiv.mortality),
      as.numeric(dx.hiv.mortality))
}

run.model <- function(parameters,
                      start.state)
{
        #This is just a stub
        odeintr::integrate_sys(sys=function(x,t){model.dx.Rcpp(time=t,y=x,parameters=parameters)},
                               init=init,
                               start=start.year,
                               duration=end.year+1-start.year,
                               step_size=1)
}