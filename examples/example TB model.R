library(deSolve)



##------------##
##-- STATES --##
##------------##

UNINFECTED = 1
RECOVERED = 2
LTBI.1 = 3
LTBI.2 = 4
ACTIVE.TB =  5

CUMULATIVE.INCIDENCE.FROM.PRIMARY = 6
CUMULATIVE.INCIDENCE.FROM.LATENT.1 = 7
CUMULATIVE.INCIDENCE.FROM.LATENT.2 = 8
CUMULATIVE.TB.MORTALITY = 9

NUM.STATES = 9


##--------------------------##
##--  THE DELTA FUNCTION  --##
##-- (for the ODE solver) --##
##--------------------------##

# parameters should be a matrix, with one row for each age stratum, and columns:
# $transmission.rate.1980
# $annual.transmission.decline
# $primary.progression.risk
# $latent.protection.factor
# $reactivation.rate.1
# $reactivation.rate.2
# $ltbi.1.length
# $treatment.initiation.rate
# $treatment.success.proportion
# $self.cure.rate
# $population.size
# $mortality.rate
# $tb.mortality
# $age.stratum.length
dx.model <- function(t, y, parameters)
{
    num.ages = dim(parameters)[2]
    input = matrix(y, ncol=num.ages)
    delta = matrix(0, ncol=ncol(input), nrow = nrow(input))

    transmission.rates = parameters['transmission.rate.1980',] * parameters['annual.transmission.decline',] ^ max(0, t-1980)
    non.tally.states = setdiff(1:nrow(input), c(CUMULATIVE.INCIDENCE.FROM.PRIMARY, CUMULATIVE.INCIDENCE.FROM.LATENT.1, CUMULATIVE.INCIDENCE.FROM.LATENT.2, CUMULATIVE.TB.MORTALITY))
    total.pop = colSums(input[non.tally.states,])
    num.active = input[ACTIVE.TB,]
    force.of.infection = sum(num.active * transmission.rates) / sum(total.pop)

    for (age in 1:num.ages)
    {
        #from uninfected
        uninfected.to.ltbi.1 = input[UNINFECTED, age] * force.of.infection * (1-parameters['primary.progression.risk',age])
        uninfected.to.active = input[UNINFECTED, age] * force.of.infection * parameters['primary.progression.risk',age]
        
        #from recovered
        recovered.to.ltbi.1 = input[RECOVERED, age] * force.of.infection * (1-parameters['primary.progression.risk',age]) * parameters['latent.protection.factor',age]
        recovered.to.active = input[RECOVERED, age] * force.of.infection * parameters['primary.progression.risk',age] * parameters['latent.protection.factor',age]

        #from ltbi
        ltbi.1.reactivation = input[LTBI.1, age] * parameters['reactivation.rate.1',age]
        ltbi.1.primary = input[LTBI.1, age] * parameters['latent.protection.factor',age] * force.of.infection * parameters['primary.progression.risk',age]
        ltbi.2.reactivation = input[LTBI.2, age] * parameters['reactivation.rate.2',age]
        ltbi.2.primary = input[LTBI.2, age] * parameters['latent.protection.factor',age] * force.of.infection * parameters['primary.progression.risk',age]
        
        ltbi.1.to.2 = input[LTBI.1, age] / parameters['ltbi.1.length',age]
        ltbi.2.to.1 = input[LTBI.2, age] * parameters['latent.protection.factor',age] * force.of.infection * (1 - parameters['primary.progression.risk',age])
        
        ltbi.1.to.recovered = input[LTBI.1, age] * parameters['ipt.rate',age] * parameters['ipt.success.proportion',age]
        ltbi.2.to.recovered = input[LTBI.2, age] * parameters['ipt.rate',age] * parameters['ipt.success.proportion',age]
        
        #from active
        active.to.recovered = input[ACTIVE.TB, age] * parameters['treatment.success.proportion',age] * 
                                (parameters['treatment.initiation.rate',age] + parameters['active.screening.rate',age])
        active.to.ltbi.1 = input[ACTIVE.TB, age] * parameters['self.cure.rate',age]
        
        #mortality
        uninfected.mortality = input[UNINFECTED,age] * parameters['mortality.rate',age]
        recovered.mortality = input[RECOVERED,age] * parameters['mortality.rate',age]
        ltbi.1.mortality = input[LTBI.1,age] * parameters['mortality.rate',age]
        ltbi.2.mortality = input[LTBI.2,age] * parameters['mortality.rate',age]
        active.general.mortality = input[ACTIVE.TB,age] * parameters['mortality.rate',age]
        tb.mortality = input[ACTIVE.TB,age] * parameters['tb.mortality',age]
      
        #deltas, disregarding aging
        delta[CUMULATIVE.INCIDENCE.FROM.PRIMARY, age] = uninfected.to.active + recovered.to.active + ltbi.1.primary + ltbi.2.primary
        delta[CUMULATIVE.INCIDENCE.FROM.LATENT.1, age] = ltbi.1.reactivation
        delta[CUMULATIVE.INCIDENCE.FROM.LATENT.2, age] = ltbi.2.reactivation
        delta[CUMULATIVE.TB.MORTALITY, age] = tb.mortality
        
        delta[UNINFECTED, age] = -uninfected.to.ltbi.1 - uninfected.to.active - uninfected.mortality
        delta[RECOVERED, age] = ltbi.1.to.recovered + ltbi.2.to.recovered + active.to.recovered - 
            recovered.to.ltbi.1 - recovered.to.active - recovered.mortality
        
        delta[ACTIVE.TB, age] = uninfected.to.active + recovered.to.active + 
            ltbi.1.primary + ltbi.1.reactivation + ltbi.2.primary + ltbi.2.reactivation -
            active.to.recovered - active.to.ltbi.1 - tb.mortality - active.general.mortality
        
        delta[LTBI.1, age] = uninfected.to.ltbi.1 + recovered.to.ltbi.1 + ltbi.2.to.1 + active.to.ltbi.1 - 
            ltbi.1.to.2 - ltbi.1.primary - ltbi.1.reactivation - ltbi.1.mortality
        
        delta[LTBI.2, age] = ltbi.1.to.2 - ltbi.2.to.1 - ltbi.2.primary - ltbi.2.reactivation - ltbi.2.mortality
    }
    
    #add in aging
    for (age in 1:(num.ages-1))
    {
        age.rate = 1 / parameters['age.stratum.length', age]
        
        delta[non.tally.states, age] = delta[non.tally.states, age] - age.rate * input[non.tally.states, age]
        delta[non.tally.states, age+1] = delta[non.tally.states, age+1] + age.rate * input[non.tally.states, age]
    }
 
    #assume that outflows from the first age brackets are all balanced by births
    # (ie, new uninfected)
    delta[UNINFECTED, 1] = delta[UNINFECTED, 1] + parameters['population.size', 1] - sum(input[non.tally.states,1] + delta[non.tally.states,1])
 

    
    list(as.numeric(delta))
}
  

##-------------------------------##
##-- SETTING THE INITIAL STATE --##
##-------------------------------##

create.initial.state <- function(cases.per.age=c(0,0,1,0,0), population.sizes)
{
    rv = matrix(0, nrow=NUM.STATES, ncol=length(cases.per.age))
    rv[UNINFECTED,] = population.sizes - cases.per.age
    rv[ACTIVE.TB,] = cases.per.age
    
    as.numeric(rv)
}

##---------------------------------------------------------##
##-- MAPPING PARAMETER LIST TO THE AGE-STRATIFIED MATRIX --##
##---------------------------------------------------------##

#param.list should have the following elements (in no particular order)
# NOT USING (pegged to zero): $transmission.rate.1980.young
# $transmission.rate.1980.old
# $annual.transmission.decline
# $latent.protection.factor
# $primary.progression.risk.young
# $reactivation.rate.1.young
# $reactivation.rate.2.young
# $primary.progression.risk.old
# $reactivation.rate.1.old
# $reactivation.rate.2.old
# $treatment.initiation.rate
# $treatment.success.proportion
# $ipt.success.proportion
# $self.cure.rate
# $tb.mortality
# $ipt.rate
# $active.screening.rate

map.parameters <- function(param.list, 
                           population.sizes,
                           ltbi.1.length=5,
                           age.strata.lengths=c(15,15,15,15,NA),
                           age.specific.mortality.rates,
                           active.screening.rate = 0,
                           ipt.rate = 0,
                           ipt.success.proportion = 0)
{
    num.strata = length(age.strata.lengths)

    rv = suppressWarnings(data.frame(transmission.rate.1980 = c(0, rep(param.list['transmission.rate.1980.old'], num.strata-1)),
                    annual.transmission.decline = param.list['annual.transmission.decline'],
                    latent.protection.factor = param.list['latent.protection.factor'],
                    primary.progression.risk = c(param.list['primary.progression.risk.young'], rep(param.list['primary.progression.risk.old'], num.strata-1)),
                    reactivation.rate.1 = c(param.list['reactivation.rate.1.young'], rep(param.list['reactivation.rate.1.old'], num.strata-1)),
                    reactivation.rate.2 = c(param.list['reactivation.rate.2.young'], rep(param.list['reactivation.rate.2.old'], num.strata-1)),
                    ltbi.1.length = ltbi.1.length,
                    treatment.initiation.rate = param.list['treatment.initiation.rate'],
                    treatment.success.proportion = param.list['treatment.success.proportion'],
                    ipt.success.proportion = ipt.success.proportion,
                    self.cure.rate = param.list['self.cure.rate'],
                    population.size = population.sizes,
                    mortality.rate = age.specific.mortality.rates,
                    tb.mortality = param.list['tb.mortality'],
                    age.stratum.length = age.strata.lengths,
                    ipt.rate = ipt.rate,
                    active.screening.rate = active.screening.rate
    ))
    t(as.matrix(rv))
}


##-----------------------##
##-- RUNNING THE MODEL --##
##-----------------------##

RUN.IN.YEARS = 500
START.YEAR = 1980
END.YEAR = 2040
#INTERVENTION.YEAR = 2020

run.model <- function(parameters,
                      age.cutoffs,
                      population.sizes,
                      age.specific.mortality.rates,
                      keep.years = START.YEAR:END.YEAR)
{
    orig.parameters = parameters
    parameters = map.parameters(parameters, 
                                population.sizes=population.sizes,
                                age.specific.mortality.rates=age.specific.mortality.rates,
                                active.screening.rate = 0,
                                ipt.rate = 0)

    times = unique(c(START.YEAR-RUN.IN.YEARS, min(keep.years)-1, keep.years, END.YEAR))
    start.year=times[1]
    end.year=times[length(times)]
    intervention.times = (INTERVENTION.YEAR - 1):end.year

#SHALOM THIS CODE RUNS THE ODE SOLVER    
    ode.output = ode(create.initial.state(population.sizes=population.sizes), times, dx.model, parameters)

    results = list(base.data=process.results(ode.output, age.cutoffs, keep.years),
                   years=times,
                   num.age.brackets = length(age.cutoffs)+1,
                   age.cutoffs = age.cutoffs,
                   age.bracket.names = c(paste0('<', age.cutoffs[1]),
                                         paste0(age.cutoffs[-length(age.cutoffs)], '-', age.cutoffs[-1]),
                                         paste0('<', age.cutoffs[length(age.cutoffs)])),
                   start.year=start.year,
                   intervention.data.start.year = intervention.times[1],
                   intervention.year = INTERVENTION.YEAR,
                   end.year=end.year,
                   parameters = orig.parameters,
                   population.sizes = population.sizes,
                   age.specific.mortality.rates = age.specific.mortality.rates
                   )

 #   results$intervention.data[1, 1+c(CUMULATIVE.INCIDENCE, CUMULATIVE.TB.MORTALITY)] = results$base.data[before.intervention.index, 1+c(CUMULATIVE.INCIDENCE, CUMULATIVE.TB.MORTALITY)]
    
    results
}

run.model.interventions <- function(results,
                                    acf.rates,
                                    ipt.rates,
                                    ipt.success.proportion=0.5,
                                    first.intervention.year=2021,
                                    last.intervention.year=first.intervention.year+4,
                                    end.year=last.intervention.year+15)
{
    #save the rates in our result object
    results$acf.rates = acf.rates
    results$ipt.rates = ipt.rates
    
    
    #Run ACF simulations
    results$acf.results = lapply(acf.rates, function(acf.rate){
        do.run.one.intervention.and.post(results, 
                                         acf.rate = acf.rate,
                                         ipt.rate = 0,
                                         ipt.success.proportion = 0,
                                         first.intervention.year = first.intervention.year,
                                         last.intervention.year = last.intervention.year,
                                         end.year = end.year)
    })
    names(results$acf.results) = as.character(acf.rates)
    
    #Run IPT + ACF simulations
    results$ipt.results = lapply(ipt.rates, function(ipt.rate){
        do.run.one.intervention.and.post(results, 
                                         acf.rate = ipt.rate,
                                         ipt.rate = ipt.rate,
                                         ipt.success.proportion = ipt.success.proportion,
                                         first.intervention.year = first.intervention.year,
                                         last.intervention.year = last.intervention.year,
                                         end.year = end.year)
    })
    names(results$ipt.results) = as.character(ipt.rates)

    #Return the results
    results
}

do.run.one.intervention.and.post <- function(results,
                                             acf.rate,
                                             ipt.rate,
                                             ipt.success.proportion,
                                             first.intervention.year,
                                             last.intervention.year,
                                             end.year)
{
    #Set up times and state
    intervention.times = (first.intervention.year - 1):last.intervention.year
    post.intervention.times = last.intervention.year:end.year
    initial.state = as.numeric(results$base.data[as.character(first.intervention.year-1),1:NUM.STATES,])
    
    params = map.parameters(results$parameters, 
                            population.sizes=results$population.sizes,
                            age.specific.mortality.rates=results$age.specific.mortality.rates,
                            active.screening.rate = acf.rate,
                            ipt.rate = ipt.rate,
                            ipt.success.proportion = ipt.success.proportion)
    
    ode.output1 = ode(initial.state, intervention.times, dx.model, params)
    processed.1 = process.results(ode.output1, results$age.cutoffs, keep.years=intervention.times[-1])
    mid.state = as.numeric(processed.1[as.character(last.intervention.year),1:NUM.STATES,])
    
    params = map.parameters(results$parameters, 
                            population.sizes=results$population.sizes,
                            age.specific.mortality.rates=results$age.specific.mortality.rates,
                            active.screening.rate = 0,
                            ipt.rate = 0)
    
    ode.output2 = ode(mid.state, post.intervention.times, dx.model, params)
    processed.2 = process.results(ode.output2, results$age.cutoffs, keep.years=post.intervention.times[-1])
    
    rv = array(0, dim=c(length(intervention.times)+length(post.intervention.times)-1, dim(processed.1)[2:3]),
               dimnames = c(list(as.character(c(intervention.times, post.intervention.times[-1]))), dimnames(processed.1)[2:3]))
    
    rv[as.character(first.intervention.year-1),,] = results$base.data[as.character(first.intervention.year-1),,] 
    rv[as.character(intervention.times[-1]),,] = processed.1
    rv[as.character(post.intervention.times[-1]),,] = processed.2

    rv
}

#converts the results matrix into a 3-d array structured by year, state, and age
#also, converts cumulative incidence and mortality into yearly
process.results <- function(results, age.cutoffs, keep.years)
{
    n = dim(results)[1]
    num.ages = (dim(results)[2]-1) / NUM.STATES
    
    age.bracket.names = c(paste0('<', age.cutoffs[1]),
                          paste0(age.cutoffs[-length(age.cutoffs)], '-', age.cutoffs[-1]),
                          paste0('>', age.cutoffs[length(age.cutoffs)])
    )
    
    state.names = character(NUM.STATES)
    state.names[CUMULATIVE.INCIDENCE.FROM.PRIMARY] = 'incidence.from.primary'
    state.names[CUMULATIVE.INCIDENCE.FROM.LATENT.1] = 'incidence.from.latent.1'
    state.names[CUMULATIVE.INCIDENCE.FROM.LATENT.2] = 'incidence.from.latent.2'
    state.names[CUMULATIVE.TB.MORTALITY] = 'tb.mortality'
    state.names[UNINFECTED] = 'uninfected'
    state.names[RECOVERED] = 'recovered'
    state.names[LTBI.1] = 'ltbi.1'
    state.names[LTBI.2] = 'ltbi.2'
    state.names[ACTIVE.TB] = 'active.tb'

    results.3d = array(results[,-1], #get rid of the year column
                 dim=c(n, NUM.STATES, num.ages),
                 dimnames = list(as.character(results[,1]), state.names, age.bracket.names))
    
    
    revised.state.names = state.names[c(UNINFECTED, RECOVERED, LTBI.1, LTBI.2, ACTIVE.TB, CUMULATIVE.INCIDENCE.FROM.PRIMARY, CUMULATIVE.INCIDENCE.FROM.LATENT.1, CUMULATIVE.INCIDENCE.FROM.LATENT.2, CUMULATIVE.TB.MORTALITY)]
    non.tally.state.names = state.names[c(UNINFECTED, RECOVERED, LTBI.1, LTBI.2, ACTIVE.TB)] #ie, the non-cumulative ones
    rv = array(NA,
               dim=c(n, NUM.STATES+2, num.ages),
               dimnames = list(dimnames(results.3d)[[1]], c(revised.state.names, 'total.incidence', 'population.size'), age.bracket.names))
    rv[,revised.state.names,] = results.3d[,revised.state.names,]
    rv[,'population.size',] = apply(results.3d[,non.tally.state.names,], c(1,3), sum)
    
    rv[2:n,'incidence.from.primary',] = rv[2:n,'incidence.from.primary',] - rv[1:(n-1),'incidence.from.primary',]
    rv[1,'incidence.from.primary',] = NA
    rv[2:n,'incidence.from.latent.1',] = rv[2:n,'incidence.from.latent.1',] - rv[1:(n-1),'incidence.from.latent.1',]
    rv[1,'incidence.from.latent.1',] = NA
    rv[2:n,'incidence.from.latent.2',] = rv[2:n,'incidence.from.latent.2',] - rv[1:(n-1),'incidence.from.latent.2',]
    rv[1,'incidence.from.latent.2',] = NA
    
    rv[,'total.incidence',] = rv[,'incidence.from.primary',] + rv[,'incidence.from.latent.1',] + rv[,'incidence.from.latent.2',]
    
    rv[2:n,'tb.mortality',] = rv[2:n,'tb.mortality',] - rv[1:(n-1),'tb.mortality',]
    rv[1,'tb.mortality',] = NA

    rv[as.character(keep.years),,]
}
