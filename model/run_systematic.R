################################################################################################
# Description: Code that runs from initializing parameters all the way through simulation
################################################################################################

# Functions 
#     1. run.model.for.parameters 
#     2. extract.hiv.data.for.ncd 

source('source_code.R')

## Added this in so that parameters objects exists without running a sim - need this for likelihood functions 
if(1==2){
    parameters = create.model.parameters()
    parameters = map.model.parameters(parameters)
}




# Single function that is analogous to all the code in the test_case file, but also allows for
# easier manipulation of sampled parameters
#     1. Creates basic model parameters; maps all parameters to structure needed for diffeq;
#         sets up the initial state; runs the model
#     2. Can be passed variable.parameters as an input to override default sampled parameters
#     3. Called in parameter_optim code

## Run model with sampled parameters
run.model.for.parameters = function(variable.parameters,
                                    parameters=create.model.parameters(),
                                    end.year=2021,
                                    interventions=NO.INTERVENTION){
    
    sampled.parameters = get.default.parameters()
    
    invalid.names = setdiff(names(variable.parameters), names(sampled.parameters))
    if(length(invalid.names)>0)
        stop(paste0("Invalid parameters passed: ",paste0(invalid.names,collapse = ", ")))
    
    sampled.parameters[names(variable.parameters)]=variable.parameters #overwrite with whatever new parameters we want
    
    parameters = map.model.parameters(parameters,
                                      sampled.parameters=sampled.parameters,
                                      interventions=interventions)
    
    state.dim.names = list(age=parameters$AGES, 
                           sex=parameters$SEXES,
                           subgroup=parameters$SUBGROUPS,
                           hiv.status=parameters$HIV.STATUS)
    
    initial.state = get.initial.population(year = "1970", 
                                           data.manager = DATA.MANAGER, 
                                           parameters = parameters,
                                           model.age.cutoffs = MODEL.AGE.CUTOFFS, 
                                           ages = parameters$AGES, 
                                           sexes = parameters$SEXES, 
                                           seed.to.ages = c(5,6,7,8), 
                                           seed.to.sexes = c(1,2), 
                                           seed.n = 1)
    
    save(parameters,variable.parameters,file="calibration/debug.parameters.Rdata")
    
    sim = run.model(parameters=parameters,
                    initial.state=initial.state,
                    start.year=1970, # later make these arguments that I pass to the function, with these as defaults 
                    end.year=end.year,
                    keep.years=c(1970:2030))
    
    sim$parameters = parameters
    
    sim
    
}


variable.parameters=get.default.parameters()
# Transmission parameters
variable.parameters['trate.0']=.5 # 1990
variable.parameters['trate.1']=0.045 # 1997
variable.parameters['trate.2']=0.04 # 2008, keep this the same as trate.1
variable.parameters['trate.3']=0.08 # 2015
variable.parameters['female.to.male.multiplier']=2
variable.parameters['age.15.to.19.transmission.multiplier']=.8
variable.parameters['age.20.to.29.transmission.multiplier']=1
variable.parameters['age.40.to.49.transmission.multiplier']=1
variable.parameters['age.50.and.over.transmission.multiplier.0']=.5
variable.parameters['age.50.and.over.transmission.multiplier.1']=1.2
variable.parameters['age.50.and.over.transmission.multiplier.2']=.8
variable.parameters['age.50.and.over.transmission.multiplier.3']=.5
variable.parameters['age.assortativity']=.8
variable.parameters['birth.transmission.risk.0']=.7
# Cascade parameters
# variable.parameters['testing.rate.1']=0.5
# variable.parameters['engagement.rate.2']=1.5
variable.parameters['suppression.rate.0']=0.7
variable.parameters['suppression.rate.1']=4
variable.parameters['unsuppression.rates']=.05
variable.parameters['male.cascade.multiplier']=.7
# Mortality parameters
variable.parameters['age.45.to.65.mortality.intercept.multiplier']= 2.3 # multiplies intercept or slope before projecting
variable.parameters['age.45.to.65.mortality.slope.multiplier']= 1.01
variable.parameters['over.65.mortality.intercept.multiplier']= 1.0
variable.parameters['over.65.mortality.slope.multiplier']= 1.01

variable.parameters['hiv.specific.mortality.rates.0']=0.04
variable.parameters['hiv.specific.mortality.rates.1']=0.07
variable.parameters['hiv.specific.mortality.rates.2']=0.018


variable.parameters['age.15.to.24.hiv.mortality.multiplier']= 0.4
variable.parameters['over.50.hiv.mortality.multiplier']= .9
variable.parameters['age.0.to.14.hiv.mortality.multiplier.0']=12


if(1==2){
    sim = run.model.for.parameters(variable.parameters = variable.parameters)
    sim = run.model.for.parameters(variable.parameters = params.start.values)
}


## Plot results
if(1==2){
    ## Population
    print(simplot(sim,
                  years=c(1980:2020),
                  data.types = c("population"),
                  facet.by = 'age'))
    
    ## Incidence
    print(simplot(sim,
                  years=c(1980:2020),
                  data.types = c("incidence"),
                  facet.by = 'age'))
    print(simplot(sim,
                  years=c(1980:2020),
                  data.types = c("incidence"),
                  ages = "15+",
                  facet.by = c("age",'sex')))
    print(simplot(sim1,sim2,
                  years=c(1980:2020),
                  data.types = c("incidence"))) 
    
    ## Prevalence
    print(simplot(sim,
                  years=c(1980:2020),
                  data.types = c("prevalence"),
                  facet.by = 'age'))

    ## HIV mortality
    print(simplot(sim,
                  years=c(1980:2020),
                  data.types = c("hiv.mortality"),
                  facet.by = "age",
                  proportion = T)) # this one doesn't work anymore
    print(simplot(sim,
                  years=c(1980:2020),
                  data.types = c("hiv.mortality"),
                  facet.by = "age",
                  proportion = F)) # likelihood is absolute value so use this one for testing 
    
    ## Cascade
    print(simplot(sim,
                  years=c(1980:2020),
                  data.types = c("awareness","engagement","suppression"), 
                  proportion = T,
                  facet.by = c("age","sex")))
    
    ## Awareness - default denominator is PLHIV 
    print(simplot(sim,
                  years=c(1980:2020),
                  data.types = c("awareness"),
                  proportion = T,
                  facet.by = c("age","sex")))
    
    ## Engagement - default denominator is total aware
    print(simplot(sim1, sim2,
                  years=c(1980:2020),
                  data.types = c("engagement"),
                  proportion = T,
                  facet.by = c("age","sex"))) 
    print(simplot(sim,
                  years=c(1980:2020),
                  data.types = c("engagement"),
                  proportion = T) + geom_vline(xintercept = c(1986,2014,2016)-1) + ylim(0,100))
    
    ## Suppression - default denominator is total aware
    print(simplot(sim1, sim2,
                  years=c(1980:2020),
                  data.types = c("suppression"),
                  proportion = T,
                  facet.by = c("age","sex")))
    
    print(simplot(sim,
                  years=c(1980:2020),
                  data.types = c("engagement","suppression"),
                  proportion = T,
                  facet.by = c("age","sex")))
}
   

  
