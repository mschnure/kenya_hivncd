################################################################################################
# Description: Code that runs from initializing parameters all the way through simulation
################################################################################################

source('source_code.R')

# Single function that is analogous to all the code in the test_case file, but also allows for
# easier manipulation of sampled parameters
#     1. Creates basic model parameters; maps all parameters to structure needed for diffeq;
#         sets up the initial state; runs the model
#     2. Can be passed variable.parameters as an input to override default sampled parameters
#     3. Called in parameter_optim code
run.model.for.parameters = function(variable.parameters,
                                    parameters=create.model.parameters()){
    
    sampled.parameters = get.default.parameters()
    
    invalid.names = setdiff(names(variable.parameters), names(sampled.parameters))
    if(length(invalid.names)>0)
        stop(paste0("Invalid parameters passed: ",paste0(invalid.names,collapse = ", ")))
    
    sampled.parameters[names(variable.parameters)]=variable.parameters #overwrite with whatever new parameters we want
    
    parameters = map.model.parameters(parameters,
                                      sampled.parameters=sampled.parameters)
    
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
                                           seed.to.ages = c(4,5,6), 
                                           seed.to.sexes = c(1,2), 
                                           seed.n = 1)
    
    sim = run.model(parameters=parameters,
                    initial.state=initial.state,
                    start.year=1970, # later make these arguments that I pass to the function, with these as defaults 
                    end.year=2020,
                    keep.years=c(1970:2020))
    
    sim$parameters = parameters
    
    sim
    
}


if(1==2){
    
    variable.parameters=get.default.parameters()
    variable.parameters['suppression.rate.0']=0.6
    variable.parameters['testing.rate.0']=0.4
    variable.parameters['trate.0']=.85
    variable.parameters['trate.1']=0.2
    variable.parameters['age.50.and.over.transmission.multiplier']=0.4
    variable.parameters['age.50.to.79.mortality.multiplier']=1.6
    variable.parameters['over.80.mortality.multiplier']=2
    variable.parameters['birth.transmission.risk']=.8
    sim = run.model.for.parameters(variable.parameters = variable.parameters)
    
    print(simplot(sim,
                  years=c(1980:2020),
                  data.types = c("incidence"),
                  facet.by = 'age')
    )
    
    print(simplot(sim,
                  years=c(1980:2020),
                  data.types = c("prevalence"),
                  facet.by = 'age')
    )
    
    print(simplot(sim,
                  years=c(1980:2020),
                  data.types = c("prevalence"),
                  facet.by = 'age',
                  ages = c('10-14','15-19'))+ geom_hline(yintercept=130000)
    )
    
    print(simplot(sim,
                  years=c(1980:2020),
                  data.types = c("incidence"),
                  facet.by = 'sex')
    )
    
    print(simplot(sim,
                  years=c(1980:2020),
                  data.types = c("incidence"))
    )
    
    print(simplot(sim,
                  years=c(1980:2020),
                  data.types = c("population"),
                  facet.by = 'age')
    )
    
}

