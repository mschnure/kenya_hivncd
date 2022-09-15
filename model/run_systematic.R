################################################################################################
# Description: Code that runs from initializing parameters all the way through simulation
################################################################################################

# Functions 
#     1. run.model.for.parameters 
#     2. extract.hiv.data.for.ncd 

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
                                           seed.to.ages = c(5,6,7,8), 
                                           seed.to.sexes = c(1,2), 
                                           seed.n = 1)
    
    sim = run.model(parameters=parameters,
                    initial.state=initial.state,
                    start.year=1970, # later make these arguments that I pass to the function, with these as defaults 
                    end.year=2030,
                    keep.years=c(1970:2030))
    
    sim$parameters = parameters
    
    sim
    
}


## Run model with sampled parameters
variable.parameters=get.default.parameters()
variable.parameters['suppression.rate.0']=0.6
variable.parameters['testing.rate.1']=0.5
variable.parameters['trate.0']=.77
variable.parameters['trate.1']=0.25
variable.parameters['age.15.to.19.transmission.multiplier']=.66
variable.parameters['age.20.to.29.transmission.multiplier']=1.3
variable.parameters['age.40.to.49.transmission.multiplier']=1.1
variable.parameters['age.50.and.over.transmission.multiplier']=0.5
variable.parameters['female.to.male.multiplier']=1.03
variable.parameters['age.50.to.79.mortality.multiplier']=2.1
variable.parameters['over.80.mortality.multiplier']=1
variable.parameters['birth.transmission.risk']=.6
variable.parameters['age.assortativity']=.8
sim = run.model.for.parameters(variable.parameters = variable.parameters)


## Extracts outputs needed for NCD model
extract.hiv.data.for.ncd = function(sim,
                                    years){
    
    # These will be combined into one "disengagement" output (Parastu's request)
    disengagement.suppressed = extract.data(sim, 
                                            data.type = "disengagement.suppressed",
                                            years = years,
                                            keep.dimensions = c("year","age","sex"))
    disengagement.unsuppressed = extract.data(sim, 
                                              data.type = "disengagement.unsuppressed",
                                              years = years,
                                              keep.dimensions = c("year","age","sex"))
    
    rv = list()
    rv$population = extract.data(sim, 
                                 data.type = "population",
                                 years = years,
                                 keep.dimensions = c("year","age","sex","hiv.status"))
    rv$incidence = extract.data(sim, 
                                data.type = "incidence",
                                years = years,
                                keep.dimensions = c("year","age","sex"))
    rv$hiv.mortality = extract.data(sim, 
                                    data.type = "hiv.mortality",
                                    years = years,
                                    keep.dimensions = c("year","age","sex"))
    rv$diagnosis = extract.data(sim, 
                                data.type = "diagnoses",
                                years = years,
                                keep.dimensions = c("year","age","sex"))
    rv$engagement = extract.data(sim, 
                                 data.type = "annual.engagement",
                                 years = years,
                                 keep.dimensions = c("year","age","sex"))
    rv$disengagement = disengagement.suppressed + disengagement.unsuppressed
    rv$suppression = extract.data(sim,
                                  data.type = "annual.suppression",
                                  years = years,
                                  keep.dimensions = c("year","age","sex"))
    
    rv
}

hiv.output.for.ncd = extract.hiv.data.for.ncd(sim=sim,years = 2010:2030)




if(1==2){
    ## Plot results
    
    ## Engagement - default denominator is total aware
    print(simplot(sim,
                  years=c(1980:2020),
                  data.types = c("engagement"),
                  proportion = T,
                  facet.by = c("age","sex"))) 
    print(simplot(sim,
                  years=c(1980:2020),
                  data.types = c("engagement"),
                  proportion = T) + geom_vline(xintercept = c(1986,2014,2016)-1) + ylim(0,100))
    
    ## Suppression - default denominator is total aware
    print(simplot(sim,
                  years=c(1980:2020),
                  data.types = c("suppression"),
                  proportion = T))
    print(simplot(sim,
                  years=c(1980:2020),
                  data.types = c("suppression"),
                  proportion = T,
                  facet.by = c("age","sex"))) 
    
    ## Awareness 
    print(simplot(sim,
                  years=c(1980:2020),
                  data.types = c("awareness"),
                  proportion = T))
    print(simplot(sim,
                  years=c(1980:2020),
                  data.types = c("awareness"),
                  proportion = T,
                  facet.by = c("age","sex")))
    
    ## Incidence
    print(simplot(sim,
                  years=c(1980:2020),
                  data.types = c("incidence"),
                  facet.by = 'age'))
    print(simplot(sim,
                  years=c(1980:2020),
                  data.types = c("incidence"),
                  facet.by = 'age',
                  ages = c('55-59','60-64','65-69',
                           '70-74','75-79','80 and over')))
    print(simplot(sim,
                  years=c(1980:2020),
                  data.types = c("incidence"),
                  facet.by = 'sex'))
    print(simplot(sim,
                  years=c(1980:2020),
                  data.types = c("incidence"))) 
    
    ## Prevalence
    print(simplot(sim,
                  years=c(1980:2020),
                  data.types = c("prevalence"),
                  facet.by = 'age'))
    print(simplot(sim,
                  years=c(1980:2020),
                  data.types = c("prevalence"),
                  facet.by = 'age',
                  ages = c('10-14','15-19'))+ geom_hline(yintercept=130000))
    
    ## Population
    print(simplot(sim,
                  years=c(1980:2020),
                  data.types = c("population"),
                  facet.by = 'age'))
    print(simplot(sim,
                  years=c(1980:2020),
                  data.types = c("population"),
                  facet.by = 'hiv.status'))     #THIS ISN'T RIGHT
    
    ## HIV mortality
    print(simplot(sim,
                  years=c(1980:2020),
                  data.types = c("hiv.mortality"),
                  facet.by = "age",
                  proportion = F)) 
    print(simplot(sim,
                  years=c(1980:2020),
                  data.types = c("hiv.mortality"),
                  facet.by = "age",
                  proportion = T)) 
 
}
   

  
