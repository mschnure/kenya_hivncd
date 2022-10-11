################################################################################################
# Description: Code that runs from initializing parameters all the way through simulation
################################################################################################

# Functions 
#     1. run.model.for.parameters 
#     2. extract.hiv.data.for.ncd 

source('source_code.R')

## Added this in so that parameters objects exists without running a sim - need this for likelihood functions 
parameters = create.model.parameters()
parameters = map.model.parameters(parameters)

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
# Transmission parameters
variable.parameters['trate.0']=.7
variable.parameters['trate.1']=0.2
variable.parameters['trate.2']=0.15
variable.parameters['female.to.male.multiplier']=1.03
variable.parameters['age.15.to.19.transmission.multiplier']=.67
variable.parameters['age.20.to.29.transmission.multiplier']=1.23
variable.parameters['age.40.to.49.transmission.multiplier']=1.1
variable.parameters['age.50.and.over.transmission.multiplier.0']=0.55
variable.parameters['age.50.and.over.transmission.multiplier.1']=0.35
variable.parameters['age.50.and.over.transmission.multiplier.2']=0.28
variable.parameters['age.assortativity']=.8
variable.parameters['birth.transmission.risk']=.6
# Cascade parameters
variable.parameters['testing.rate.1']=0.5
variable.parameters['engagement.rate.2']=1.5
variable.parameters['suppression.rate.0']=0.7
variable.parameters['suppression.rate.1']=4
variable.parameters['unsuppression.rates']=.05
variable.parameters['male.cascade.multiplier']=.6
# Mortality parameters
variable.parameters['age.45.to.65.mortality.intercept.multiplier']= 2.3 # multiplies intercept or slope before projecting
variable.parameters['age.45.to.65.mortality.slope.multiplier']= 1.01 
variable.parameters['over.65.mortality.intercept.multiplier']= 1.0 
variable.parameters['over.65.mortality.slope.multiplier']= 1.01 

variable.parameters['hiv.specific.mortality.rates.1']=0.025
variable.parameters['hiv.specific.mortality.rates.2']=0.1
variable.parameters['hiv.specific.mortality.rates.3']=0.03


variable.parameters['age.15.to.24.hiv.mortality.multiplier']= 0.4
variable.parameters['over.50.hiv.mortality.multiplier']= 3
variable.parameters['age.0.to.14.hiv.mortality.multiplier.1']= 12


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
    print(simplot(sim,
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
                  proportion = T)) 
    print(simplot(sim,
                  years=c(1980:2020),
                  data.types = c("hiv.mortality"),
                  facet.by = "age",
                  proportion = F)) 
    
    ## Awareness - default denominator is PLHIV 
    print(simplot(sim,
                  years=c(1980:2020),
                  data.types = c("awareness"),
                  proportion = T,
                  facet.by = c("age","sex")))
    
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
                  proportion = T,
                  facet.by = c("age","sex")))
    
    print(simplot(sim,
                  years=c(1980:2020),
                  data.types = c("engagement","suppression"),
                  proportion = T,
                  facet.by = c("age","sex")))
}
   
## Save outputs for NCD model 
if(1==2){
    hiv.output.for.ncd = extract.hiv.data.for.ncd(sim=sim,years = 2015)
    hiv.pop.2015 = hiv.output.for.ncd$population[1,,,]
    
    ## Combining engaged categories into one 
    dim.names = list(age = dimnames(hiv.pop.2015)[[1]],
                    sex = dimnames(hiv.pop.2015)[[2]],
                    hiv.status = c("hiv_negative","undiagnosed","diagnosed_unengaged","engaged"))
    
    hiv.pop.2015.new = array(c(hiv.pop.2015[,,c("hiv_negative","undiagnosed","diagnosed_unengaged")],
                               hiv.pop.2015[,,"engaged_unsuppressed"] + hiv.pop.2015[,,"engaged_suppressed"]),
                             dim = sapply(dim.names, length),
                             dimnames = dim.names)

    write.csv(c(hiv.pop.2015.new),file="hivpop.csv")
    
    # Population distribution 
    pop.2015 = hiv.output.for.ncd$population[1,,,]
    pop.2015 = apply(pop.2015,c(1:2),sum) # combine over hiv states to only return age/sex distribution 
    
    pop.2015.distribution = round(100*apply(pop.2015,2,function(x){x/sum(x)}),2)
    pop.2015.distribution = rbind(pop.2015.distribution,colSums(pop.2015.distribution))
    rownames(pop.2015.distribution)[nrow(pop.2015.distribution)] = "Total"
    
    write.csv(pop.2015.distribution,file="pop.2015.distribution.csv")
    
}
  
