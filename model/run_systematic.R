
# High-level function to take us from parameters to simulation 
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
        
        sim
        
}