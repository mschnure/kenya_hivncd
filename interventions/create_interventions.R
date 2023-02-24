####################################################################################################
# Description: Functions to run interventions (functions only; no actual interventions in this code)
####################################################################################################

# Functions
#     1. run.intervention.on.simset
#     2. create.intervention.unit
#     3. create.intervention.from.units
#     4. convert.scales


# applies over the sims in a simset, running the model with parameters changed based on intervention values 
run.intervention.on.simset = function(simset,
                                      end.year,
                                      interventions){
    
    simset@simulations = lapply(1:simset@n.sim,function(i){
        
        run.model.for.parameters(variable.parameters = simset@parameters[i,], # rows are values of sampled parameters for each simulation in simset
                                 end.year = end.year,
                                 interventions = interventions) 
        
    })
    
    attr(simset,"intervention") = interventions
    
    simset
    
}

# creates a single intervention unit - does not specify which ages/sexes to apply to 
create.intervention.unit = function(parameter,
                                    scale, # proportion, rate, time
                                    start.time, # time when intervention starts scaling up
                                    effect.times, # times when intervention reaches value
                                    effect.values, # values that intervention reaches 
                                    end.time=Inf, # when intervention should end
                                    allow.lower.than.baseline = T,
                                    allow.higher.than.baseline = T
                                    ){ 
    # check that all arguments are in the right format (e.g., scale is length 1 character, etc.)
    
    rv = list(parameter=parameter,
              scale=scale,
              start.time = start.time,
              effect.times = effect.times,
              effect.values = effect.values,
              end.time = end.time,
              allow.lower.than.baseline = allow.lower.than.baseline,
              allow.higher.than.baseline = allow.higher.than.baseline)
    class(rv) = "intervention.unit"
    rv
    
}

# assembles intervention units into an intervention; specifies which ages/sexes to apply to 
create.intervention.from.units = function(..., # pass one or more intervention objects; want to pass a list of intervention objects to parameters
                                          target.ages=NULL, # will write into parameters code that if it's null, apply to all ages/sexes
                                          target.sexes=NULL,
                                          code){ # for naming interventions
    units = list(...)
    
    # name the intervention objects after what parameter they affect
    names(units) = sapply(units,function(intervention.unit){
        intervention.unit$parameter
    })
    
    rv = list(units=units,
              target.ages=target.ages,
              target.sexes=target.sexes,
              code=code)
    class(rv) = "intervention"
    rv
}

# allows the user to write the intervention in whatever scale they want (e.g., a proportion); 
# parameter will later be converted to the correct scale for the underlying parameter (e.g., a rate)
convert.scales = function(values,
                          from.scale,
                          to.scale){
    
    if(from.scale=="rate"){
        
        if(to.scale=="rate"){

            values
            
        } else if(to.scale=="proportion"){
            
            1-exp(-values) # converting rate to proportion, assuming time is 1
            
        } else if(to.scale=="time"){
            
            1/values # converting rate to time
            
        } else
            stop(paste0("unrecognized scale: ",to.scale))
        
    } else if(from.scale=="proportion"){
        
        if(to.scale=="rate"){
            
            -log(1-values) # proportion to rate
            
        } else if(to.scale=="proportion"){
            
            values
            
        } else if(to.scale=="time"){
            
            1/(-log(1-values))
            
        } else
            stop(paste0("unrecognized scale: ",to.scale))
        
    } else if(from.scale=="time"){
        
        if(to.scale=="rate"){
            
            1/values
            
        } else if(to.scale=="proportion"){
            
            1-exp(-(1/values))
            
        } else if(to.scale=="time"){
            
            values
            
        } else
            stop(paste0("unrecognized scale: ",to.scale))
        
    } else
        stop(paste0("unrecognized scale: ",from.scale))
    
    
}


