
# TESTING 
# undiagnosed --> diagnosed_unengaged
# parameter = TESTING.RATES

# ENGAGEMENT 
# diagnosed_unengaged --> engaged_unsuppressed
# parameter = ENGAGEMENT.RATES

# RETENTION 
# (1) retention.unsuppressed
# engaged_unsuppressed --> diagnosed_unengaged
# parameter = UNSUPPRESSED.DISENGAGEMENT.RATES 
# (2) retention.suppressed
# engaged_suppressed --> diagnosed_unengaged
# parameter = SUPPRESSED.DISENGAGEMENT.RATES

# SUPPRESSION 
# (1) gain.suppression
# engaged_unsuppressed --> engaged_suppressed
# parameter = SUPPRESSION.RATES
# (2) lose.suppression
# engaged_suppressed --> engaged_unsuppressed
# parameter = UNSUPPRESSION.RATES

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


# testing these out 
testing.unit.1 = create.intervention.unit(parameter = "TESTING.RATES", 
                                          scale = "proportion",
                                          start.time = 2025,
                                          effect.time = 2030,
                                          effect.value = .90,
                                          allow.lower.than.baseline = F)

engagement.unit.1 = create.intervention.unit(parameter = "ENGAGEMENT.RATES",
                                          scale = "proportion",
                                          start.time = 2025,
                                          effect.time = 2030,
                                          effect.value = 0.95,
                                          allow.lower.than.baseline = F)

gain.suppression.unit.1 = create.intervention.unit(parameter = "SUPPRESSION.RATES",
                                              scale = "proportion",
                                              start.time = 2025,
                                              effect.time = 2030,
                                              effect.value = .95,
                                              allow.lower.than.baseline = F)

lose.suppression.unit.1 = create.intervention.unit(parameter = "UNSUPPRESSION.RATES",
                                              scale = "proportion",
                                              start.time = 2025,
                                              effect.time = 2030,
                                              effect.value = .05,
                                              allow.higher.than.baseline = F)

disengagement.unsuppressed.unit.1 = create.intervention.unit(parameter = "UNSUPPRESSED.DISENGAGEMENT.RATES",
                                                             scale = "proportion",
                                                             start.time = 2025,
                                                             effect.time = 2030,
                                                             effect.value = 1-.90,
                                                             allow.higher.than.baseline = F)

disengagement.suppressed.unit.1 = create.intervention.unit(parameter = "SUPPRESSED.DISENGAGEMENT.RATES",
                                                           scale = "proportion",
                                                           start.time = 2025,
                                                           effect.time = 2030,
                                                           effect.value = 1-.90,
                                                           allow.higher.than.baseline = F)

gain.suppression.unit.fake.1 = create.intervention.unit(parameter = "SUPPRESSION.RATES",
                                                   scale = "proportion",
                                                   start.time = 2025,
                                                   effect.time = 2025.1,
                                                   effect.value = .1,
                                                   allow.lower.than.baseline = T)


testing.1 = create.intervention.from.units(testing.unit.1,
                                           code="testing.1")

engagement.1 = create.intervention.from.units(engagement.unit.1,
                                              code="engagement.1")

gain.suppression.1 = create.intervention.from.units(gain.suppression.unit.1,
                                                    code="gain.suppression.1")
disengagement.unsuppressed.1 = create.intervention.from.units(disengagement.unsuppressed.unit.1,
                                                    code="disengagement.unsuppressed.1")

disengagement.suppressed.1 = create.intervention.from.units(disengagement.suppressed.unit.1,
                                                              code="disengagement.suppressed.1")

all.interventions = create.intervention.from.units(testing.unit.1,
                                                   engagement.unit.1,
                                                   gain.suppression.unit.1,
                                                   lose.suppression.unit.1,
                                                   disengagement.unsuppressed.unit.1,
                                                   disengagement.suppressed.unit.1,
                                                   code="all")
all.interventions.female = create.intervention.from.units(testing.unit.1,
                                                   engagement.unit.1,
                                                   gain.suppression.unit.1,
                                                   lose.suppression.unit.1,
                                                   disengagement.unsuppressed.unit.1,
                                                   disengagement.suppressed.unit.1,
                                                   target.sexes = "female",
                                                   code="all")

all.interventions.male = create.intervention.from.units(testing.unit.1,
                                                          engagement.unit.1,
                                                          gain.suppression.unit.1,
                                                          lose.suppression.unit.1,
                                                          disengagement.unsuppressed.unit.1,
                                                          disengagement.suppressed.unit.1,
                                                          target.sexes = "male",
                                                          code="all")

suppression.female.bad = create.intervention.from.units(gain.suppression.unit.fake.1,
                                                        code="female.suppressed.bad",
                                                        target.sexes = "female")


NO.INTERVENTION = create.intervention.from.units(code="no.int")

# 
# convert.scales(2,from.scale = "rate",to.scale = "time")
# convert.scales(2,from.scale = "rate",to.scale = "proportion")
# convert.scales(3,from.scale = "rate",to.scale = "proportion")
# check the rest of these




