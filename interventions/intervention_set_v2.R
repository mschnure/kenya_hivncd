#########################################################################
# Description: Code to generate actual intervention units & interventions 
#########################################################################

source("interventions/create_interventions.R")
START.TIME = 2025

##------------------------##
##-- INTERVENTION UNITS --##
##------------------------##
# (just units; assemble these below into actual interventions; no target populations specified yet)
testing.unit.50 = create.intervention.unit(parameter = "TESTING.RATES", 
                                           scale = "proportion",
                                           start.time = START.TIME,
                                           effect.time = 2030,
                                           effect.value = .50,
                                           allow.lower.than.baseline = F)
testing.unit.75 = create.intervention.unit(parameter = "TESTING.RATES", 
                                          scale = "proportion",
                                          start.time = START.TIME,
                                          effect.time = 2030,
                                          effect.value = .75,
                                          allow.lower.than.baseline = F)

engagement.unit.80 = create.intervention.unit(parameter = "ENGAGEMENT.RATES",
                                              scale = "proportion",
                                              start.time = START.TIME,
                                              effect.time = 2030,
                                              effect.value = 0.80,
                                              allow.lower.than.baseline = F)
engagement.unit.90 = create.intervention.unit(parameter = "ENGAGEMENT.RATES",
                                             scale = "proportion",
                                             start.time = START.TIME,
                                             effect.time = 2030,
                                             effect.value = 0.90,
                                             allow.lower.than.baseline = F)

gain.suppression.unit.80 = create.intervention.unit(parameter = "SUPPRESSION.RATES",
                                                    scale = "proportion",
                                                    start.time = START.TIME,
                                                    effect.time = 2030,
                                                    effect.value = .80,
                                                    allow.lower.than.baseline = F)
gain.suppression.unit.90 = create.intervention.unit(parameter = "SUPPRESSION.RATES",
                                                   scale = "proportion",
                                                   start.time = START.TIME,
                                                   effect.time = 2030,
                                                   effect.value = .90,
                                                   allow.lower.than.baseline = F)

lose.suppression.unit.20 = create.intervention.unit(parameter = "UNSUPPRESSION.RATES",
                                                    scale = "proportion",
                                                    start.time = START.TIME,
                                                    effect.time = 2030,
                                                    effect.value = .20,
                                                    allow.higher.than.baseline = F)
lose.suppression.unit.10 = create.intervention.unit(parameter = "UNSUPPRESSION.RATES",
                                                   scale = "proportion",
                                                   start.time = START.TIME,
                                                   effect.time = 2030,
                                                   effect.value = .10,
                                                   allow.higher.than.baseline = F)

disengagement.unsuppressed.unit.20 = create.intervention.unit(parameter = "UNSUPPRESSED.DISENGAGEMENT.RATES",
                                                              scale = "proportion",
                                                              start.time = START.TIME,
                                                              effect.time = 2030,
                                                              effect.value = .20,
                                                              allow.higher.than.baseline = F)
disengagement.unsuppressed.unit.10 = create.intervention.unit(parameter = "UNSUPPRESSED.DISENGAGEMENT.RATES",
                                                             scale = "proportion",
                                                             start.time = START.TIME,
                                                             effect.time = 2030,
                                                             effect.value = .10,
                                                             allow.higher.than.baseline = F)

disengagement.suppressed.unit.20 = create.intervention.unit(parameter = "SUPPRESSED.DISENGAGEMENT.RATES",
                                                            scale = "proportion",
                                                            start.time = START.TIME,
                                                            effect.time = 2030,
                                                            effect.value = .20,
                                                            allow.higher.than.baseline = F)
disengagement.suppressed.unit.10 = create.intervention.unit(parameter = "SUPPRESSED.DISENGAGEMENT.RATES",
                                                           scale = "proportion",
                                                           start.time = START.TIME,
                                                           effect.time = 2030,
                                                           effect.value = .10,
                                                           allow.higher.than.baseline = F)

##------------------------##
##---- INTERVENTIONS -----##
##------------------------## 
# (assembling intervention units)

NO.INTERVENTION = create.intervention.from.units(code="no.int")

testing.50 = create.intervention.from.units(testing.unit.50,
                                           code="testing.50")
testing.75 = create.intervention.from.units(testing.unit.75,
                                           code="testing.75")

engagement.80 = create.intervention.from.units(engagement.unit.80,
                                              code="engagement.80")
engagement.90 = create.intervention.from.units(engagement.unit.90,
                                              code="engagement.90")

gain.suppression.80 = create.intervention.from.units(gain.suppression.unit.80,
                                                    code="gain.suppression.80")
gain.suppression.90 = create.intervention.from.units(gain.suppression.unit.90,
                                                    code="gain.suppression.90")

lose.suppression.20 = create.intervention.from.units(lose.suppression.unit.20,
                                                     code="lose.suppression.20")
lose.suppression.10 = create.intervention.from.units(lose.suppression.unit.10,
                                                     code="lose.suppression.10")

disengagement.unsuppressed.20 = create.intervention.from.units(disengagement.unsuppressed.unit.20,
                                                              code="disengagement.unsuppressed.20")
disengagement.unsuppressed.10 = create.intervention.from.units(disengagement.unsuppressed.unit.10,
                                                              code="disengagement.unsuppressed.10")

disengagement.suppressed.20 = create.intervention.from.units(disengagement.suppressed.unit.20,
                                                            code="disengagement.suppressed.20")
disengagement.suppressed.10 = create.intervention.from.units(disengagement.suppressed.unit.10,
                                                            code="disengagement.suppressed.10")

all.max = create.intervention.from.units(testing.unit.75,
                                         engagement.unit.90,
                                         gain.suppression.unit.90,
                                         lose.suppression.unit.10,
                                         disengagement.unsuppressed.unit.10,
                                         disengagement.suppressed.unit.10,
                                         code="all.max")

all.intermediate = create.intervention.from.units(testing.unit.50,
                                                  engagement.unit.80,
                                                  gain.suppression.unit.80,
                                                  lose.suppression.unit.20,
                                                  disengagement.unsuppressed.unit.20,
                                                  disengagement.suppressed.unit.20,
                                                  code="all.intermediate")

engagement.retention = create.intervention.from.units(engagement.unit.90,
                                                      disengagement.unsuppressed.unit.10,
                                                      disengagement.suppressed.unit.10,
                                                      code="engagement.retention")





##----------------------------##
##-- POSSIBLE INTERVENTIONS --##
##----------------------------##

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

if(1==2){
    
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
}