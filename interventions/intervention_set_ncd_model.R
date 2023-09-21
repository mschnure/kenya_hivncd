#########################################################################
# Description: Code to generate actual intervention units & interventions 
#########################################################################

source("interventions/create_interventions.R")
START.TIME = 2023

##------------------------##
##-- INTERVENTION UNITS --##
##------------------------##
# (just units; assemble these below into actual interventions; no target populations specified yet)
testing.unit.75 = create.intervention.unit(parameter = "TESTING.RATES", 
                                           scale = "proportion",
                                           start.time = START.TIME,
                                           effect.time = 2030,
                                           effect.value = .75,
                                           allow.lower.than.baseline = F)

engagement.unit.90 = create.intervention.unit(parameter = "ENGAGEMENT.RATES",
                                              scale = "proportion",
                                              start.time = START.TIME,
                                              effect.time = 2030,
                                              effect.value = 0.90,
                                              allow.lower.than.baseline = F)

gain.suppression.unit.90 = create.intervention.unit(parameter = "SUPPRESSION.RATES",
                                                    scale = "proportion",
                                                    start.time = START.TIME,
                                                    effect.time = 2030,
                                                    effect.value = .90,
                                                    allow.lower.than.baseline = F)

lose.suppression.unit.10 = create.intervention.unit(parameter = "UNSUPPRESSION.RATES",
                                                    scale = "proportion",
                                                    start.time = START.TIME,
                                                    effect.time = 2030,
                                                    effect.value = .10,
                                                    allow.higher.than.baseline = F)

disengagement.unsuppressed.unit.10 = create.intervention.unit(parameter = "UNSUPPRESSED.DISENGAGEMENT.RATES",
                                                              scale = "proportion",
                                                              start.time = START.TIME,
                                                              effect.time = 2030,
                                                              effect.value = .10,
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

testing.75 = create.intervention.from.units(testing.unit.75,
                                            code="testing.75")

engagement.90 = create.intervention.from.units(engagement.unit.90,
                                               code="engagement.90")

gain.suppression.90 = create.intervention.from.units(gain.suppression.unit.90,
                                                     code="gain.suppression.90")

lose.suppression.10 = create.intervention.from.units(lose.suppression.unit.10,
                                                     code="lose.suppression.10")

disengagement.unsuppressed.10 = create.intervention.from.units(disengagement.unsuppressed.unit.10,
                                                               code="disengagement.unsuppressed.10")

disengagement.suppressed.10 = create.intervention.from.units(disengagement.suppressed.unit.10,
                                                             code="disengagement.suppressed.10")

NO.INTERVENTION = create.intervention.from.units(code="no.int")

retention.suppression = create.intervention.from.units(gain.suppression.unit.90,
                                                       lose.suppression.10,
                                                       disengagement.unsuppressed.unit.10,
                                                       disengagement.suppressed.unit.10,
                                                       code="retention.suppression")

testing.engagement = create.intervention.from.units(testing.unit.75,
                                                    engagement.unit.90,
                                                    code="testing.engagement")

all.max = create.intervention.from.units(testing.unit.75,
                                         engagement.unit.90,
                                         gain.suppression.unit.90,
                                         lose.suppression.unit.10,
                                         disengagement.unsuppressed.unit.10,
                                         disengagement.suppressed.unit.10,
                                         code="all.max")
