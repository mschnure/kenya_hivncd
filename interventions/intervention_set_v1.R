#########################################################################
# Description: Code to generate actual intervention units & interventions 
#########################################################################

source("interventions/create_interventions.R")

##------------------------##
##-- INTERVENTION UNITS --##
##------------------------##
# (just units; assemble these below into actual interventions; no target populations specified yet)
testing.unit.1 = create.intervention.unit(parameter = "TESTING.RATES", 
                                          scale = "proportion",
                                          start.time = 2025,
                                          effect.time = 2030,
                                          effect.value = .50,
                                          allow.lower.than.baseline = F)

engagement.unit.1 = create.intervention.unit(parameter = "ENGAGEMENT.RATES",
                                             scale = "proportion",
                                             start.time = 2025,
                                             effect.time = 2030,
                                             effect.value = 0.90,
                                             allow.lower.than.baseline = F)

gain.suppression.unit.1 = create.intervention.unit(parameter = "SUPPRESSION.RATES",
                                                   scale = "proportion",
                                                   start.time = 2025,
                                                   effect.time = 2030,
                                                   effect.value = .90,
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

##------------------------##
##---- INTERVENTIONS -----##
##------------------------## 
# (assembling intervention units)

NO.INTERVENTION = create.intervention.from.units(code="no.int")

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