
# load 1/9 starting values
load("calibration/starting_values_01-09.Rdata")

# Add in new male hiv mortality multiplier
params.start.values["male.hiv.mortality.multiplier.0"] = 2
params.start.values["male.hiv.mortality.multiplier.1"] = 1
params.start.values["male.hiv.mortality.multiplier.2"] = 1

save(params.start.values,file=("calibration/starting_values_01-26.Rdata"))
