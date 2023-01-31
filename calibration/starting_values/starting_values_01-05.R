
# load 12/19 starting values
load("calibration/starting_values_12-19.Rdata")

# Add in new aging rates/factors
params.start.values["age.15.to.19.base.aging.rate"] = 0.25
params.start.values["age.20.to.24.base.aging.rate"] = 0.25

params.start.values["age.15.to.19.aging.factor"] = 2
params.start.values["age.20.to.24.aging.factor"] = 2
params.start.values["age.25.to.50.aging.factor"] = 2
params.start.values["over.50.aging.factor"] = 2

save(params.start.values,file=("calibration/starting_values_01-05.Rdata"))

