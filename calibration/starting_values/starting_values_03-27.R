# load 2/9 MCMC run 
load("calibration/starting_values/starting_values_02-09.Rdata")

params.start.values["proportion.trate.change.by.3.5"] = 0.75

save(params.start.values,file=("calibration/starting_values/starting_values_03-27.Rdata"))
