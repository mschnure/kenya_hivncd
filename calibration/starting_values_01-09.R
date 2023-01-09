
# load 1/5 starting values
load("calibration/starting_values_01-05.Rdata")

# Add in new transmission & cascade multipliers 
params.start.values["age.15.to.19.transmission.multiplier.3"] = params.start.values["age.15.to.19.transmission.multiplier.2"] = 
    params.start.values["age.15.to.19.transmission.multiplier.1"] = params.start.values["age.15.to.19.transmission.multiplier.0"] = 
    params.start.values["age.15.to.19.transmission.multiplier"]

params.start.values["age.20.to.29.transmission.multiplier.3"] = params.start.values["age.20.to.29.transmission.multiplier.2"] = 
    params.start.values["age.20.to.29.transmission.multiplier.1"] = params.start.values["age.20.to.29.transmission.multiplier.0"] = 
    params.start.values["age.20.to.29.transmission.multiplier"]

params.start.values["age.40.to.49.transmission.multiplier.3"] = params.start.values["age.40.to.49.transmission.multiplier.2"] = 
    params.start.values["age.40.to.49.transmission.multiplier.1"] = params.start.values["age.40.to.49.transmission.multiplier.0"] = 
    params.start.values["age.40.to.49.transmission.multiplier"]

params.start.values["male.suppression.multiplier"] = params.start.values["male.engagement.multiplier"] = 
    params.start.values["male.awareness.multiplier"] = params.start.values["male.cascade.multiplier"]


remove.mask = names(params.start.values) %in% c("age.15.to.19.transmission.multiplier",
                                                "age.20.to.29.transmission.multiplier",
                                                "age.40.to.49.transmission.multiplier",
                                                "male.cascade.multiplier")

params.start.values = params.start.values[!remove.mask]




save(params.start.values,file=("calibration/starting_values_01-09.Rdata"))
