

# convert to percent; population-weighted percentage
testing.base.rates = simset.no.int@simulations[[1]]$parameters$time.varying.parameters$TESTING.RATES$values[51] #2025
engagement.base.rates = simset.no.int@simulations[[1]]$parameters$time.varying.parameters$ENGAGEMENT.RATES$values[51] # 2025 value
gain.of.supp.base.rates = simset.no.int@simulations[[1]]$parameters$time.varying.parameters$SUPPRESSION.RATES$values[3] # 2003 value
loss.of.supp.base.rates = simset.no.int@simulations[[1]]$parameters$time.varying.parameters$UNSUPPRESSION.RATES$values # 2000 value

# need to do this for all parameters 
testing.base.rates = sapply(simset.no.int@simulations, function(sim){
    sim$parameters$time.varying.parameters$TESTING.RATES$values[[51]]
})
dim.names = c(dimnames(simset.no.int@simulations[[1]]$parameters$time.varying.parameters$TESTING.RATES$values[[51]]),
              list(sim=1:simset.no.int@n.sim))
dim(testing.base.rates) = sapply(dim.names, length)
dimnames(testing.base.rates) = dim.names
rowMeans(testing.base.rates,dims=3)

# weight for unsupp/supp
unsupp.disengagement.base.rates = simset.no.int@simulations[[1]]$parameters$time.varying.parameters$UNSUPPRESSED.DISENGAGEMENT.RATES$values #2000 value
supp.disengagement.base.rates = simset.no.int@simulations[[1]]$parameters$time.varying.parameters$SUPPRESSED.DISENGAGEMENT.RATES$values # 2000 value


# Rate --> proportion
# rate = -log(1-prop)
prop.from.rate = function(rate){
    (1-exp(-rate))
}

testing.base.props = prop.from.rate(testing.base.rates[[1]])
engagement.base.props = prop.from.rate(engagement.base.rates[[1]])
gain.of.supp.base.props = prop.from.rate(gain.of.supp.base.rates[[1]])

loss.of.supp.base.props = prop.from.rate(loss.of.supp.base.rates[[1]])
maintain.supp.base.props = 1-loss.of.supp.base.props

unsupp.disengagement.base.props = prop.from.rate(unsupp.disengagement.base.rates[[1]])
supp.disengagement.base.props = prop.from.rate(supp.disengagement.base.rates[[1]])

unsupp.pop.2000 = simset.no.int@simulations[[1]]$population["2000",,,,"engaged_unsuppressed"]
supp.pop.2000 = simset.no.int@simulations[[1]]$population["2000",,,,"engaged_suppressed"]
engaged.pop.2000 = unsupp.pop.2000 + supp.pop.2000
unsupp.p = unsupp.pop.2000/engaged.pop.2000
supp.p = 1-unsupp.p

disengagement.base.props = (unsupp.disengagement.base.props*as.vector(unsupp.p)) + 
    (supp.disengagement.base.props*as.vector(supp.p))
retention.base.props = 1-disengagement.base.props

# this should also be in an sapply 
pop.2000 = rowSums(simset.no.int@simulations[[1]]$population["2000",,,,], dims = 2) # loss of supp, disengagement
pop.2003 = rowSums(simset.no.int@simulations[[1]]$population["2003",,,,], dims = 2) # gain of supp
pop.2025 = rowSums(simset.no.int@simulations[[1]]$population["2025",,,,], dims = 2) # testing, engagement

# FINAL VALUES
testing.base.weighted = sum(testing.base.props * (as.vector(pop.2025/sum(pop.2025))))
engagement.base.weighted = sum(engagement.base.props * (as.vector(pop.2025/sum(pop.2025))))
gain.of.supp.base.weighted = sum(gain.of.supp.base.props * (as.vector(pop.2003/sum(pop.2003))))
maintain.supp.base.weighted = sum(maintain.supp.base.props * (as.vector(pop.2000/sum(pop.2000))))
retention.base.weighted = sum(retention.base.props * (as.vector(pop.2000/sum(pop.2000))))

