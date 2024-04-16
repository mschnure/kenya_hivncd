# convert to percent; population-weighted percentage
testing.base.rates = sapply(simset.no.int@simulations, function(sim){ 
    sim$parameters$time.varying.parameters$TESTING.RATES$values[[51]] }) #2025 value

engagement.base.rates = sapply(simset.no.int@simulations, function(sim){
    sim$parameters$time.varying.parameters$ENGAGEMENT.RATES$values[[51]] })  #2025 value

gain.of.supp.base.rates = sapply(simset.no.int@simulations, function(sim){
    sim$parameters$time.varying.parameters$SUPPRESSION.RATES$values[[3]] }) #2003 value

loss.of.supp.base.rates = sapply(simset.no.int@simulations, function(sim){
    sim$parameters$time.varying.parameters$UNSUPPRESSION.RATES$values[[1]] }) #2000 value

# weight for unsupp/supp
unsupp.disengagement.base.rates = sapply(simset.no.int@simulations, function(sim){
    sim$parameters$time.varying.parameters$UNSUPPRESSED.DISENGAGEMENT.RATES$values[[1]] }) #2000 value

supp.disengagement.base.rates = sapply(simset.no.int@simulations, function(sim){
    sim$parameters$time.varying.parameters$SUPPRESSED.DISENGAGEMENT.RATES$values[[1]] }) #2000 value

dim.names = c(dimnames(simset.no.int@simulations[[1]]$parameters$time.varying.parameters$TESTING.RATES$values[[51]]),
              list(sim=1:simset.no.int@n.sim))
dim(testing.base.rates) = dim(engagement.base.rates) = dim(gain.of.supp.base.rates) = 
    dim(loss.of.supp.base.rates) = dim(unsupp.disengagement.base.rates) = dim(supp.disengagement.base.rates) = 
    sapply(dim.names, length)
dimnames(testing.base.rates) = dimnames(engagement.base.rates) = dimnames(gain.of.supp.base.rates) = 
    dimnames(loss.of.supp.base.rates) = dimnames(unsupp.disengagement.base.rates) = dimnames(supp.disengagement.base.rates) = 
    dim.names


# Rate --> proportion
# rate = -log(1-prop)
prop.from.rate = function(rate){
    (1-exp(-rate))
}

testing.base.props = sapply(testing.base.rates, prop.from.rate)
engagement.base.props = sapply(engagement.base.rates, prop.from.rate)
gain.of.supp.base.props = sapply(gain.of.supp.base.rates, prop.from.rate)
loss.of.supp.base.props = sapply(loss.of.supp.base.rates, prop.from.rate)
maintain.supp.base.props = 1-loss.of.supp.base.props

unsupp.disengagement.base.props = sapply(unsupp.disengagement.base.rates, prop.from.rate)
supp.disengagement.base.props = sapply(supp.disengagement.base.rates, prop.from.rate)

dim(testing.base.props) = dim(engagement.base.props) = dim(gain.of.supp.base.props) = 
    dim(loss.of.supp.base.props) = dim(maintain.supp.base.props) = dim(unsupp.disengagement.base.props) = 
    dim(supp.disengagement.base.props) = sapply(dim.names, length)
dimnames(testing.base.props) = dimnames(engagement.base.props) = dimnames(gain.of.supp.base.props) = 
    dimnames(loss.of.supp.base.props) = dimnames(maintain.supp.base.props)= dimnames(unsupp.disengagement.base.props) = 
    dimnames(supp.disengagement.base.props) = dim.names


unsupp.pop.2000 = sapply(simset.no.int@simulations, function(sim){sim$population["2000",,,,"engaged_unsuppressed"] })
supp.pop.2000 = sapply(simset.no.int@simulations, function(sim){sim$population["2000",,,,"engaged_suppressed"] })
dim(unsupp.pop.2000) = dim(supp.pop.2000) = sapply(dim.names, length)
dimnames(unsupp.pop.2000) = dimnames(supp.pop.2000) = dim.names

engaged.pop.2000 = unsupp.pop.2000 + supp.pop.2000
unsupp.p = unsupp.pop.2000/engaged.pop.2000
supp.p = 1-unsupp.p

disengagement.base.props = (unsupp.disengagement.base.props*as.vector(unsupp.p)) + 
    (supp.disengagement.base.props*as.vector(supp.p))
retention.base.props = 1-disengagement.base.props

# this should also be in an sapply 
pop.2000 = sapply(simset.no.int@simulations, function(sim){
    rowSums(sim$population["2000",,,,], dims = 2) }) # loss of supp, disengagement
pop.2003 = sapply(simset.no.int@simulations, function(sim){
    rowSums(sim$population["2003",,,,], dims = 2) }) # gain of supp
pop.2025 = sapply(simset.no.int@simulations, function(sim){
    rowSums(sim$population["2025",,,,], dims = 2) }) # testing, engagement
dim(pop.2000) = dim(pop.2003) = dim(pop.2025) = sapply(dim.names, length)
dimnames(pop.2000) = dimnames(pop.2003) = dimnames(pop.2025) = dim.names

pop.2000.total = colSums(pop.2000, dims = 3)
pop.2003.total = colSums(pop.2003, dims = 3)
pop.2025.total = colSums(pop.2025, dims = 3)

pop.2000.total = array(rep(pop.2000.total, each=34),
                       dim = dim(pop.2000),
                       dimnames = dimnames(pop.2000))
pop.2003.total = array(rep(pop.2003.total, each=34),
                       dim = dim(pop.2003),
                       dimnames = dimnames(pop.2003))
pop.2025.total = array(rep(pop.2025.total, each=34),
                       dim = dim(pop.2025),
                       dimnames = dimnames(pop.2025))

# FINAL VALUES
testing.base.weighted = colSums((testing.base.props * (as.vector(pop.2025/pop.2025.total))),dims=3)
engagement.base.weighted = colSums((engagement.base.props * (as.vector(pop.2025/pop.2025.total))),dims=3)
gain.of.supp.base.weighted = colSums((gain.of.supp.base.props * (as.vector(pop.2003/pop.2003.total))),dims=3)
maintain.supp.base.weighted = colSums((maintain.supp.base.props * (as.vector(pop.2000/pop.2000.total))),dims=3)
retention.base.weighted = colSums((retention.base.props * (as.vector(pop.2000/pop.2000.total))),dims=3)

quantile(testing.base.weighted, c(.025,.5,.975))*100
quantile(engagement.base.weighted, c(.025,.5,.975))*100
quantile(gain.of.supp.base.weighted, c(.025,.5,.975))*100
quantile(maintain.supp.base.weighted, c(.025,.5,.975))*100
quantile(retention.base.weighted, c(.025,.5,.975))*100
