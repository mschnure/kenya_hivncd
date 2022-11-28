source("model/run_systematic.R")

variable.parameters.2 = variable.parameters

# testing different likelihoods (make these changes one at a time)
# incidence, prevalence, hiv mortality (transmission-related likelihoods)
variable.parameters.2['trate.0'] = 0.65 # original value is 0.7

# awareness
# variable.parameters.2['testing.rate.1'] = 0.2 # original value is 0.5

# engagement
# variable.parameters.2['engagement.rate.2'] = 4 # original value is 1.5

# suppression
variable.parameters.2['suppression.rate.1'] = 6 # original value is 4

sim1 = run.model.for.parameters(variable.parameters = variable.parameters)
sim2 = run.model.for.parameters(variable.parameters = variable.parameters.2)

print(simplot(sim1, sim2,
              years=c(1980:2020),
              data.types = c("awareness","engagement","suppression"), 
              proportion = T,
              facet.by = c("age","sex")))

print(simplot(sim1, sim2,
              years=c(1980:2020),
              data.types = "engagement",
              proportion = T,
              facet.by = c("age","sex")))

print(simplot(sim1, sim2,
              years=c(1980:2020),
              data.types = c("incidence","prevalence"),
              facet.by = c("age")))

lik = create.likelihood(parameters=sim1$parameters) # run once
lik.components = attr(lik,"components") # run once

# making this a likelihood ratio; exponentiate after subtracting on log scale
# e.g., value of 0.01 means that sim2 is 100x worse than sim 1
sapply(lik.components,function(sub.lik){exp(sub.lik(sim2) - sub.lik(sim1))}) 
round(sapply(lik.components,function(sub.lik){exp(sub.lik(sim2) - sub.lik(sim1))}),2) 

round(sapply(lik.components,function(sub.lik){(exp(sub.lik(sim2)))}),2)
round(sapply(lik.components,function(sub.lik){(exp(sub.lik(sim1)))}),2)

# print(lik.components$engagement(sim2, debug=T))

# code for debugging likelihood (within browser)
if(1==2){
    extract.data(sim2,data.type = "hiv.mortality",year = "2010",
                 keep.dimensions = c("year","age","sex"))
    rowSums(M)
    M[1,]
    length(years)
    dim(y.star)
    M[2,]
    y.star = extract.data(sim = sim,
                          data.type = numerator.data.type,
                          years=years,
                          keep.dimensions = c("year","sex","age"))
    z = melt(y.star)
    z
    z[M[1,]==1,]
    
}

# # auto regressive correlation structure example 
# rho =0.9
# diff = matrix(abs(rep(time,length(time))-rep(time,each=length(time))),nrow=10)
# rho^diff