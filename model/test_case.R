source('source_code.R')
source('model/plots.R')

# Set up the parameters
parameters = create.model.parameters()
parameters = map.model.parameters(parameters)

# Set up the initial state
state.dim.names = list(age=parameters$AGES, 
                       sex=parameters$SEXES,
                       subgroup=parameters$SUBGROUPS,
                       hiv.status=parameters$HIV.STATUS)
# initial.state = array(100000, 
#                       dim = sapply(state.dim.names, length), 
#                       dimnames = state.dim.names)#indexed [age, sex, subgroup, hiv-status]

initial.state = get.initial.population(year = "1970", 
                                       data.manager = DATA.MANAGER, 
                                       model.age.cutoffs = MODEL.AGE.CUTOFFS, 
                                       ages = parameters$AGES, 
                                       sexes = parameters$SEXES, 
                                       seed.to.ages = c(4,5,6), 
                                       seed.to.sexes = c(1,2), 
                                       seed.n = 1)

# Run it
sim = run.model(parameters=parameters,
              initial.state=initial.state,
              start.year=1970,
              end.year=2020,
              keep.years=c(1970:2020))

sim$population[1,,,,]
sim$years

# sims=list(sim,sim1)

# print(simplot.basic(sim,
#                     years=c(2000:2020),
#                     data.types = c("incidence","prevalence"))
# )
# 
# print(simplot.basic(sim,
#                     years=c(1970:2020),
#                     data.types = "population")
# )

print(simplot(sim, 
              years=c(1970:2020),
              data.types = "population", facet.by = 'age'))

# code for testing births/deaths 
if (1==2){
        births.test = sapply(parameters$time.varying.parameters$FERTILITY.RATES$values, function(z){z[1]})
        qplot(1:30,births.test)+ylim(0,NA) # testing out births plot
        qplot(1:30,log(births.test)) # log transformed births 
        logit = function(p){log(p)-log(1-p)} 
        qplot(1:30,logit(births.test)) # logit births 
        qplot(1:30,logit(births.test/.06)) # relative to max birth rate 
        
        deaths.test = sapply(parameters$time.varying.parameters$NON.HIV.MORTALITY.RATES$values, function(z){z[,1,1,1]})
        qplot(1:14,deaths.test[1,])+ylim(0,NA) # testing out deaths plot; 1st age group only (11 age groups total)
        qplot(1:14,log(deaths.test[1,])) # log transformed deaths
}
