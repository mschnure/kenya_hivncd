#testing a commit 
# new commit

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
initial.state = array(47000000, 
                      dim = sapply(state.dim.names, length), 
                      dimnames = state.dim.names)#indexed [age, sex, subgroup, hiv-status]

# Run it
sim = run.model(parameters=parameters,
              initial.state=initial.state,
              start.year=1970,
              end.year=2020,
              keep.years=c(1970:2020))

sim$population[1,,,,]
sim$years

# sims=list(sim,sim1)
# make this plot total population 
print(simplot.basic(sim,
                    years=c(2010:2020),
                    data.types = c("incidence","prevalence"))
)
