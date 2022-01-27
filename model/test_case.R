
source('model/diffeq.R')
source('model/parameters.R')

# Set up the parameters

parameters = create.model.parameters()
parameters = map.model.parameters(parameters)

# Set up the initial state
state.dim.names = list(age=parameters$AGES, 
                       sex=parameters$SEXES,
                       subgroup=parameters$SUBGROUPS,
                       hiv.status=parameters$HIV.STATUS)
initial.state = array(10, 
                      dim = sapply(state.dim.names, length), 
                      dimnames = state.dim.names)#indexed [age, sex, subgroup, hiv-status]

# Run it
compute.dx(time=2010,
           y=set.up.initial.diffeq.vector(initial.state, parameters),
           parameters)