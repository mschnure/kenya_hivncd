################################################################################################
# Description: Code for running and scoring simulations using different sampled parameter values
################################################################################################

# 1. Can set values either manually or using an optimization function 
# 2. Generic optim function in this code returns optimized parameter values; can then compare simulation 
#     scores using these optimized parameter values or manually chosen parameter values 

# Functions
# 1. run.and.score.sim
#     Runs a simulation given set of sampled parameters; returns a score by calling score.sim function
#     This function is fed to a generic optimization function to be minimized 
# 2. score.sim
#     Calculates the log sum of squared errors between the simulated population and surveillance data for 
#     population (to be minimized via optimization); called by run.and.score.sim 


source('source_code.R')
source('model/plots.R')

par = c(fertility.multiplier=1,
        over.80.mortality.multiplier=1)

run.and.score.sim = function(par,
                             data.manager=DATA.MANAGER) {
    
    sim = run.model.for.parameters(variable.parameters=par)
    
    score.sim(sim)
    
}

score.sim = function(sim){
    sim.pop = extract.data(sim,
                           years=2010:2020, 
                           data.type = "population",
                           keep.dimensions = c("year","age"))
    
    true.pop = get.surveillance.data(data.manager = DATA.MANAGER, 
                                     years=2010:2020, 
                                     data.type = "population",
                                     keep.dimensions = c("year","age"))
    
    # sum of squared errors of the population proportion - I think 
    #sum(((sim.pop/rowSums(sim.pop)) - (true.pop/rowSums(true.pop)))^2)
    
    # sum of squared errors of the population itself 
    log(sum((sim.pop - true.pop)^2))
}

rv = optim(par = par, fn = run.and.score.sim)

sim.test.optim=run.model.for.parameters(variable.parameters = rv$par)
sim.test.manual = run.model.for.parameters(variable.parameters = c(rv$par[1]))

print(simplot(sim.test.optim, 
              years=c(2010:2020),
              data.types = "population", facet.by = 'age'))

print(simplot(sim.test.manual, 
              years=c(2010:2020),
              data.types = "population", facet.by = 'age'))

# lower score better 
score.sim(sim.test.manual)
score.sim(sim.test.optim)






# Online example (https://jootse84.github.io/notes/optim-in-R)
if(1==2)
{
    df <- data.frame(x = c(1, 2, 3, 4, 5), y = c(1, 1, 1, 1, 1), z = c(1, 3, 5, 6, 8))
    sumSqMin <- function(par, data) {
        with(data, sum((par[1] + par[2] * y + par[3] * x - z)^2))
    }
    result <- optim(par = c(0, 1, 1), fn = sumSqMin, data = df)
    plot(z ~ x, data = df, main="Least square error regression for x")
    abline(a = result$par[1], b = result$par[3], col = "red")
}
