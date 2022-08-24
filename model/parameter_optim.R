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


source('model/run_systematic.R')

par = c( #testing.time.0=1986,
        #testing.time.1=2000,
        # testing.rate.0=0.5,
        # testing.rate.1=1,
        #engagement.time.0=2014,
        #engagement.time.1=2016,
        # engagement.rate.0=0.15,
        # engagement.rate.1=0.33,
        # engagement.rate.2=3,
        #suppression.time.0=1986,
        #suppression.time.1=2003,
        # suppression.rate.0=0.8,
        # suppression.rate.1=3,
        trate.0=0.88,
        trate.1=0.2)


run.and.score.sim = function(par,
                             data.manager=DATA.MANAGER,
                             data.type="incidence") {
    
    sim = run.model.for.parameters(variable.parameters=par)
    
    score.sim(sim, data.type = data.type, data.manager=DATA.MANAGER)
    
}

score.sim = function(sim,
                     data.type,
                     data.manager){
    
    sim.output = as.numeric(extract.data(sim,
                                         years=2010:2020, 
                                         data.type = data.type,
                                         age=data.manager[[data.type[1]]]$AGES,
                                         keep.dimensions = c("year","age")))
    
    true = as.numeric(get.surveillance.data(data.manager = DATA.MANAGER, 
                                            years=2010:2020, 
                                            data.type = data.type,
                                            keep.dimensions = c("year","age")))
    
    # sum of squared errors of the population proportion - I think 
    #sum(((sim.pop/rowSums(sim.pop)) - (true.pop/rowSums(true.pop)))^2)
    
    # sum of squared errors of the population itself 
    log(sum((sim.output - true)^2))
}

rv = optim(par = par, fn = run.and.score.sim, method = "BFGS")

sim.test.optim=run.model.for.parameters(variable.parameters = rv$par)
sim.test.manual = run.model.for.parameters(variable.parameters = c(rv$par[1]))

print(simplot(sim.test.optim, 
              years=c(2010:2020),
              data.types = "incidence", facet.by = 'age'))

print(simplot(sim.test.manual, 
              years=c(2010:2020),
              data.types = "incidence", facet.by = 'age'))

# lower score better 
score.sim(sim.test.manual)
score.sim(sim.test.optim)





print(simplot(sim.test.optim, 
              years=c(2010:2020),
              data.types = "population", facet.by = 'age'))

print(simplot(sim.test.manual, 
              years=c(2010:2020),
              data.types = "population", facet.by = 'age'))


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
