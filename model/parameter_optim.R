source('source_code.R')
source('model/plots.R')

par = c(fertility.multiplier=1,
        over.80.mortality.multiplier=1)

sumSqMin = function(par,
                    data.manager=DATA.MANAGER) {
        browser()
        parameters = create.model.parameters()
        
        ## sampled parameters set to NA if I don't pass them 
        parameters = map.model.parameters(parameters,
                                          sampled.parameters=c(fertility.multiplier=par["fertility.multiplier"],
                                                               over.80.mortality.multiplier=par["over.80.mortality.multiplier"],
                                                               hiv.mortality.rates.suppressed=0.01, 
                                                               hiv.mortality.rates.unsuppressed=0.02,
                                                               # non.hiv.mortality.rates=0.01, 
                                                               testing.rates=1.5, 
                                                               engagement.rates=3,
                                                               unsuppressed.disengagement.rates=0.2,
                                                               suppressed.disengagement.rates=0.2,
                                                               suppression.rates=3,
                                                               unsuppression.rates=0.1,
                                                               global.transmission.rate=6, #the average number of infections from one undiagnosed HIV+ person per year 
                                                               relative.transmission.from.diagnosis=0.33))
        
        state.dim.names = list(age=parameters$AGES, 
                               sex=parameters$SEXES,
                               subgroup=parameters$SUBGROUPS,
                               hiv.status=parameters$HIV.STATUS)
        
        initial.state = get.initial.population(year = "1970", 
                                               data.manager = DATA.MANAGER, 
                                               model.age.cutoffs = MODEL.AGE.CUTOFFS, 
                                               ages = parameters$AGES, 
                                               sexes = parameters$SEXES, 
                                               seed.to.ages = c(4,5,6), 
                                               seed.to.sexes = c(1,2), 
                                               seed.n = 1)
        
        # for some reason, running the model here returns NAs for the population 
        sim = run.model(parameters=parameters,
                        initial.state=initial.state,
                        start.year=1970,
                        end.year=2020,
                        keep.years=c(1970:2020))
        
        sim.pop = extract.data(sim,
                               years=2000:2010, 
                               data.type = "population",
                               keep.dimensions = c("year","age"))
        
        true.pop = get.surveillance.data(data.manager = DATA.MANAGER, 
                                         years=2000:2010, 
                                         data.type = "population",
                                         keep.dimensions = c("year","age"))
        
        # sum of squared errors of the population proportion - I think 
        sum(((sim.pop/rowSums(sim.pop)) - (true.pop/rowSums(true.pop)))^2)
        
        # sum of squared errors of the population itself 
        # sum((sim.pop - true.pop)^2)
}

rv = optim(par = par, fn = sumSqMin)



# Online example (https://jootse84.github.io/notes/optim-in-R)
df <- data.frame(x = c(1, 2, 3, 4, 5), y = c(1, 1, 1, 1, 1), z = c(1, 3, 5, 6, 8))
sumSqMin <- function(par, data) {
        with(data, sum((par[1] + par[2] * y + par[3] * x - z)^2))
}
result <- optim(par = c(0, 1, 1), fn = sumSqMin, data = df)
plot(z ~ x, data = df, main="Least square error regression for x")
abline(a = result$par[1], b = result$par[3], col = "red")
