source("model/run_systematic.R")

##-----------------------------##
##-- OUTPUT FROM MCMC SIMSET --##
##-----------------------------##
## Choose MCMC run 
load("mcmcruns/mcmc2023-01-05 22:10:23.Rdata")
mcmc=mcmc.7

# 10 sims for now 
simset.for.ncd = extract.simset(mcmc,
                                additional.burn=1000, # throw away first 1000
                                additional.thin=100) # thin by 10, 100 remaining

extract.simset.output = function(simset,
                                 years=c(2014:2030),
                                 id.prefix="khm.sim",
                                 intervention.id) {
    
    simset = add.sim.ids(simset,
                         id.prefix = id.prefix)
    simset = add.model.type(simset)
    simset = add.sim.intervention.id(simset,
                                     intervention.id = intervention.id)
    
    rv = list()
    for(i in 1:simset@n.sim){
        sim = simset@simulations[[i]]
        
        rv[[i]] = extract.hiv.data.for.ncd(sim)
        rv[[i]]$id = sim$id
        rv[[i]]$model = sim$model
        rv[[i]]$intervention.id = sim$intervention.id
        rv[[i]]$target.parameters = return.all.parameters(sim,years=years)
    }
    
    class(rv) = "khm_simulation_output"
    
    rv
}

# helper functions
add.sim.ids = function(simset,
                       id.prefix=""){
    
    simset@simulations = lapply(1:simset@n.sim,function(i){
        
        sim = simset@simulations[[i]]
        sim$id = paste0(id.prefix,i)
        
        sim
    })
    
    simset
}

add.model.type = function(simset){
    
    simset@simulations = lapply(1:simset@n.sim,function(i){
        
        sim = simset@simulations[[i]]
        sim$model = "hiv"
        
        sim
    })
    
    simset
}

add.sim.intervention.id = function(simset,
                                   intervention.id){
    
    simset@simulations = lapply(1:simset@n.sim,function(i){
        
        sim = simset@simulations[[i]]
        sim$intervention.id = intervention.id
        
        sim
    })
    
    simset
    
}

##----------------------------##
##-- OUTPUT FROM SINGLE SIM --##
##----------------------------##
sim = run.model.for.parameters(variable.parameters = params.start.values)

## Extracts outputs needed for NCD model
extract.hiv.data.for.ncd = function(sim,
                                    years=c(2014:2030)){
    
    # These will be combined into one "disengagement" output (Parastu's request)
    disengagement.suppressed = extract.data(sim, 
                                            data.type = "disengagement.suppressed",
                                            years = years,
                                            keep.dimensions = c("year","age","sex"))
    disengagement.unsuppressed = extract.data(sim, 
                                              data.type = "disengagement.unsuppressed",
                                              years = years,
                                              keep.dimensions = c("year","age","sex"))
    
    rv = list()
    
    # For population, combine two engagement compartments after extracting
    rv$population = extract.data(sim, 
                                 data.type = "population",
                                 years = years,
                                 keep.dimensions = c("year","age","sex","hiv.status"))
    dim.names = list(year = dimnames(rv$population)[[1]],
                     age = dimnames(rv$population)[[2]],
                     sex = dimnames(rv$population)[[3]],
                     hiv.status = c("HIV.NEG","HIV.UNDIAG","HIV.UNENG","HIV.ENG"))
    rv$population = array(c(rv$population[,,,c("hiv_negative","undiagnosed","diagnosed_unengaged")],
                            rv$population[,,,"engaged_unsuppressed"] + rv$population[,,,"engaged_suppressed"]),
                             dim = sapply(dim.names, length),
                             dimnames = dim.names)
    
    
    rv$incidence = extract.data(sim, 
                                data.type = "incidence",
                                years = years,
                                keep.dimensions = c("year","age","sex"))
    rv$hiv.mortality = extract.data(sim, 
                                    data.type = "hiv.mortality",
                                    years = years,
                                    keep.dimensions = c("year","age","sex"))
    rv$non.hiv.mortality = extract.data(sim, 
                                    data.type = "non.hiv.mortality",
                                    years = years,
                                    keep.dimensions = c("year","age","sex"))
    rv$diagnosis = extract.data(sim, 
                                data.type = "diagnoses",
                                years = years,
                                keep.dimensions = c("year","age","sex"))
    rv$engagement = extract.data(sim, 
                                 data.type = "annual.engagement",
                                 years = years,
                                 keep.dimensions = c("year","age","sex"))
    rv$disengagement = disengagement.suppressed + disengagement.unsuppressed
    rv$suppression = extract.data(sim,
                                  data.type = "annual.suppression",
                                  years = years,
                                  keep.dimensions = c("year","age","sex"))
    
    
    ### convert all "80 and over" to "80-85", rename female/male to be all caps ####
    rv$population[,"80 and over",,] =rv$population[,"80 and over",,]/4
    dimnames(rv$population)[[2]][17] = "80-85"
    dimnames(rv$population)[[3]] = c("FEMALE","MALE")
    rv$population = aperm(rv$population, c(1,4,2,3))
    
    rv$incidence[,"80 and over",] =rv$incidence[,"80 and over",]/4
    dimnames(rv$incidence)[[2]][17] = "80-85"
    dimnames(rv$incidence)[[3]] = c("FEMALE","MALE")
    
    rv$hiv.mortality[,"80 and over",] =rv$hiv.mortality[,"80 and over",]/4
    dimnames(rv$hiv.mortality)[[2]][17] = "80-85"
    dimnames(rv$hiv.mortality)[[3]] = c("FEMALE","MALE")
    
    rv$non.hiv.mortality[,"80 and over",] =rv$non.hiv.mortality[,"80 and over",]/4
    dimnames(rv$non.hiv.mortality)[[2]][17] = "80-85"
    dimnames(rv$non.hiv.mortality)[[3]] = c("FEMALE","MALE")
    
    rv$diagnosis[,"80 and over",] =rv$diagnosis[,"80 and over",]/4
    dimnames(rv$diagnosis)[[2]][17] = "80-85"
    dimnames(rv$diagnosis)[[3]] = c("FEMALE","MALE")
    
    rv$engagement[,"80 and over",] =rv$engagement[,"80 and over",]/4
    dimnames(rv$engagement)[[2]][17] = "80-85"
    dimnames(rv$engagement)[[3]] = c("FEMALE","MALE")
    
    rv$disengagement[,"80 and over",] =rv$disengagement[,"80 and over",]/4
    dimnames(rv$disengagement)[[2]][17] = "80-85"
    dimnames(rv$disengagement)[[3]] = c("FEMALE","MALE")
    
    rv$suppression[,"80 and over",] =rv$suppression[,"80 and over",]/4
    dimnames(rv$suppression)[[2]][17] = "80-85"
    dimnames(rv$suppression)[[3]] = c("FEMALE","MALE")
    
    rv
}

## Extract target parameter values for NCD model ## 
extract.parameter.by.year = function(sim,
                                     specific.parameter,
                                     year){
    
    parameters=sim$parameters
    all.params = compute.time.varying.parameters(parameters=parameters,
                                                 time=year)
    
    rv = all.params[[specific.parameter]]
    
    rv
}

calculate.proportion = function(rate,time=1){
    
    p = 1-exp(-(rate*time))
    
    p
}

extract.parameter.all.years = function(sim,
                                       specific.parameter,
                                       years){
    parameters = sim$parameters
    
    dim.names = list(year = years,
                     age = c(parameters$AGES[-length(parameters$AGES)],"80-85"),
                     sex = c("FEMALE","MALE"))
    
    rv = array(NA,
               dim = sapply(dim.names,length),
               dimnames = dim.names)
    
    for(year in years){
        rv[as.character(year),,] = extract.parameter.by.year(sim=sim,
                                                             specific.parameter = specific.parameter,
                                                             year=year)
    }
    
    # convert from rate to proportion
    rv = calculate.proportion(rate=rv,time=1)
    
    rv
}

extract.fertility.and.maternal.transmission = function(sim,
                                                       specific.parameter,
                                                       years){
    
    parameters = sim$parameters
    
    dim.names = list(year = years,
                     age = c(parameters$AGES),
                     sex = c("FEMALE","MALE"),
                     hiv.status=c("hiv_negative","undiagnosed","diagnosed_unengaged","engaged_unsuppressed","engaged_suppressed"))
    
    rv = array(NA,
               dim = sapply(dim.names,length),
               dimnames = dim.names)
    
    for(year in years){
        rv[as.character(year),,,] = extract.parameter.by.year(sim=sim,
                                                              specific.parameter = specific.parameter,
                                                              year=year)
    }
    
    rv
    
}

return.births.by.hiv.status = function(sim,
                                       years,
                                       hiv.status){
    parameters = sim$parameters
    
    fertility = extract.fertility.and.maternal.transmission(sim=sim,
                                                            years=years,
                                                            specific.parameter = "FERTILITY.RATES")
    
    pop = extract.data(sim, 
                       data.type = "population",
                       years = years,
                       keep.dimensions = c("year","age","sex","hiv.status"))
    
    births = fertility*pop
    
    maternal.fetal.transmission = extract.fertility.and.maternal.transmission(sim=sim,
                                                                              years=years,
                                                                              specific.parameter = "MATERNAL.FETAL.TRANSMISSION")
    if(hiv.status=="hiv"){
        births.by.status = births*maternal.fetal.transmission
        
    } else if(hiv.status=="non.hiv"){
        births.by.status = births*(1-maternal.fetal.transmission)
    }
    
    births.by.status = apply(births.by.status, 1, sum)
}


return.all.parameters = function(sim,
                                 years){
    
    parameters = sim$parameters
    
    rv = list()
    
    rv$prob.diag = extract.parameter.all.years(sim=sim,
                                               specific.parameter = "TESTING.RATES",
                                               years=years)
    
    rv$prob.eng = extract.parameter.all.years(sim=sim,
                                              specific.parameter = "ENGAGEMENT.RATES",
                                              years=years)
    
    prob.diseng.supp = extract.parameter.all.years(sim=sim,
                                                   specific.parameter = "SUPPRESSED.DISENGAGEMENT.RATES",
                                                   years=years)
    prob.diseng.unsupp = extract.parameter.all.years(sim=sim,
                                                     specific.parameter = "UNSUPPRESSED.DISENGAGEMENT.RATES",
                                                     years=years)
    
    disengagement.suppressed = extract.data(sim=sim, 
                                            data.type = "disengagement.suppressed",
                                            years = years,
                                            keep.dimensions = c("year","age","sex"))
    disengagement.unsuppressed = extract.data(sim=sim, 
                                              data.type = "disengagement.unsuppressed",
                                              years = years,
                                              keep.dimensions = c("year","age","sex"))
    
    rv$prob.diseng = ((prob.diseng.supp*disengagement.suppressed) + (prob.diseng.unsupp*disengagement.unsuppressed))/
        (disengagement.suppressed + disengagement.unsuppressed)
    
    rv$hiv.births = return.births.by.hiv.status(sim=sim,
                                                years=years,
                                                hiv.status="hiv")
    
    rv$non.hiv.births = return.births.by.hiv.status(sim=sim,
                                                    years=years,
                                                    hiv.status="non.hiv")
    
    rv
}



# SAVE NEW OUTPUT
khm = extract.simset.output(simset.for.ncd,
                            intervention.id="no.int")


# save(simset.for.ncd,khm,file="~/Dropbox/Documents_local/Hopkins/PhD/Dissertation/ABM/rHivNcd/data/hiv_simset.RData")
save(khm,file="~/Dropbox/Documents_local/Hopkins/PhD/Dissertation/ABM/rHivNcd/data/hiv_simset.RData")



## OLD OUTPUT FROM SINGLE SIM
# renamed from hiv.output.for.ncd to khm (for "kenya hiv model")
# khm = extract.hiv.data.for.ncd(sim=sim)
# hiv.sim = sim
# target.parameters = return.all.parameters(sim=hiv.sim,
#                                           years=hiv.sim$years)

# testing plotting hiv distribution 
if(1==2) {
    
    if(hiv.positive.population.only) {
        pop = khm$population[,-1,,]  
    } else 
        pop = khm$population
    
    full.dim.names = dimnames(pop)
    hiv.dim.names =  dimnames(pop)[-1]
    
    hiv.distr = array(0,
                      dim = sapply(full.dim.names,length),
                      dimnames = full.dim.names)
    
    years = full.dim.names[[1]]
    for(i in 1:length(years)){
        
        hiv.probs = 
            sapply(1:length(hiv.dim.names$age), function(age){
                sapply(1:length(hiv.dim.names$sex), function(sex){
                    rowSums(pop[i,,,],1)/sum(pop[i,,,])
                })
            })
        
        dim(hiv.probs) = sapply(hiv.dim.names,length)
        dimnames(hiv.probs) = hiv.dim.names
        
        # test
        # colSums(hiv.probs,1)
        
        hiv.distr[i,,,] = round(hiv.probs,5)
        
        # test
        # apply(hiv.distr,c(1,3,4),sum)
    }
    
    df.for.plot = melt(hiv.distr)
    
    # total
    ggplot(df.for.plot, aes(fill=hiv.status, x = year, y = value)) + 
        geom_bar(position="fill", stat="identity") 

    # age
    ggplot(df.for.plot, aes(fill=hiv.status, x = year, y = value)) + 
        geom_bar(position="fill", stat="identity") + 
        facet_wrap(~age, scales = "free_y")
    
    # sex
    ggplot(df.for.plot, aes(fill=hiv.status, x = year, y = value)) + 
        geom_bar(position="fill", stat="identity") + 
        facet_wrap(~sex, scales = "free_y")
    
}




# Not sure if I still need this?
if(1==2){
    # Population distribution 
    pop.2015 = khm$population[1,,,]
    pop.2015 = apply(pop.2015,c(1:2),sum) # combine over hiv states to only return age/sex distribution 
    
    pop.2015.distribution = round(100*apply(pop.2015,2,function(x){x/sum(x)}),2) # get proportions (divide by total)
    pop.2015.distribution = rbind(pop.2015.distribution,colSums(pop.2015.distribution))
    rownames(pop.2015.distribution)[nrow(pop.2015.distribution)] = "Total"
    
    write.csv(pop.2015.distribution,file="pop.2015.distribution.csv")
}


    