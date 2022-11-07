source("model/run_systematic.R")

sim = run.model.for.parameters(variable.parameters = variable.parameters)

## Extracts outputs needed for NCD model
extract.hiv.data.for.ncd = function(sim,
                                    years=sim$years){
    
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
                     hiv.status = c("hiv_negative","undiagnosed","diagnosed_unengaged","engaged"))
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
    
    rv
}

hiv.output.for.ncd = extract.hiv.data.for.ncd(sim=sim)

# convert all "80 and over" to "80-85"
hiv.output.for.ncd$population[,"80 and over",,] =hiv.output.for.ncd$population[,"80 and over",,]/4
dimnames(hiv.output.for.ncd$population)[[2]][17] = "80-85"
hiv.output.for.ncd$population = aperm(hiv.output.for.ncd$population, c(1,4,2,3))

hiv.output.for.ncd$incidence[,"80 and over",] =hiv.output.for.ncd$incidence[,"80 and over",]/4
dimnames(hiv.output.for.ncd$incidence)[[2]][17] = "80-85"

hiv.output.for.ncd$hiv.mortality[,"80 and over",] =hiv.output.for.ncd$hiv.mortality[,"80 and over",]/4
dimnames(hiv.output.for.ncd$hiv.mortality)[[2]][17] = "80-85"

hiv.output.for.ncd$diagnosis[,"80 and over",] =hiv.output.for.ncd$diagnosis[,"80 and over",]/4
dimnames(hiv.output.for.ncd$diagnosis)[[2]][17] = "80-85"

hiv.output.for.ncd$engagement[,"80 and over",] =hiv.output.for.ncd$engagement[,"80 and over",]/4
dimnames(hiv.output.for.ncd$engagement)[[2]][17] = "80-85"

hiv.output.for.ncd$disengagement[,"80 and over",] =hiv.output.for.ncd$disengagement[,"80 and over",]/4
dimnames(hiv.output.for.ncd$disengagement)[[2]][17] = "80-85"

hiv.output.for.ncd$suppression[,"80 and over",] =hiv.output.for.ncd$suppression[,"80 and over",]/4
dimnames(hiv.output.for.ncd$suppression)[[2]][17] = "80-85"

hiv.sim = sim

save(hiv.sim,DATA.MANAGER,hiv.output.for.ncd,file="~/Dropbox/Documents_local/Hopkins/PhD/Dissertation/ABM/rHivNcd/hiv_sim.RData")

# testing plotting hiv distribution 
if(1==2) {
    
    if(hiv.positive.population.only) {
        pop = hiv.output.for.ncd$population[,-1,,]  
    } else 
        pop = hiv.output.for.ncd$population
    
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
        
        hiv.distr[i,,,] = hiv.probs
        
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
    pop.2015 = hiv.output.for.ncd$population[1,,,]
    pop.2015 = apply(pop.2015,c(1:2),sum) # combine over hiv states to only return age/sex distribution 
    
    pop.2015.distribution = round(100*apply(pop.2015,2,function(x){x/sum(x)}),2) # get proportions (divide by total)
    pop.2015.distribution = rbind(pop.2015.distribution,colSums(pop.2015.distribution))
    rownames(pop.2015.distribution)[nrow(pop.2015.distribution)] = "Total"
    
    write.csv(pop.2015.distribution,file="pop.2015.distribution.csv")
}


    