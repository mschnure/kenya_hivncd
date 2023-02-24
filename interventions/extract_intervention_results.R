#########################################################################
# Description: Functions to extract simset results after interventions 
#########################################################################

# Functions
#     1. generate.full.results.array
#     2. generate.age.distribution

# full array of results indexed [year,age,sex,outcome,sim,intervention] - each intervention is a simset
generate.full.results.array = function(simset.list,
                                       years = as.character(simset.list[[1]]@simulations[[1]]$years),
                                       ages = simset.list[[1]]@simulations[[1]]$AGES,
                                       sexes = simset.list[[1]]@simulations[[1]]$SEXES,
                                       outcomes = c(
                                           # STATES: 
                                           "population","prevalence","awareness","engagement","suppression", 
                                           # ANNUAL CONTINUUM TRANSITIONS: 
                                           "incidence","diagnoses","annual.engagement","annual.suppression",
                                           "disengagement.unsuppressed","disengagement.suppressed",
                                           # MORTALITY: 
                                           "hiv.mortality","non.hiv.mortality"
                                       )){
    sims = paste0("sim",c(1:simset@n.sim))
    simset.list = simset.list
    if(is.null(names(simset.list)))
        stop("simset list must be named by intervention")
    interventions = names(simset.list)
    
    full.dim.names = list(year = years,
                          age = ages,
                          sex = sexes,
                          outcome = outcomes,
                          sim = sims,
                          intervention = interventions)
    
    rv = sapply(simset.list, function(simset){
        sapply(simset@simulations, function(sim){
            sapply(outcomes, function(x){
                extract.data(sim, 
                             data.type = x,
                             years = full.dim.names$year,
                             keep.dimensions = c("year","age","sex"))
            })
        })
    })
    
    dim(rv) = sapply(full.dim.names, length)
    dimnames(rv) = full.dim.names
    
    rv
}

# making age structure summary 
generate.age.distribution = function(results.array,
                                     interventions,
                                     outcome,
                                     year="2040",
                                     display="figure"){
    
    results.array = results.array[,,,,,interventions]
    
    # get age counts
    age.counts = apply(results.array[year,,,outcome,,],c("age","sim","intervention"),sum)
    # get totals 
    total.counts = apply(age.counts,c("sim","intervention"),sum)
    
    age.proportions = age.counts/rep(as.numeric(total.counts), each=17)
    # apply(age.proportions,c("sim","intervention"),sum) # checking to make sure it sums to 1 in the right dimension
    
    # marginalizing over sim now
    age.summary = apply(age.proportions,c("age","intervention"),quantile,probs=c(.025,.5,.975))
    
    tab = paste0(round(100*age.summary[2,,]),"% [",
                 round(100*age.summary[1,,]),"-",
                 round(100*age.summary[3,,]),"]")
    tab.dim.names = dimnames(age.counts)[c("age","intervention")]
    dim(tab) = sapply(tab.dim.names,length)
    dimnames(tab) = tab.dim.names
    
    if(display=="table"){
        return(tab)
    } else if(display=="figure"){
        dimnames(age.summary)[1] = list(stat = c("lower","median","upper"))
        df = melt(age.summary)
        
        ggplot(data = df,aes(x=age,y=value,fill=intervention)) + 
            geom_bar(stat="identity",position = "dodge") + 
            ggtitle(paste0(outcome))
    }
    
}



if(1==2){
    # checking this array 
    simset.all.int@simulations[[12]]$incidence[33,10,2,]==full.results.array[33,10,2,"incidence","sim12","all.int"]
    simset.testing.1@simulations[[8]]$disengagement.unsuppressed[5,4,1,]==full.results.array[5,4,1,"disengagement.unsuppressed","sim8","testing.1"]  
    
    # example summary result 
    mean.incidence = apply(full.results.array[,,,"incidence",,],c("year","age","sex","intervention"),mean)    
}



if(1==2){
    # making age structure summary 
    # get age counts
    age.prevalence.counts = apply(full.results.array["2030",,,"prevalence",,],c("age","sim","intervention"),sum)
    # get totals 
    prevalence.counts = apply(age.prevalence.counts,c("sim","intervention"),sum)
    
    age.prevalence.proportions = age.prevalence.counts/rep(as.numeric(prevalence.counts), each=17)
    apply(age.prevalence.proportions,c("sim","intervention"),sum) # checking to make sure it sums to 1 in the right dimension
    
    # marginalizing over sim now
    age.prevalence.summary = apply(age.prevalence.proportions,c("age","intervention"),quantile,probs=c(.025,.5,.975))
    
    tab.1 = paste0(round(100*age.prevalence.summary[2,,]),"% [",
                   round(100*age.prevalence.summary[1,,]),"-",
                   round(100*age.prevalence.summary[3,,]),"]")
    tab.dim.names = dimnames(age.prevalence.counts)[c("age","intervention")]
    dim(tab.1) = sapply(tab.dim.names,length)
    dimnames(tab.1) = tab.dim.names
    
    # get age counts
    age.incidence.counts = apply(full.results.array["2030",,,"incidence",,],c("age","sim","intervention"),sum)
    # get totals 
    incidence.counts = apply(age.incidence.counts,c("sim","intervention"),sum)
    
    age.incidence.proportions = age.incidence.counts/rep(as.numeric(incidence.counts), each=17)
    apply(age.incidence.proportions,c("sim","intervention"),sum) # checking to make sure it sums to 1 in the right dimension
    
    # marginalizing over sim now
    age.incidence.summary = apply(age.incidence.proportions,c("age","intervention"),quantile,probs=c(.025,.5,.975))
    
    tab.2 = paste0(round(100*age.incidence.summary[2,,]),"% [",
                   round(100*age.incidence.summary[1,,]),"-",
                   round(100*age.incidence.summary[3,,]),"]")
    tab.dim.names = dimnames(age.incidence.counts)[c("age","intervention")]
    dim(tab.2) = sapply(tab.dim.names,length)
    dimnames(tab.2) = tab.dim.names
    
    dimnames(age.incidence.summary)[1] = list(stat = c("lower","median","upper"))
    # dim(age.incidence.summary) = sapply(dimnames(age.incidence.summary),length)
    df = melt(age.incidence.summary)
    
    ggplot(data = df,aes(x=age,y=value,fill=intervention)) + geom_bar(stat="identity",position = "dodge")
    
}
