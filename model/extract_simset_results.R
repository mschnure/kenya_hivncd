source("model/run_systematic.R")
load("mcmcruns/mcmc_v15_2023-02-09.Rdata")

RUN.SIMULATIONS.TO.YEAR = 2040

mcmc=mcmc.17
simset = extract.simset(mcmc,
                        additional.burn=500, 
                        additional.thin=30) 

simset.testing.1 = run.intervention.on.simset(simset,
                                              end.year = RUN.SIMULATIONS.TO.YEAR,
                                              intervention = testing.1)

simset.all.int = run.intervention.on.simset(simset,
                                            end.year = RUN.SIMULATIONS.TO.YEAR,
                                            intervention = all.interventions)


years = as.character(simset@simulations[[1]]$years)
ages = simset@simulations[[1]]$AGES
sexes = simset@simulations[[1]]$SEXES
outcomes = c("population","prevalence","incidence","diagnoses","hiv.mortality","non.hiv.mortality",
             "engagement","disengagement.unsuppressed","disengagement.suppressed","suppression")
sims = paste0("sim",c(1:simset@n.sim))
simset.list = list(no.int = simset,
                   testing.1 = simset.testing.1,
                   all.int = simset.all.int)

interventions = names(simset.list)

full.dim.names = list(year = years,
                      age = ages,
                      sex = sexes,
                      outcome = outcomes,
                      sim = sims,
                      intervention = interventions)




full.results.array = sapply(simset.list, function(simset){
    
    sapply(simset@simulations, function(sim){
        sapply(outcomes, function(x){
            
            extract.data(sim, 
                         data.type = x,
                         years = full.dim.names$year,
                         keep.dimensions = c("year","age","sex"))
            
        })
        
    })
})

    
dim(full.results.array) = sapply(full.dim.names, length)
dimnames(full.results.array) = full.dim.names


# checking this array 
simset.all.int@simulations[[13]]$incidence[33,10,2,]==full.results.array[33,10,2,"incidence","sim13","all.int"]
simset.testing.1@simulations[[8]]$disengagement.unsuppressed[5,4,1,]==full.results.array[5,4,1,"disengagement.unsuppressed","sim8","testing.1"]  


simplot(simset,simset.all.int, years=1980:2030, data.types=c('awareness',"engagement","suppression"), proportion=T)
simplot(simset,simset.all.int, years=2000:2030, facet.by='age', data.types='incidence')


# outcomes I want: 
# numbers: prevalence (filter population), incidence, diagnoses, hiv.mortality, non.hiv.mortality
# proportions: hiv.mortality, non.hiv.mortality, engagement, disengagement.unsuppressed, disengagement.suppressed, suppression
# need to manipulate to get for ncd model: population with combined engagement compartments; combined disengagement

# parameters I want for NCD model: 
# prob.diag, prob.eng, prob.diseng (combine two), 

# things that are not by age/sex: 
# hiv.births, non.hiv.births


mean.incidence = apply(full.results.array[,,,"incidence",,],c("year","age","sex","intervention"),mean)

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


