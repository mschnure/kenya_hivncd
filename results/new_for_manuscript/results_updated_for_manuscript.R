
##------------------------##
##-- SUMMARY STATISTICS --##
##------------------------##
{ # Female and male
    outcomes = c("prevalence","engagement","incidence","annual.engagement")
    dim.names = list(intervention=dimnames(full.results.array)$intervention,
                     outcome.year = paste0(rep(outcomes,each=2),rep(c(".2025",".2040"),2)))
    
    median.export.to.csv = c(prevalence.engagement.median.age.table,incidence.annual.engagement.median.age.table)
    dim(median.export.to.csv) = sapply(dim.names,length)
    dimnames(median.export.to.csv) = dim.names
    
    prop.over.age.export.to.csv = c(prevalence.engagement.over.50.table,incidence.over.30.table,annual.engagement.over.30.table)
    dim(prop.over.age.export.to.csv) = sapply(dim.names,length)
    dimnames(prop.over.age.export.to.csv) = dim.names
    
    # median.export.to.csv
    # prop.over.age.export.to.csv
    
    both.sexes.export = cbind(median.export.to.csv,prop.over.age.export.to.csv)
    
}

{ # Female only 
    median.export.to.csv.female = c(prevalence.engagement.median.age.table.female,incidence.annual.engagement.median.age.table.female)
    dim(median.export.to.csv.female) = sapply(dim.names,length)
    dimnames(median.export.to.csv.female) = dim.names
    
    prop.over.age.export.to.csv.female = c(prevalence.engagement.over.50.table.female,incidence.over.30.table.female,annual.engagement.over.30.table.female)
    dim(prop.over.age.export.to.csv.female) = sapply(dim.names,length)
    dimnames(prop.over.age.export.to.csv.female) = dim.names
    
    # median.export.to.csv.female
    # prop.over.age.export.to.csv.female
    
    female.export = cbind(median.export.to.csv.female,prop.over.age.export.to.csv.female)
}


{ # Male only 
    median.export.to.csv.male = c(prevalence.engagement.median.age.table.male,incidence.annual.engagement.median.age.table.male)
    dim(median.export.to.csv.male) = sapply(dim.names,length)
    dimnames(median.export.to.csv.male) = dim.names
    
    prop.over.age.export.to.csv.male = c(prevalence.engagement.over.50.table.male,incidence.over.30.table.male,annual.engagement.over.30.table.male)
    dim(prop.over.age.export.to.csv.male) = sapply(dim.names,length)
    dimnames(prop.over.age.export.to.csv.male) = dim.names
    
    # median.export.to.csv.male
    # prop.over.age.export.to.csv.male
    
    male.export = cbind(median.export.to.csv.male,prop.over.age.export.to.csv.male)
}

full.export = rbind(both.sexes.export,female.export,male.export)
write.csv(full.export, file = paste0("results/full.export_",Sys.Date(),".csv"))

calculate.outcome.reduction(full.results.array,target.year="2040",
                            data.type="incidence",
                            intervention="all.max")
calculate.outcome.reduction(full.results.array,target.year="2040",
                            data.type="incidence",
                            intervention="all.intermediate")
calculate.outcome.reduction(full.results.array,target.year="2040",
                            data.type="incidence",
                            intervention="engagement.retention")

calculate.outcome.reduction(full.results.array,target.year="2040",
                            data.type="prevalence",
                            intervention="all.max")


# base.2025
inc.2025 = round(quantile(apply(full.results.array["2025",,,"incidence",,"no.int"],c("sim"),sum), probs = c(.025,.5,.975), na.rm=T),-3)
prev.2025 = round(quantile(apply(full.results.array["2025",,,"prevalence",,"no.int"],c("sim"),sum), probs = c(.025,.5,.975), na.rm=T),-4)
# base.2040
inc.2040.no.int = round(quantile(apply(full.results.array["2040",,,"incidence",,"no.int"],c("sim"),sum), probs = c(.025,.5,.975), na.rm=T),-3) 
prev.2040.no.int = round(quantile(apply(full.results.array["2040",,,"prevalence",,"no.int"],c("sim"),sum), probs = c(.025,.5,.975), na.rm=T),-4) 
# intervention.2040
inc.2040.full.int = round(quantile(apply(full.results.array["2040",,,"incidence",,"all.max"],c("sim"),sum), probs = c(.025,.5,.975), na.rm=T),-3) 
prev.2040.full.int = round(quantile(apply(full.results.array["2040",,,"prevalence",,"all.max"],c("sim"),sum), probs = c(.025,.5,.975), na.rm=T),-4) 

inc.prev.results = rbind(inc.2025,inc.2040.no.int,inc.2040.full.int,
                         prev.2025,prev.2040.no.int,prev.2040.full.int)


## NUMBERS over 50 (not just %)
absolute.numbers.over.50 = generate.number.over.age.table(simset.list = simset.list.full,
                                                                      age.point=50,
                                                                      data.types = c("prevalence"),
                                                                      years=c(2025,2040))

absolute.numbers.over.50.female = generate.number.over.age.table(simset.list = simset.list.full,
                                                                             age.point=50,
                                                                             data.types = c("prevalence"),
                                                                             years=c(2025,2040),
                                                                             sexes = "female")
absolute.numbers.over.50.male = generate.number.over.age.table(simset.list = simset.list.full,
                                                                           age.point=50,
                                                                           data.types = c("prevalence"),
                                                                           years=c(2025,2040),
                                                                           sexes = "male")

# General population summary stats
{ 
    outcomes = c("population")
    dim.names = list(intervention=dimnames(full.results.array)$intervention,
                     outcome.year = paste0(rep(outcomes,each=2),rep(c(".2025",".2040"),2)))
    
    gen.pop.median.age.table = generate.median.age.table(simset.list = simset.list.full,
                                                         data.types = c("population"),
                                                         years = c(2025,2040))
    
    gen.pop.over.50.table = generate.percent.over.age.table(simset.list = simset.list.full,
                                                            age.point=50,
                                                            data.types = c("population"),
                                                            years=c(2025,2040))
    
}

# NO LONGER USING - CI around delta (per simulation)
{
    quantile((apply(full.results.array["2025",,,"prevalence",,"no.int"],c("sim"),sum) - 
                  apply(full.results.array["2040",,,"prevalence",,"no.int"],c("sim"),sum)), 
             probs = c(.025,.5,.975), na.rm=T)
    qplot(apply(full.results.array["2025",,,"prevalence",,"no.int"],c("sim"),sum) - 
              apply(full.results.array["2040",,,"prevalence",,"no.int"],c("sim"),sum))
    table((apply(full.results.array["2025",,,"prevalence",,"no.int"],c("sim"),sum) - 
               apply(full.results.array["2040",,,"prevalence",,"no.int"],c("sim"),sum))>0)/1000 
    # 74.9% are greater than 0 (i.e., reduction in prevalence)
    
    quantile((apply(full.results.array["2025",,,"incidence",,"no.int"],c("sim"),sum) - 
                  apply(full.results.array["2040",,,"incidence",,"no.int"],c("sim"),sum)), 
             probs = c(.025,.5,.975), na.rm=T)
    qplot(apply(full.results.array["2025",,,"incidence",,"no.int"],c("sim"),sum) - 
              apply(full.results.array["2040",,,"incidence",,"no.int"],c("sim"),sum)) 
    table((apply(full.results.array["2025",,,"incidence",,"no.int"],c("sim"),sum) - 
               apply(full.results.array["2040",,,"incidence",,"no.int"],c("sim"),sum))>0)/1000 
    # 73.7% are greater than 0 (i.e., reduction in incidence)
    
}
