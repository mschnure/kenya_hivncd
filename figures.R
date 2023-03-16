#########################################################################
# Description: Generate results/figures from interventions run on mcmc.17 
#########################################################################
library("scales")
library("ggsci")

# Types of figures
#     1. Calibration plots (no intervention, 2000-2020)
#     2. Intervention plots (2000-2040)
#     3. Age distribution plots (2040)


source("interventions/run_interventions_on_simset.R")
calibration.plot.years = 2000:2020
intervention.plot.years = 2010:2040

##-----------------------------------------##
##-- CALIBRATION PLOTS (no intervention) --##
##-----------------------------------------##

## Incidence 
simplot(simset.no.int, data.types='incidence', years=calibration.plot.years, show.individual.sims = F)
simplot(simset.no.int, data.types='incidence', years=calibration.plot.years, facet.by='age', show.individual.sims = F)
simplot(simset.no.int, data.types='incidence', years=calibration.plot.years, facet.by=c('age',"sex"), ages = "15+", show.individual.sims = F)

## Prevalence 
simplot(simset.no.int, data.types='prevalence', years=calibration.plot.years, show.individual.sims = F)
simplot(simset.no.int, data.types='prevalence', years=calibration.plot.years, facet.by='age', show.individual.sims = F)
simplot(simset.no.int, data.types='prevalence', years=calibration.plot.years, facet.by=c('age',"sex"), ages = "15+", show.individual.sims = F)

## Cascade 
simplot(simset.no.int, data.types=c('awareness',"engagement","suppression"), years=calibration.plot.years, proportion=T, show.individual.sims = F)
simplot(simset.no.int, data.types='awareness', years=calibration.plot.years, facet.by=c('age','sex'), proportion=T, show.individual.sims = F)
simplot(simset.no.int, data.types='engagement', years=calibration.plot.years, facet.by=c('age','sex'), proportion=T, show.individual.sims = F)
simplot(simset.no.int, data.types='suppression', years=calibration.plot.years, facet.by=c('age','sex'), proportion=T, show.individual.sims = F)

## HIV mortality 
simplot(simset.no.int, data.types='hiv.mortality', years=calibration.plot.years, facet.by='age', proportion = T, show.individual.sims = F)

##------------------------##
##-- INTERVENTION PLOTS --##
##------------------------##
# Below plots are all for all.max
# other (intermediate) interventions: 
    # simset.all.intermediate (all interventions at lower level)
    # simset.engagement.retention (engagement 90; disengagment 10)

# individual interventions (did not run individual ones):
    # simset.testing (50 or 75)
    # simset.engagement (80 or 90)
    # simset.gain.suppression (80 or 90)
    # simset.lose.suppression (10 or 5)
    # simset.disengagement.unsuppressed (15 or 10)
    # simset.disengagement.suppressed (15 or 10)

## Incidence 
simplot(simset.no.int, simset.all.max, data.types='incidence', years=intervention.plot.years, show.individual.sims = F)
simplot(simset.no.int, simset.all.max, data.types='incidence', years=intervention.plot.years, facet.by='age', show.individual.sims = F)
simplot(simset.no.int, simset.all.max, data.types='incidence', years=intervention.plot.years, facet.by=c('age',"sex"), ages = "15+", show.individual.sims = F)

## Prevalence 
simplot(simset.no.int, simset.all.max, data.types='prevalence', years=intervention.plot.years, show.individual.sims = F)
simplot(simset.no.int, simset.all.max, data.types='prevalence', years=intervention.plot.years, facet.by='age', show.individual.sims = F)
simplot(simset.no.int, simset.all.max, data.types='prevalence', years=intervention.plot.years, facet.by=c('age',"sex"), ages = "15+", show.individual.sims = F)

## Cascade 
simplot(simset.no.int, simset.all.max, years=intervention.plot.years, data.types=c('awareness',"engagement","suppression"), proportion=T, show.individual.sims = F)
simplot(simset.no.int, simset.all.max, years=intervention.plot.years, facet.by=c('age','sex'), data.types='awareness', proportion=T, show.individual.sims = F)
simplot(simset.no.int, simset.all.max, years=intervention.plot.years, facet.by=c('age','sex'), data.types='engagement', proportion=T, show.individual.sims = F)
simplot(simset.no.int, simset.all.max, years=intervention.plot.years, facet.by=c('age','sex'), data.types='suppression', proportion=T, show.individual.sims = F)


##----------------------------##
##-- AGE DISTRIBUTION PLOTS --##
##----------------------------##

## Incidence
generate.age.distribution(full.results.array, 
                          outcome = "incidence", 
                          intervention.1 = "no.int", year.1 = "2040",
                          intervention.2 = "all.max", year.2 = "2040",
                          display = "table") # default display is figure
generate.age.distribution(results.array = full.results.array,
                          outcome="incidence",
                          intervention.1 = "no.int",year.1="2040",
                          intervention.2 = "all.max",year.2="2040",
                          percent=T)
generate.age.distribution(results.array = full.results.array,
                          outcome="incidence", 
                          intervention.1 = "no.int",year.1="2040",
                          intervention.2 = "all.max",year.2="2040",
                          percent=F) 

## Prevalence
generate.age.distribution(results.array = full.results.array,
                          outcome="prevalence",
                          intervention.1 = "no.int",year.1="2040",
                          intervention.2 = "all.max",year.2="2040",
                          percent=F) 

## Cascade ANNUAL TRANSITIONS: new diagnoses, annual engagement, annual suppression
generate.age.distribution(results.array = full.results.array,
                          outcome="diagnoses", 
                          intervention.1 = "no.int",year.1="2040",
                          intervention.2 = "all.max",year.2="2040",
                          percent=T) 
generate.age.distribution(results.array = full.results.array,
                          outcome="annual.engagement", 
                          intervention.1 = "no.int",year.1="2040",
                          intervention.2 = "all.max",year.2="2040",
                          percent=T) 
generate.age.distribution(results.array = full.results.array,
                          outcome="annual.suppression", 
                          intervention.1 = "no.int",year.1="2040",
                          intervention.2 = "all.max",year.2="2040",
                          percent=T) 

## Cascade STATES: awareness, engagement, suppression 
generate.age.distribution(results.array = full.results.array,
                          outcome="awareness", 
                          intervention.1 = "no.int",year.1="2040",
                          intervention.2 = "all.max",year.2="2040",
                          percent=T) 
generate.age.distribution(results.array = full.results.array,
                          outcome="engagement", 
                          intervention.1 = "no.int",year.1="2040",
                          intervention.2 = "all.max",year.2="2040",
                          percent=T) 
generate.age.distribution(results.array = full.results.array,
                          outcome="suppression", 
                          intervention.1 = "no.int",year.1="2040",
                          intervention.2 = "all.max",year.2="2040",
                          percent=T) 


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
    
    median.export.to.csv
    prop.over.age.export.to.csv
    
    }

{ # Female only 
    median.export.to.csv.female = c(prevalence.engagement.median.age.table.female,incidence.annual.engagement.median.age.table.female)
    dim(median.export.to.csv.female) = sapply(dim.names,length)
    dimnames(median.export.to.csv.female) = dim.names
    
    prop.over.age.export.to.csv.female = c(prevalence.engagement.over.50.table.female,incidence.over.30.table.female,annual.engagement.over.30.table.female)
    dim(prop.over.age.export.to.csv.female) = sapply(dim.names,length)
    dimnames(prop.over.age.export.to.csv.female) = dim.names
    
    median.export.to.csv.female
    prop.over.age.export.to.csv.female
}


{ # Male only 
    median.export.to.csv.male = c(prevalence.engagement.median.age.table.male,incidence.annual.engagement.median.age.table.male)
    dim(median.export.to.csv.male) = sapply(dim.names,length)
    dimnames(median.export.to.csv.male) = dim.names
    
    prop.over.age.export.to.csv.male = c(prevalence.engagement.over.50.table.male,incidence.over.30.table.male,annual.engagement.over.30.table.male)
    dim(prop.over.age.export.to.csv.male) = sapply(dim.names,length)
    dimnames(prop.over.age.export.to.csv.male) = dim.names
    
    median.export.to.csv.male
    prop.over.age.export.to.csv.male
}

calculate.outcome.reduction(full.results.array,target.year="2040",
                            data.type="incidence",
                            intervention="all.max")

calculate.outcome.reduction(full.results.array,target.year="2040",
                            data.type="prevalence",
                            intervention="all.max")


base.2025 = quantile(apply(full.results.array["2025",,,"incidence",,"no.int"],c("sim"),sum), probs = c(.025,.5,.975), na.rm=T)
base.2040 = quantile(apply(full.results.array["2040",,,"incidence",,"no.int"],c("sim"),sum), probs = c(.025,.5,.975), na.rm=T) 
intervention.2040 = quantile(apply(full.results.array["2040",,,"incidence",,"all.max"],c("sim"),sum), probs = c(.025,.5,.975), na.rm=T) 


##--------------------##
##-- PAPER FIGURES  --##
##--------------------##

## Figure 1
# Option 1: 
simplot(simset.no.int, simset.all.max,
        years=intervention.plot.years, 
        show.individual.sims = F,
        for.paper = T) 
# Option 1 (two rows): 
simplot(simset.no.int, simset.all.max, 
        data.types='incidence', 
        ages = c("0-14","15-49","50 and over"),
        facet.by = "age",
        years=intervention.plot.years, 
        show.individual.sims = F,
        for.paper = T)
simplot(simset.no.int, simset.all.max, 
        data.types='prevalence', 
        ages = c("0-14","15-49","50 and over"),
        facet.by = "age",
        years=intervention.plot.years, 
        show.individual.sims = F,
        for.paper = T)


## Figure 2
generate.age.distribution(full.results.array, 
                          outcome="prevalence", 
                          intervention.1 = "no.int",year.1="2025",
                          intervention.2 = "no.int",year.2="2040",
                          intervention.3 = "all.max",year.3="2040",
                          percent=F,
                          sexes = c("female","male")) + 
    scale_fill_discrete(labels=c("no.int/2025" = "2025",
                                 "no.int/2040" = "No intervention, 2040",
                                 "all.max/2040" = "Full intervention, 2040"), 
                        name=NULL) 

generate.age.distribution(full.results.array, 
                          outcome="engagement", 
                          intervention.1 = "no.int",year.1="2025",
                          intervention.2 = "no.int",year.2="2040",
                          intervention.3 = "all.max",year.3="2040",
                          percent=F,
                          sexes = c("female")) + 
    scale_fill_discrete(labels=c("no.int/2025" = "2025",
                                 "no.int/2040" = "No intervention, 2040",
                                 "all.max/2040" = "Full intervention, 2040"), 
                        name=NULL) 

generate.age.distribution(full.results.array, 
                          outcome="engagement", 
                          intervention.1 = "no.int",year.1="2025",
                          intervention.2 = "no.int",year.2="2040",
                          intervention.3 = "all.max",year.3="2040",
                          percent=T,
                          sexes = c("female")) + 
    scale_fill_discrete(labels=c("no.int/2025" = "2025",
                                 "no.int/2040" = "No intervention, 2040",
                                 "all.max/2040" = "Full intervention, 2040"), 
                        name=NULL) 

## Supplemental figures 
generate.age.distribution(full.results.array, 
                          outcome="engagement", 
                          intervention.1 = "no.int",year.1="2025",
                          intervention.2 = "no.int",year.2="2040",
                          intervention.3 = "all.max",year.3="2040",
                          percent=F,
                          sexes = c("female","male")) + 
    scale_fill_discrete(labels=c("no.int/2025" = "2025",
                                 "no.int/2040" = "No intervention, 2040",
                                 "all.max/2040" = "Full intervention, 2040"), 
                        name=NULL) 

generate.age.distribution(full.results.array, 
                          outcome="incidence", 
                          intervention.1 = "no.int",year.1="2025",
                          intervention.2 = "no.int",year.2="2040",
                          intervention.3 = "all.max",year.3="2040",
                          percent=T,
                          sexes = c("female","male")) + 
    scale_fill_discrete(labels=c("no.int/2025" = "2025",
                                 "no.int/2040" = "No intervention, 2040",
                                 "all.max/2040" = "Full intervention, 2040"), 
                        name=NULL) 





# old plot code 
if(1==2){
    
    generate.age.distribution.2.column(full.results.array, 
                                       outcome="prevalence", 
                                       intervention.1 = "no.int",year.1="2025",
                                       intervention.2 = "no.int",year.2="2040",
                                       percent=F) + 
        scale_fill_discrete(labels=c("no.int/2025" = "No intervention, 2025","no.int/2040" = "No intervention, 2040"), 
                            name=NULL) 
    
    generate.age.distribution.2.column(full.results.array, 
                                       outcome="incidence", 
                                       intervention.1 = "no.int",year.1="2025",
                                       intervention.2 = "all.max",year.2="2040",
                                       percent=T) + 
        scale_fill_discrete(labels=c("no.int/2025" = "No intervention, 2025","all.max/2040" = "Combined interventions, 2040"), 
                            name=NULL)
    
    
    pal = ggsci::pal_jama()(3)[-1] # function to pick from a journal's color scheme; then specify how many colors you need
    
    generate.age.distribution(full.results.array, 
                              outcome="engagement", 
                              intervention.1 = "no.int",year.1="2040",
                              intervention.2 = "all.max",year.2="2040") + 
        theme(panel.background = element_blank(), legend.position = "bottom") + # move legend to the bottom
        scale_y_continuous(labels = "percent",name = NULL) + # convert y axis labels to percent 
        # x axis label 
        scale_fill_manual(labels=c("no.int/2040" = "No intervention, 2040","all.max/2040" = "Combined interventions, 2040"), 
                          name=NULL, values=c("no.int/2040"=pal[1],"all.max/2040"=pal[2])) # change legend and color scheme 
}
