#########################################################################
# Description: Generate results/figures from interventions run on mcmc.17 
#########################################################################

# Types of figures
#     1. Calibration plots (no intervention, 2000-2020)
#     2. Intervention plots (2000-2040)
#     3. Age distribution plots (2040)


source("interventions/run_interventions_on_simset.R")

##-----------------------------------------##
##-- CALIBRATION PLOTS (no intervention) --##
##-----------------------------------------##
calibration.plot.years = 2000:2020

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

intervention.plot.years = 2000:2040

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


install.packages("ggsci")
library("scales")
library("ggsci")
pal = ggsci::pal_jama()(3)[-1] # function to pick from a journal's color scheme; then specify how many colors you need

generate.age.distribution(full.results.array, 
                          outcome="engagement", 
                          intervention.1 = "no.int",year.1="2040",
                          intervention.2 = "all.max",year.2="2040") + 
    theme(panel.background = element_blank(), legend.position = "bottom") + # move legend to the bottom
    scale_y_continuous(labels = percent,name = NULL) + # convert y axis labels to percent 
    xlab("Age") + # x axis label 
    scale_fill_manual(labels=c("no.int/2040" = "No intervention, 2040","all.max/2040" = "Combined interventions, 2040"), 
                      name=NULL, values=c("no.int/2040"=pal[1],"all.max/2040"=pal[2])) # change legend and color scheme 

