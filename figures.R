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
# Below plots are all for all.int
# other interventions: 
    # simset.testing.1
    # simset.engagement.1
    # simset.gain.suppression.1
    # simset.disengagement.unsuppressed.1
    # simset.disengagement.suppressed.1

intervention.plot.years = 2000:2040

## Incidence 
simplot(simset.no.int, simset.all.int, data.types='incidence', years=intervention.plot.years, show.individual.sims = F)
simplot(simset.no.int, simset.all.int, data.types='incidence', years=intervention.plot.years, facet.by='age', show.individual.sims = F)
simplot(simset.no.int, simset.all.int, data.types='incidence', years=intervention.plot.years, facet.by=c('age',"sex"), ages = "15+", show.individual.sims = F)

## Prevalence 
simplot(simset.no.int, simset.all.int, data.types='prevalence', years=intervention.plot.years, show.individual.sims = F)
simplot(simset.no.int, simset.all.int, data.types='prevalence', years=intervention.plot.years, facet.by='age', show.individual.sims = F)
simplot(simset.no.int, simset.all.int, data.types='prevalence', years=intervention.plot.years, facet.by=c('age',"sex"), ages = "15+", show.individual.sims = F)

## Cascade 
simplot(simset.no.int, simset.all.int, years=intervention.plot.years, data.types=c('awareness',"engagement","suppression"), proportion=T, show.individual.sims = F)
simplot(simset.no.int, simset.all.int, years=intervention.plot.years, facet.by=c('age','sex'), data.types='awareness', proportion=T, show.individual.sims = F)
simplot(simset.no.int, simset.all.int, years=intervention.plot.years, facet.by=c('age','sex'), data.types='engagement', proportion=T, show.individual.sims = F)
simplot(simset.no.int, simset.all.int, years=intervention.plot.years, facet.by=c('age','sex'), data.types='suppression', proportion=T, show.individual.sims = F)


##----------------------------##
##-- AGE DISTRIBUTION PLOTS --##
##----------------------------##

## Incidence
generate.age.distribution(full.results.array, outcome = "incidence", year = "2040", display = "table") # default display is figure
generate.age.distribution(full.results.array, outcome = "incidence", interventions = c("no.int","all.int"), year = "2040")

## Prevalence
generate.age.distribution(full.results.array, outcome = "prevalence", interventions = c("no.int","all.int"), year = "2040")

## Cascade ANNUAL TRANSITIONS: new diagnoses, annual engagement, annual suppression
generate.age.distribution(full.results.array, outcome = "diagnoses", interventions = c("no.int","all.int"), year = "2040")
generate.age.distribution(full.results.array, outcome = "annual.engagement", interventions = c("no.int","all.int"), year = "2040")
generate.age.distribution(full.results.array, outcome = "annual.suppression", interventions = c("no.int","all.int"), year = "2040")

## Cascade STATES: awareness, engagement, suppression 
generate.age.distribution(full.results.array, outcome = "awareness", interventions = c("no.int","all.int"), year = "2040")
generate.age.distribution(full.results.array, outcome = "engagement", interventions = c("no.int","all.int"), year = "2040")
generate.age.distribution(full.results.array, outcome = "suppression", interventions = c("no.int","all.int"), year = "2040")



