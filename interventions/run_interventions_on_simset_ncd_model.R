#####################################################
# Description: Code to run interventions on simset.29 
#####################################################

source("model/run_systematic.R")
source("interventions/extract_intervention_results.R")
print("loading mcmc.29")
load("~/Dropbox/Documents_local/Hopkins/PhD/Dissertation/ABM/kenya_hivncd/mcmcruns/mcmc_v29_2023-04-21.Rdata")

mcmc=mcmc.29
simset = suppressWarnings(extract.simset(mcmc,
                                         additional.burn=500, 
                                         additional.thin=20)) 

RUN.SIMULATIONS.TO.YEAR = 2040
print("running no.int on mcmc.29")
simset.no.int = run.intervention.on.simset(simset,
                                           end.year = RUN.SIMULATIONS.TO.YEAR,
                                           intervention = NO.INTERVENTION)

print("running retention/suppression on mcmc.29")
simset.retention.suppression = run.intervention.on.simset(simset,
                                                          end.year = RUN.SIMULATIONS.TO.YEAR,
                                                          intervention = retention.suppression)

print("running testing/engagement only, max level on mcmc.29")
simset.testing.engagement = run.intervention.on.simset(simset,
                                                       end.year = RUN.SIMULATIONS.TO.YEAR,
                                                       intervention = testing.engagement)

print("running all interventions, max level on mcmc.29")
simset.all.max = run.intervention.on.simset(simset,
                                            end.year = RUN.SIMULATIONS.TO.YEAR,
                                            intervention = all.max)

simset.list.full = list(no.int = simset.no.int,
                        retention.suppression = simset.retention.suppression,
                        testing.engagement = simset.testing.engagement,
                        all.max = simset.all.max)

print("generating full.results.array")
full.results.array = generate.full.results.array(simset.list = simset.list.full)

## BOTH SEXES ## 
print("generating summary statistics, both sexes")
prevalence.engagement.median.age.table = generate.median.age.table(simset.list = simset.list.full,
                                                                   data.types = c("prevalence","engagement"),
                                                                   years = c(2025,2040))
incidence.annual.engagement.median.age.table = generate.median.age.table(simset.list = simset.list.full,
                                                                         data.types = c("incidence","annual.engagement"),
                                                                         years = c(2025,2040))
prevalence.engagement.over.50.table = generate.percent.over.age.table(simset.list = simset.list.full,
                                                                      age.point=50,
                                                                      data.types = c("prevalence","engagement"),
                                                                      years=c(2025,2040))
incidence.over.30.table = generate.percent.over.age.table(simset.list = simset.list.full,
                                                          age.point=30,
                                                          data.types = c("incidence"),
                                                          years=c(2025,2040))
annual.engagement.over.30.table = generate.percent.over.age.table(simset.list = simset.list.full,
                                                                  age.point=30,
                                                                  data.types = c("annual.engagement"),
                                                                  years=c(2025,2040))

## FEMALE ONLY ## 
print("generating summary statistics, female")
prevalence.engagement.median.age.table.female = generate.median.age.table(simset.list = simset.list.full,
                                                                          data.types = c("prevalence","engagement"),
                                                                          years = c(2025,2040),
                                                                          sexes = "female")
incidence.annual.engagement.median.age.table.female = generate.median.age.table(simset.list = simset.list.full,
                                                                                data.types = c("incidence","annual.engagement"),
                                                                                years = c(2025,2040),
                                                                                sexes = "female")
prevalence.engagement.over.50.table.female = generate.percent.over.age.table(simset.list = simset.list.full,
                                                                             age.point=50,
                                                                             data.types = c("prevalence","engagement"),
                                                                             years=c(2025,2040),
                                                                             sexes = "female")
incidence.over.30.table.female = generate.percent.over.age.table(simset.list = simset.list.full,
                                                                 age.point=30,
                                                                 data.types = c("incidence"),
                                                                 years=c(2025,2040),
                                                                 sexes = "female")
annual.engagement.over.30.table.female = generate.percent.over.age.table(simset.list = simset.list.full,
                                                                         age.point=30,
                                                                         data.types = c("annual.engagement"),
                                                                         years=c(2025,2040),
                                                                         sexes = "female")

## MALE ONLY ## 
print("generating summary statistics, male")
prevalence.engagement.median.age.table.male = generate.median.age.table(simset.list = simset.list.full,
                                                                        data.types = c("prevalence","engagement"),
                                                                        years = c(2025,2040),
                                                                        sexes = "male")
incidence.annual.engagement.median.age.table.male = generate.median.age.table(simset.list = simset.list.full,
                                                                              data.types = c("incidence","annual.engagement"),
                                                                              years = c(2025,2040),
                                                                              sexes = "male")
prevalence.engagement.over.50.table.male = generate.percent.over.age.table(simset.list = simset.list.full,
                                                                           age.point=50,
                                                                           data.types = c("prevalence","engagement"),
                                                                           years=c(2025,2040),
                                                                           sexes = "male")
incidence.over.30.table.male = generate.percent.over.age.table(simset.list = simset.list.full,
                                                               age.point=30,
                                                               data.types = c("incidence"),
                                                               years=c(2025,2040),
                                                               sexes = "male")
annual.engagement.over.30.table.male = generate.percent.over.age.table(simset.list = simset.list.full,
                                                                       age.point=30,
                                                                       data.types = c("annual.engagement"),
                                                                       years=c(2025,2040),
                                                                       sexes = "male")
# print("saving simset.list.full")
# save(simset.list.full,file = paste0("cached/simset.list.full_",Sys.Date(),".Rdata"))

print("saving all results")
save(simset.list.full,
     full.results.array,
     prevalence.engagement.median.age.table,
     incidence.annual.engagement.median.age.table,
     prevalence.engagement.over.50.table,
     incidence.over.30.table,
     annual.engagement.over.30.table,
     
     prevalence.engagement.median.age.table.female,
     incidence.annual.engagement.median.age.table.female,
     prevalence.engagement.over.50.table.female,
     incidence.over.30.table.female,
     annual.engagement.over.30.table.female,
     
     prevalence.engagement.median.age.table.male,
     incidence.annual.engagement.median.age.table.male,
     prevalence.engagement.over.50.table.male,
     incidence.over.30.table.male,
     annual.engagement.over.30.table.male,
     
     file = paste0("cached/all.results_",Sys.Date(),".Rdata"))