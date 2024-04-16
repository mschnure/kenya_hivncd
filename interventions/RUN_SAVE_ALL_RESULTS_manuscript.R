########################################################
# Description: Code to run interventions on simset.final 
########################################################

source("model/run_systematic.R")
source("interventions/extract_intervention_results.R")
source("calibration/resampling.R")
source("for_ncd_model/extract_ncd_output.R")

RUN.SIMULATIONS.TO.YEAR = 2040
print("running interventions for Aim 2 paper")
print("running no.int on simset.final")
simset.no.int = run.intervention.on.simset(simset.final,
                                           end.year = RUN.SIMULATIONS.TO.YEAR,
                                           intervention = NO.INTERVENTION)

# print("running all interventions, intermediate level on simset.final")
# simset.all.intermediate = run.intervention.on.simset(simset.final,
#                                                      end.year = RUN.SIMULATIONS.TO.YEAR,
#                                                      intervention = all.intermediate)
# 
# print("running engagement/retention only, max level on simset.final")
# simset.engagement.retention = run.intervention.on.simset(simset.final,
#                                                          end.year = RUN.SIMULATIONS.TO.YEAR,
#                                                          intervention = engagement.retention)

print("running all interventions, max level on simset.final")
simset.all.max = run.intervention.on.simset(simset.final,
                                            end.year = RUN.SIMULATIONS.TO.YEAR,
                                            intervention = all.max)

# print("running interventions for NCD model")
# print("running retention/suppression on simset.final")
# simset.retention.suppression = run.intervention.on.simset(simset.final,
#                                                           end.year = RUN.SIMULATIONS.TO.YEAR,
#                                                           intervention = retention.suppression)
# 
# print("running testing/engagement only, max level on simset.final")
# simset.testing.engagement = run.intervention.on.simset(simset.final,
#                                                        end.year = RUN.SIMULATIONS.TO.YEAR,
#                                                        intervention = testing.engagement)


simset.list.full = list(no.int = simset.no.int,
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


# SAVE RESULTS
print("saving all results for Aim 2 paper")
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


# print("saving simsets for NCD model")
# khm.no.int.1 = extract.simset.output(simset.no.int,
#                                      intervention.id="1")
# 
# khm.no.int.2 = extract.simset.output(simset.no.int,
#                                      intervention.id="2")
# 
# khm.retention.suppression = extract.simset.output(simset.retention.suppression,
#                                                   intervention.id="3")
# 
# khm.testing.engagement = extract.simset.output(simset.testing.engagement,
#                                                intervention.id="4")
# 
# khm.all = extract.simset.output(simset.all.max,
#                                 intervention.id="5")

# need to rename everything to khm.full for NCD side to work - save these in sequence 

# # Baseline
# khm.full = khm.no.int.1
# save(khm.full,
#      file=paste0("~/Dropbox/Documents_local/Hopkins/PhD/Dissertation/ABM/rHivNcd/data/hiv_simset_noint.RData"))
# 
# # Ret/Supp
# khm.full = khm.retention.suppression
# save(khm.full,
#      file=paste0("~/Dropbox/Documents_local/Hopkins/PhD/Dissertation/ABM/rHivNcd/data/hiv_simset_retsupp.RData"))
# 
# # Tst/Eng
# khm.full = khm.testing.engagement
# save(khm.full,
#      file=paste0("~/Dropbox/Documents_local/Hopkins/PhD/Dissertation/ABM/rHivNcd/data/hiv_simset_tsteng.RData"))
# 
# # Comprehensive
# khm.full = khm.all
# save(khm.full,
#      file=paste0("~/Dropbox/Documents_local/Hopkins/PhD/Dissertation/ABM/rHivNcd/data/hiv_simset_comp.RData"))
