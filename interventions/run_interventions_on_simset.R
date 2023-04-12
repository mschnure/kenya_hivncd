#####################################################
# Description: Code to run interventions on simset.28 
#####################################################

source("model/run_systematic.R")
source("interventions/extract_intervention_results.R")
print("loading mcmc.28")
load("mcmcruns/mcmc_v28_2023-04-11.Rdata") 

mcmc=mcmc.28
simset = suppressWarnings(extract.simset(mcmc,
                                         additional.burn=100, 
                                         additional.thin=7)) 
# mcmc.28: thin=7 --> 102 sims (burn to 100 instead of 500)
# mcmc.27: thin=8 --> 212 sims
# mcmc.19: thin=3 --> 400 sims
# mcmc.17:  thin=3 --> 486 sims; thin=14 --> 105 sims; thin=100 --> 12 sims

RUN.SIMULATIONS.TO.YEAR = 2040
print("running no.int on mcmc.28")
simset.no.int = run.intervention.on.simset(simset,
                                           end.year = RUN.SIMULATIONS.TO.YEAR,
                                           intervention = NO.INTERVENTION)

print("running all interventions, intermediate level on mcmc.28")
simset.all.intermediate = run.intervention.on.simset(simset,
                                                     end.year = RUN.SIMULATIONS.TO.YEAR,
                                                     intervention = all.intermediate)

print("running engagement/retention only, max level on mcmc.28")
simset.engagement.retention = run.intervention.on.simset(simset,
                                                         end.year = RUN.SIMULATIONS.TO.YEAR,
                                                         intervention = engagement.retention)

print("running all interventions, max level on mcmc.28")
simset.all.max = run.intervention.on.simset(simset,
                                            end.year = RUN.SIMULATIONS.TO.YEAR,
                                            intervention = all.max)

simset.list.full = list(no.int = simset.no.int,
                        all.intermediate = simset.all.intermediate,
                        engagement.retention = simset.engagement.retention,
                        all.max = simset.all.max)

simset.list.base.max = list(no.int = simset.no.int,
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
print("saving simset.list.full")
save(simset.list.full,file = paste0("cached/simset.list.full_",Sys.Date(),".Rdata"))

# individual interventions
if(1==2){
    print("running testing.50 on mcmc.28")
    simset.testing.50 = run.intervention.on.simset(simset,
                                                   end.year = RUN.SIMULATIONS.TO.YEAR,
                                                   intervention = testing.50)
    print("running testing.75 on mcmc.28")
    simset.testing.75 = run.intervention.on.simset(simset,
                                                   end.year = RUN.SIMULATIONS.TO.YEAR,
                                                   intervention = testing.75)
    print("running engagement.80 on mcmc.28")
    simset.engagement.80 = run.intervention.on.simset(simset,
                                                      end.year = RUN.SIMULATIONS.TO.YEAR,
                                                      intervention = engagement.80)
    print("running engagement.90 on mcmc.28")
    simset.engagement.90 = run.intervention.on.simset(simset,
                                                      end.year = RUN.SIMULATIONS.TO.YEAR,
                                                      intervention = engagement.90)
    print("running gain.suppression.80 on mcmc.28")
    simset.gain.suppression.80 = run.intervention.on.simset(simset,
                                                            end.year = RUN.SIMULATIONS.TO.YEAR,
                                                            intervention = gain.suppression.80)
    print("running gain.suppression.90 on mcmc.28")
    simset.gain.suppression.90 = run.intervention.on.simset(simset,
                                                            end.year = RUN.SIMULATIONS.TO.YEAR,
                                                            intervention = gain.suppression.90)
    print("running lose.suppression.10 on mcmc.28")
    simset.gain.suppression.10 = run.intervention.on.simset(simset,
                                                            end.year = RUN.SIMULATIONS.TO.YEAR,
                                                            intervention = gain.suppression.10)
    print("running lose.suppression.05 on mcmc.28")
    simset.gain.suppression.05 = run.intervention.on.simset(simset,
                                                            end.year = RUN.SIMULATIONS.TO.YEAR,
                                                            intervention = gain.suppression.05)
    print("running disengagement.unsuppressed.15 on mcmc.28")
    simset.disengagement.unsuppressed.15 = run.intervention.on.simset(simset,
                                                                      end.year = RUN.SIMULATIONS.TO.YEAR,
                                                                      intervention = disengagement.unsuppressed.15)
    print("running disengagement.unsuppressed.10 on mcmc.28")
    simset.disengagement.unsuppressed.10 = run.intervention.on.simset(simset,
                                                                      end.year = RUN.SIMULATIONS.TO.YEAR,
                                                                      intervention = disengagement.unsuppressed.10)
    print("running disengagement.suppressed.15 on mcmc.28")
    simset.disengagement.suppressed.15 = run.intervention.on.simset(simset,
                                                                    end.year = RUN.SIMULATIONS.TO.YEAR,
                                                                    intervention = disengagement.suppressed.15)
    print("running disengagement.suppressed.10 on mcmc.28")
    simset.disengagement.suppressed.10 = run.intervention.on.simset(simset,
                                                                    end.year = RUN.SIMULATIONS.TO.YEAR,
                                                                    intervention = disengagement.suppressed.10)
}



