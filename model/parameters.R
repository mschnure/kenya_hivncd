

# age.cutoffs - the lower limit for each bracket
make.model.parameters <- function(age.cutoffs=c(10,25,55),
                                  sexes = c('female','male'),
                                  subpopulations = 'all',
                                  risks = 'all')
{
    parameters = list()
    
    # sex, risk, subpop
    parameters$SEXES = sexes
    parameters$RISKS = risks
    parameters$SUBPOPULATIONS = subpopulations
    
    # hiv status
    parameters$HIV.STATUS = c('uninfected','undiagnosed','diagnosed.unsuppressed','diagnosed.suppressed')
    
    # ages
    age.names = c(paste0(age.cutoffs[-length(age.cutoffs)], " to ", age.cutoffs[-1]),
                  paste0(age.cutoffs[length(age.cutoffs)], "+"))
    parameters$AGES = age.names
    
    # return
    parameters
}


add.time.varying.parameter.value <- function()
{
    
}

compute.time.varying.parameters <- function(parameters, time)
{
    
}