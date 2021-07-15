
# From jheem package 
extract.incidence <- function(sim,
                              years= NULL, 
                              ages = NULL, 
                              races = NULL, 
                              subpopulations = NULL, 
                              sexes = NULL, 
                              risks = NULL, 
                              non.hiv.subsets = NULL, 
                              continuum = NULL, 
                              cd4s = NULL, 
                              hiv.subsets = NULL, 
                              keep.dimensions = NULL, 
                              include.hiv.positive.in.denominator = T, 
                              per.population = 1e+05, 
                              use.cdc.categorizations = F)
{
        
}

extract.new.diagnoses
extract.prevalence
extract.awareness # package has extract.diagnosed.hiv


# From post-processing file 
extract.suppression <- function(sim,
                                years=sim$years,
                                keep.dimensions='year',
                                per.population=1,
                                ages=NULL,
                                races=NULL,
                                subpopulations=NULL,
                                sexes=NULL,
                                risks=NULL,
                                continuum='diagnosed',
                                cd4=NULL,
                                hiv.subsets=NULL,
                                use.cdc.categorizations=F)
{
        raw.suppression.rates = calculate.suppression(attr(sim, 'components'))
        do.extract.rates(raw.rates = raw.suppression.rates,
                         sim=sim,
                         years=years,
                         keep.dimensions=keep.dimensions,
                         per.population=per.population,
                         ages=ages,
                         races=races,
                         subpopulations=subpopulations,
                         sexes=sexes,
                         risks=risks,
                         continuum=continuum,
                         cd4=cd4,
                         hiv.subsets=hiv.subsets,
                         use.cdc.categorizations=use.cdc.categorizations,
                         include.hiv.negative = F)
}

extract.prep.coverage <- function(sim,
                                  years=sim$years,
                                  keep.dimensions='year',
                                  per.population=1,
                                  ages=NULL,
                                  races=NULL,
                                  subpopulations=NULL,
                                  sexes=NULL,
                                  risks=NULL,
                                  use.cdc.categorizations=F,
                                  multiplier=NULL)
{
        raw.prep.coverage = calculate.prep.coverage(attr(sim, 'components'))
        do.extract.rates(raw.rates = raw.prep.coverage,
                         sim=sim,
                         years=years,
                         keep.dimensions=keep.dimensions,
                         per.population=per.population,
                         ages=ages,
                         races=races,
                         subpopulations=subpopulations,
                         sexes=sexes,
                         risks=risks,
                         use.cdc.categorizations=use.cdc.categorizations,
                         include.hiv.negative = T,
                         multiplier=multiplier)
}

extract.testing.period <- function(sim,
                                   years=sim$years,
                                   keep.dimensions='year',
                                   per.population=1,
                                   ages=NULL,
                                   races=NULL,
                                   subpopulations=NULL,
                                   sexes=NULL,
                                   risks=NULL,
                                   use.cdc.categorizations=F)
{
        1 / extract.testing.rates(sim,
                                  years=years,
                                  keep.dimension=keep.dimensions,
                                  per.population=per.population,
                                  ages=ages,
                                  races=races,
                                  subpopulations=subpopulations,
                                  sexes=sexes,
                                  risks=risks,
                                  use.cdc.categorizations = use.cdc.categorizations)
}

extract.testing.rates <- function(sim,
                                  years=sim$years,
                                  keep.dimensions='year',
                                  per.population=1,
                                  ages=NULL,
                                  races=NULL,
                                  subpopulations=NULL,
                                  sexes=NULL,
                                  risks=NULL,
                                  use.cdc.categorizations=F)
{
        raw.testing.rates = calculate.testing.rates(attr(sim, 'components'))
        raw.testing.rates$rates = lapply(raw.testing.rates$rates, function(r){
                dim.names = dimnames(r)[1:5]
                r = r[,,,,,'undiagnosed',1,1]
                dim(r) = sapply(dim.names, length)
                dimnames(r) = dim.names
                r
        })
        do.extract.rates(raw.rates = raw.testing.rates,
                         sim=sim,
                         years=years,
                         keep.dimensions=keep.dimensions,
                         per.population=per.population,
                         ages=ages,
                         races=races,
                         subpopulations=subpopulations,
                         sexes=sexes,
                         risks=risks,
                         use.cdc.categorizations=use.cdc.categorizations,
                         include.hiv.negative = T)
}
