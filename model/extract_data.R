

extract.population <- function(sim,
                               years= sim$years, 
                               ages = sim$AGES, 
                               subgroups = sim$SUBGROUPS, 
                               sexes = sim$SEXES, 
                               hiv.status = sim$HIV.STATUS,
                               keep.dimensions = 'year')
{
        do.extract.4D(sim=sim,
                      arr=sim$population,
                      years= years, 
                      ages = ages, 
                      subgroups = subgroups, 
                      sexes = sexes, 
                      hiv.status = hiv.status,
                      keep.dimensions=keep.dimensions)
                    
          }

extract.incidence <- function(sim,
                              years= sim$years, 
                              ages = sim$AGES, 
                              subgroups = sim$SUBGROUPS, 
                              sexes = sim$SEXES, 
                              keep.dimensions = 'year')
{
        do.extract.3D(sim=sim,
                      arr=sim$incidence,
                      years= years, 
                      ages = ages, 
                      subgroups = subgroups, 
                      sexes = sexes, 
                      keep.dimensions=keep.dimensions)
}

extract.prevalence <- function(sim,
                               years= sim$years, 
                               ages = sim$AGES, 
                               subgroups = sim$SUBGROUPS, 
                               sexes = sim$SEXES, 
                               keep.dimensions = 'year')
{
        extract.population(sim=sim,
                           years=years,
                           ages=ages,
                           subgroups=subgroups,
                           sexes=sexes,
                           keep.dimensions = keep.dimensions,
                           hiv.status = sim$HIV.STATES)
        
}

extract.new.diagnoses <- function(sim,
                                  years= sim$years, 
                                  ages = sim$AGES, 
                                  subgroups = sim$SUBGROUPS, 
                                  sexes = sim$SEXES, 
                                  keep.dimensions = 'year')
{
        do.extract.3D(sim=sim,
                      arr=sim$diagnoses,
                      years= years, 
                      ages = ages, 
                      subgroups = subgroups, 
                      sexes = sexes, 
                      keep.dimensions=keep.dimensions)
}






do.extract.4D <- function(sim,
                          arr,
                          years= sim$years, 
                          ages = sim$AGES, 
                          subgroups = sim$SUBGROUPS, 
                          sexes = sim$SEXES, 
                          hiv.status = sim$HIV.STATUS,
                          keep.dimensions='year')
{
        # subset array; apply statement to collapse array; dimnames; return
        #return an array with dimensions keep.dimensions
        
        x = sim$arr[ages,sexes,subgroups,hiv.status]
        
        full.dim.names = list(year=keep.years,
                         age=parameters$AGES, 
                         sex=parameters$SEXES,
                         subgroup=parameters$SUBGROUPS,
                         hiv.status=parameters$HIV.STATUS)
        
        keep.dim.names = full.dim.names[keep.dimensions]

        rv = array(sapply(x,sum),
                dim=sapply(keep.dim.names, length),
                dimnames=keep.dim.names
        )
        
}



do.extract.3D <- function(sim,
                          arr,
                          years= sim$years, 
                          ages = sim$AGES, 
                          subgroups = sim$SUBGROUPS, 
                          sexes = sim$SEXES, 
                          keep.dimensions='year')
{
        # subset array; apply statement to collapse array; dimnames; return
        #return an array with dimensions keep.dimensions      
}




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
