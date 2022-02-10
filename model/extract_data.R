
extract.data = function(sim,
                        data.type,
                        years= sim$years, 
                        ages = sim$AGES, 
                        subgroups = sim$SUBGROUPS, 
                        sexes = sim$SEXES, 
                        hiv.status = sim$HIV.STATUS,
                        keep.dimensions = 'year')
{
        
}

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
        
        if(!is.character(ages))
                ages = sim$AGES[ages]
        
        if(!is.character(subgroups))
                subgroups = sim$SUBGROUPS[subgroups]
        
        if(!is.character(sexes))
                sexes = sim$SEXES[sexes]
        
        if(!is.character(hiv.status))
                hiv.status = sim$HIV.STATUS[hiv.status]
        
        x = arr[as.character(years),ages,sexes,subgroups,hiv.status]
        
        full.dim.names = list(year=years,
                         age=ages, 
                         sex=sexes,
                         subgroup=subgroups,
                         hiv.status=hiv.status)
        
        dim(x)=sapply(full.dim.names,length)
        dimnames(x)=full.dim.names
        
        keep.dim.names = full.dim.names[keep.dimensions]

        rv = apply(x,keep.dimensions,sum)
        dim(rv)=sapply(keep.dim.names, length)
        dimnames(rv)=keep.dim.names
        
        rv
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



