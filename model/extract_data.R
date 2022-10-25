#################################################################################################
# Description: Functions to extract different information from the ODE result; called in plotting 
# functions but can be used on their own 
#################################################################################################

# Functions 
#     1. extract.data
#     2. do.extract.4D
#     3. do.extract.3D
#     4. extract.population
#     5. extract.incidence
#     6. extract.prevalence
#     7. extract.total.aware
#     8. extract.new.diagnoses
#     9. extract.hiv.mortality
#     10. extract.total.suppressed
#     11. extract.annual.suppression
#     12. extract.total.engaged
#     13. extract.annual.engagement
#     14. extract.disengagement.suppressed
#     15. extract.disengagement.unsuppressed



# General function to extract data based on data type; calls lower-level functions (e.g., extract 
# incidence) based on which data type is given 
extract.data = function(sim,
                        data.type,
                        data.manager = DATA.MANAGER,
                        years = sim$years, # years to include in the report (other years are excluded)
                        ages = sim$AGES, 
                        sexes = sim$SEXES,
                        subgroups = sim$SUBGROUPS, 
                        hiv.status = sim$HIV.STATUS,
                        keep.dimensions = 'year', # collapse all other dimensions & report the data as total value over this dimension
                        scale.population = F # change once I'm pulling population data
){
    if(all(keep.dimensions!='year'))
        stop("must keep year dimension")
    
    if (data.type=='incidence')
        rv = extract.incidence(sim, 
                               years=years, 
                               ages=ages,
                               sexes=sexes,
                               subgroups=subgroups,
                               keep.dimensions=keep.dimensions)
    else if (data.type=='awareness')
        rv = extract.total.aware(sim,
                                 years=years, 
                                 ages=ages,
                                 sexes=sexes,
                                 subgroups=subgroups,
                                 keep.dimensions=keep.dimensions)
    else if (data.type=='diagnoses')
        rv = extract.new.diagnoses(sim,
                                   years=years, 
                                   ages=ages,
                                   sexes=sexes,
                                   subgroups=subgroups,
                                   keep.dimensions=keep.dimensions)
    else if (data.type=='prevalence')
        rv = extract.prevalence(sim,
                                years=years, 
                                ages=ages,
                                sexes=sexes,
                                subgroups=subgroups,
                                keep.dimensions=keep.dimensions)
    
    else if (data.type=='population')
        rv = extract.population(sim,
                                years=years, 
                                ages=ages,
                                sexes=sexes,
                                subgroups=subgroups,
                                hiv.status=hiv.status,
                                keep.dimensions=keep.dimensions)
    
    else if (data.type=='hiv.mortality')
        rv = extract.hiv.mortality(sim,
                                   years=years, 
                                   ages=ages,
                                   sexes=sexes,
                                   subgroups=subgroups,
                                   keep.dimensions=keep.dimensions)
    
    else if (data.type=='suppression')
        rv = extract.total.suppressed(sim,
                                      years=years, 
                                      ages=ages,
                                      sexes=sexes,
                                      subgroups=subgroups,
                                      keep.dimensions=keep.dimensions)
        
    else if (data.type=='annual.suppression')
        rv = extract.annual.suppression(sim,
                                        years=years, 
                                        ages=ages,
                                        sexes=sexes,
                                        subgroups=subgroups,
                                        keep.dimensions=keep.dimensions)
    else if (data.type=='engagement')
        rv = extract.total.engaged(sim,
                                   years=years, 
                                   ages=ages,
                                   sexes=sexes,
                                   subgroups=subgroups,
                                   keep.dimensions=keep.dimensions)
    
    else if (data.type=='annual.engagement')
        rv = extract.annual.engagement(sim,
                                       years=years, 
                                       ages=ages,
                                       sexes=sexes,
                                       subgroups=subgroups,
                                       keep.dimensions=keep.dimensions)
    
    else if (data.type=='disengagement.suppressed')
        rv = extract.disengagement.suppressed(sim,
                                              years=years, 
                                              ages=ages,
                                              sexes=sexes,
                                              subgroups=subgroups,
                                              keep.dimensions=keep.dimensions)
    
    else if (data.type=='disengagement.unsuppressed')
        rv = extract.disengagement.unsuppressed(sim,
                                                years=years, 
                                                ages=ages,
                                                sexes=sexes,
                                                subgroups=subgroups,
                                                keep.dimensions=keep.dimensions)
    
    else stop("not a valid data type")
    # fill in other ones
    
    # this only works if year is the first dimension (put in an error above)
    if (scale.population)
        rv = rv*(as.numeric(get.surveillance.data(data.manager, data.type = 'population', years = years, keep.dimensions='year')) /
                     as.numeric(extract.population(sim, years = years, keep.dimensions = 'year')))
    
    rv
}

# Used to pull four-dimensional results from the simulation (i.e., population) that are indexed 
# age, sex, subgroup, HIV status (also includes year as the first dimension) 
do.extract.4D <- function(sim,
                          arr, #specific subset array that is needed for the reporting
                          years = sim$years, #years to include in the report (other years are excluded)
                          ages = sim$AGES, 
                          sexes = sim$SEXES,
                          subgroups = sim$SUBGROUPS, 
                          hiv.status = sim$HIV.STATUS,
                          keep.dimensions = 'year', #collapse all other dimensions & report the data as total value over this dimension
                          age.mapping = MODEL.TO.SURVEILLANCE.AGE.MAPPING){
    
    # make sure keep dimensions are valid; put them in the right order 
    keep.dimensions = check.keep.dimensions(keep.dimensions = keep.dimensions,
                                            arr=arr)
    
    #making sure that inputs are entered as character and not numerical indexes (if so, return the char values)
    if (!is.character(ages))
        ages = sim$AGES[ages]
    
    age.brackets = map.ages(to.map = ages, mapping = age.mapping, map.to.options = sim$AGES)
    
    if (!is.character(subgroups))
        subgroups = sim$SUBGROUPS[subgroups]
    
    if (!is.character(sexes))
        sexes = sim$SEXES[sexes]
    
    if (!is.character(hiv.status))
        hiv.status = sim$HIV.STATUS[hiv.status]
    
    
    #full names of all dimensions
    full.dim.names = list(
        year = years,
        age = ages,
        sex = sexes,
        subgroup = subgroups,
        hiv.status = hiv.status
    )
    
    if(length(ages)==length(unlist(age.brackets)) && all(ages==unlist(age.brackets))){
        #filter the input array for selected years and attributes
        x=arr[as.character(years),ages,sexes,subgroups, hiv.status]
        
        dim.x=sapply(full.dim.names, length)
        dim(x)=dim.x
        dimnames(x)=full.dim.names
    }
    
    else {
        ages.to.pull = unique(unlist(age.brackets))
        
        y=arr[as.character(years),ages.to.pull,sexes,subgroups, hiv.status]
        y.dim.names = full.dim.names
        y.dim.names$age  = ages.to.pull
        dim(y) = sapply(y.dim.names, length)
        dimnames(y) = y.dim.names
        
        if(any(keep.dimensions=="age")){
            
            x = array(0, dim = sapply(full.dim.names, length), dimnames = full.dim.names)
            
            for (i in 1:length(ages)){
                sub.y = y[,age.brackets[[i]],,,]
                sub.y.dim.names = y.dim.names
                sub.y.dim.names$age = age.brackets[[i]]
                dim(sub.y) = sapply(sub.y.dim.names, length)
                dimnames(sub.y) = sub.y.dim.names
                
                x[,i,,,] = apply(sub.y, c("year","sex","subgroup","hiv.status"), sum)
            }
        }
        else
            x=y
    }
    
    #filtering unwanted dimensions out
    keep.dim.names = full.dim.names[keep.dimensions]
    #summing over dimensions that are to keep
    rv = apply(x, keep.dimensions, sum)
    #adjusting dimension names and 
    dim(rv) = sapply(keep.dim.names, length)
    dimnames(rv) = keep.dim.names
    
    rv
}


# Used to pull three-dimensional results from the simulation (i.e., incidence) that is only 
# within the HIV population and is indexed age, sex, subgroup (again includes year as first dimension) 
do.extract.3D <- function(sim,
                          arr,
                          years = sim$years,
                          ages = sim$AGES,
                          sexes = sim$SEXES,
                          subgroups = sim$SUBGROUPS,
                          keep.dimensions = 'year',
                          age.mapping = MODEL.TO.SURVEILLANCE.AGE.MAPPING
){
    # make sure keep dimensions are valid; put them in the right order 
    keep.dimensions = check.keep.dimensions(keep.dimensions = keep.dimensions,
                                            arr=arr)
    
    #making sure that inputs are entered as character and not numerical indexes (if so, return the char values)
    if (!is.character(ages))
        ages = sim$AGES[ages]
    
    age.brackets = map.ages(to.map = ages, mapping = age.mapping, map.to.options = sim$AGES)
    
    if (!is.character(subgroups))
        subgroups = sim$SUBGROUPS[subgroups]
    
    if (!is.character(sexes))
        sexes = sim$SEXES[sexes]
    
    #the full names of the array including all dimension
    full.dim.names=list(
        year=years,
        age=ages,
        sex=sexes,
        subgroup=subgroups
    )
    
    if(length(ages)==length(unlist(age.brackets)) && all(ages==unlist(age.brackets))){
        #filter the input array for selected years and attributes
        x=arr[as.character(years),ages,sexes,subgroups]
        
        dim.x=sapply(full.dim.names, length)
        dim(x)=dim.x
        dimnames(x)=full.dim.names
    }
    
    else {
        ages.to.pull = unique(unlist(age.brackets))
        
        y=arr[as.character(years),ages.to.pull,sexes,subgroups]
        y.dim.names = full.dim.names
        y.dim.names$age  = ages.to.pull
        dim(y) = sapply(y.dim.names, length)
        dimnames(y) = y.dim.names
        
        if(any(keep.dimensions=="age")){
            
            x = array(0, dim = sapply(full.dim.names, length), dimnames = full.dim.names)
            
            for (i in 1:length(ages)){
                sub.y = y[,age.brackets[[i]],,]
                sub.y.dim.names = y.dim.names
                sub.y.dim.names$age = age.brackets[[i]]
                dim(sub.y) = sapply(sub.y.dim.names, length)
                dimnames(sub.y) = sub.y.dim.names
                
                x[,i,,] = apply(sub.y, c("year","sex","subgroup"), sum)
            }
        }
        else
            x=y
    }
    
    #filtering the unwanted dimension names out
    keep.dim.names=full.dim.names[keep.dimensions]
    #summing over dimensions that are to keep
    rv= apply(x,keep.dimensions,sum)
    #adjusting the name and dimensions for the result
    dim(rv)=sapply(keep.dim.names,length)        
    dimnames(rv)=keep.dim.names
    # dim(rv)                
    rv                
}

# Call to do.extract.4D; pulls population 
extract.population <- function(sim,
                               years = sim$years,
                               ages = sim$AGES,
                               subgroups = sim$SUBGROUPS,
                               sexes = sim$SEXES,
                               hiv.status = sim$HIV.STATUS,
                               keep.dimensions = 'year'){
    
    do.extract.4D(
        sim = sim,
        arr = sim$population,
        years = years,
        ages = ages,
        subgroups = subgroups,
        sexes = sexes,
        hiv.status = hiv.status,
        keep.dimensions = keep.dimensions
    )
    
}

# Call to do.extract.3D; pulls incidence
extract.incidence <- function(sim,
                              years = sim$years,
                              ages = sim$AGES,
                              subgroups = sim$SUBGROUPS,
                              sexes = sim$SEXES,
                              keep.dimensions = 'year'){
    do.extract.3D(
        sim = sim,
        arr = sim$incidence,
        years = years,
        ages = ages,
        subgroups = subgroups,
        sexes = sexes,
        keep.dimensions = keep.dimensions
    )
}

# Call to extract.population with HIV status set only to HIV states (need to keep fourth dimension of 
# HIV status to differentiate between cascade states, but only pull those with HIV); pulls prevalence 
extract.prevalence <- function(sim,
                               years = sim$years,
                               ages = sim$AGES,
                               subgroups = sim$SUBGROUPS,
                               sexes = sim$SEXES,
                               keep.dimensions = 'year'){
    extract.population(
        sim = sim,
        years = years,
        ages = ages,
        subgroups = subgroups,
        sexes = sexes,
        keep.dimensions = keep.dimensions,
        hiv.status = sim$HIV.STATES
    )
    
}

# Call to extract.population with HIV status set only to diagnosed states; pulls prevalence 
extract.total.aware <- function(sim,
                                  years = sim$years,
                                  ages = sim$AGES,
                                  subgroups = sim$SUBGROUPS,
                                  sexes = sim$SEXES,
                                  keep.dimensions = 'year'){
    extract.population(
        sim = sim,
        years = years,
        ages = ages,
        subgroups = subgroups,
        sexes = sexes,
        keep.dimensions = keep.dimensions,
        hiv.status = sim$DIAGNOSED.STATES
    )
}


# Call to do.extract.3D; pulls new diagnoses 
extract.new.diagnoses <- function(sim,
                                  years = sim$years,
                                  ages = sim$AGES,
                                  subgroups = sim$SUBGROUPS,
                                  sexes = sim$SEXES,
                                  keep.dimensions = 'year'){
    do.extract.3D(
        sim = sim,
        arr = sim$diagnoses,
        years = years,
        ages = ages,
        subgroups = subgroups,
        sexes = sexes,
        keep.dimensions = keep.dimensions
    )
}


# Call to do.extract.4D; pulls hiv mortality
extract.hiv.mortality <- function(sim,
                                  years = sim$years,
                                  ages = sim$AGES,
                                  subgroups = sim$SUBGROUPS,
                                  sexes = sim$SEXES,
                                  hiv.status = sim$HIV.STATUS,
                                  keep.dimensions = 'year'){
    do.extract.4D(
        sim = sim,
        arr = sim$hiv.mortality,
        years = years,
        ages = ages,
        subgroups = subgroups,
        sexes = sexes,
        hiv.status = hiv.status,
        keep.dimensions = keep.dimensions
    )
}

# Call to extract.population with HIV status set only to suppressed states; pulls prevalence 
extract.total.suppressed <- function(sim,
                                  years = sim$years,
                                  ages = sim$AGES,
                                  subgroups = sim$SUBGROUPS,
                                  sexes = sim$SEXES,
                                  keep.dimensions = 'year'){
    extract.population(
        sim = sim,
        years = years,
        ages = ages,
        subgroups = subgroups,
        sexes = sexes,
        keep.dimensions = keep.dimensions,
        hiv.status = "engaged_suppressed"
    )
}

# Call to do.extract.3D; pulls annual suppression
extract.annual.suppression <- function(sim,
                                years = sim$years,
                                ages = sim$AGES,
                                subgroups = sim$SUBGROUPS,
                                sexes = sim$SEXES,
                                keep.dimensions = 'year'){
    do.extract.3D(
        sim = sim,
        arr = sim$suppression,
        years = years,
        ages = ages,
        subgroups = subgroups,
        sexes = sexes,
        keep.dimensions = keep.dimensions
    )
}

# Call to extract.population with HIV status set only to engaged states; pulls prevalence 
extract.total.engaged <- function(sim,
                                      years = sim$years,
                                      ages = sim$AGES,
                                      subgroups = sim$SUBGROUPS,
                                      sexes = sim$SEXES,
                                      keep.dimensions = 'year'){
    extract.population(
        sim = sim,
        years = years,
        ages = ages,
        subgroups = subgroups,
        sexes = sexes,
        keep.dimensions = keep.dimensions,
        hiv.status = sim$ENGAGED.STATES
    )
}


# Call to do.extract.3D; pulls annual engagement
extract.annual.engagement <- function(sim,
                               years = sim$years,
                               ages = sim$AGES,
                               subgroups = sim$SUBGROUPS,
                               sexes = sim$SEXES,
                               keep.dimensions = 'year'){
    do.extract.3D(
        sim = sim,
        arr = sim$engagement,
        years = years,
        ages = ages,
        subgroups = subgroups,
        sexes = sexes,
        keep.dimensions = keep.dimensions
    )
}

# Call to do.extract.3D; pulls disengagement from suppressed
extract.disengagement.suppressed <- function(sim,
                                             years = sim$years,
                                             ages = sim$AGES,
                                             subgroups = sim$SUBGROUPS,
                                             sexes = sim$SEXES,
                                             keep.dimensions = 'year'){
    do.extract.3D(
        sim = sim,
        arr = sim$disengagement.suppressed,
        years = years,
        ages = ages,
        subgroups = subgroups,
        sexes = sexes,
        keep.dimensions = keep.dimensions
    )
}

# Call to do.extract.3D; pulls disengagement from unsuppressed
extract.disengagement.unsuppressed <- function(sim,
                                               years = sim$years,
                                               ages = sim$AGES,
                                               subgroups = sim$SUBGROUPS,
                                               sexes = sim$SEXES,
                                               keep.dimensions = 'year'){
    do.extract.3D(
        sim = sim,
        arr = sim$disengagement.unsuppressed,
        years = years,
        ages = ages,
        subgroups = subgroups,
        sexes = sexes,
        keep.dimensions = keep.dimensions
    )
}

# Function to make sure keep dimensions are valid and returns array in the right order 
check.keep.dimensions = function(keep.dimensions,
                                 arr){
    
    arr.dimensions = names(dimnames(arr))
    
    invalid.keep.dimensions = setdiff(keep.dimensions,arr.dimensions)
    
    if(length(invalid.keep.dimensions)>0)
        stop(paste0("Invalid keep dimensions: ",paste0(invalid.keep.dimensions,collapse = ",")))
    
    intersect(arr.dimensions,keep.dimensions) # only keep the dimensions that are in both but put in the order of the array
}
