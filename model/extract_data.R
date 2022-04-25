################################################################################################
################
#Description: These functions are used to extract different information from the ODE result
################
################################################################################################

# GENERAL FORMAT
# extract.data = function(sim,
#                         #ODE result
#                         data.type,
#                         #type of reported data (states, diagnosis, etc)
#                         years = sim$years,
#                         ages = sim$AGES,
#                         subgroups = sim$SUBGROUPS,
#                         sexes = sim$SEXES,
#                         hiv.status = sim$HIV.STATUS,
#                         keep.dimensions = 'year' #data is summed for each element in this dimention (year: return total data for each year)))
# )
# {
#         
# }

#### CORE FUNCTIONS #### 
extract.data = function(sim,
                        data.type,
                        data.manager = DATA.MANAGER,
                        years = sim$years, #years to include in the report (other years are excluded)
                        ages = sim$AGES, 
                        sexes = sim$SEXES,
                        subgroups = sim$SUBGROUPS, 
                        hiv.status = sim$HIV.STATUS,
                        keep.dimensions = 'year', #collapse all other dimensions & report the data as total value over this dimension
                        scale.population = F # change once I'm pulling population data
                        )
        
{
        if(all(keep.dimensions!='year'))
                stop("must keep year dimension")
           
                if (data.type=='incidence')
                rv = extract.incidence(sim, 
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
                                        keep.dimensions=keep.dimensions)
        
        else stop("not a valid data type")
        # fill in other ones
        
        # this only works if year is the first dimension (put in an error above)
        if (scale.population)
               rv = rv*(as.numeric(get.surveillance.data(data.manager, data.type = 'population', years = years, keep.dimensions='year')) /
                        as.numeric(extract.population(sim, years = years, keep.dimensions = 'year')))
        
        rv
}


do.extract.4D <- function(sim,
                          arr, #specific subset array that is needed for the reporting
                          years = sim$years, #years to include in the report (other years are excluded)
                          ages = sim$AGES, 
                          sexes = sim$SEXES,
                          subgroups = sim$SUBGROUPS, 
                          hiv.status = sim$HIV.STATUS,
                          keep.dimensions = 'year', #collapse all other dimensions & report the data as total value over this dimension
                          age.mapping = MODEL.TO.SURVEILLANCE.AGE.MAPPING) 
{
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
        
        #filtering unwanted dimensions out
        keep.dim.names = full.dim.names[keep.dimensions]
        #summing over dimensions that are to keep
        rv = apply(x, keep.dimensions, sum)
        #adjusting dimension names and 
        dim(rv) = sapply(keep.dim.names, length)
        dimnames(rv) = keep.dim.names
        
        rv
}



do.extract.3D <- function(sim,
                          arr,
                          years = sim$years,
                          ages = sim$AGES,
                          sexes = sim$SEXES,
                          subgroups = sim$SUBGROUPS,
                          keep.dimensions = 'year',
                          age.mapping = MODEL.TO.SURVEILLANCE.AGE.MAPPING
                          )
{
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

#### FUNCTIONS TO EXTRACT SPECIFIC OUTPUTS #### 
extract.population <- function(sim,
                               years = sim$years,
                               ages = sim$AGES,
                               subgroups = sim$SUBGROUPS,
                               sexes = sim$SEXES,
                               hiv.status = sim$HIV.STATUS,
                               keep.dimensions = 'year')
{
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

extract.incidence <- function(sim,
                              years = sim$years,
                              ages = sim$AGES,
                              subgroups = sim$SUBGROUPS,
                              sexes = sim$SEXES,
                              keep.dimensions = 'year')
{
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

extract.prevalence <- function(sim,
                               years = sim$years,
                               ages = sim$AGES,
                               subgroups = sim$SUBGROUPS,
                               sexes = sim$SEXES,
                               keep.dimensions = 'year')
{
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

extract.new.diagnoses <- function(sim,
                                  years = sim$years,
                                  ages = sim$AGES,
                                  subgroups = sim$SUBGROUPS,
                                  sexes = sim$SEXES,
                                  keep.dimensions = 'year')
{
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







