################################################################################################
################
#Description: These functions are used to extract surveillance data used to calibrate the model
################
################################################################################################
library(data.table)


get.surveillance.data = function(data.manager,
                                 subgroups = data.manager$SUBGROUPS,
                                 data.type,
                                 years,
                                 ages = data.manager$AGES,
                                 sexes = NULL, #will have to fill this in later
                                 keep.dimensions,
                                 error.statement)
{
       pull.years = TRUE
       pull.ages = any(keep.dimensions=='age') || !setequal(ages, data.manager$AGES) 
       pull.sexes = any(keep.dimensions=='sex') || !setequal(sexes, data.manager$SEXES)
       pull.subgroups = any(keep.dimensions=='subgroups') || !setequal(subgroups, data.manager$SUBGROUPS)
        
       pull.dimensions = c('year','age','sex','subgroup')
       pull.dimensions = pull.dimensions[c(pull.years, pull.ages, pull.sexes, pull.subgroups)]
        
        dim.names = list(year=as.character(years),
                        age=ages,
                        sex=sexes,
                        subgroup=subgroups
                        )
       dim.names = dim.names[pull.dimensions]
       
        rv = array(NA, 
                   dim = sapply(dim.names, length), 
                   dimnames = dim.names)
        
        
        ## TO REVIEW/CHECK - 1/3 ##
        if(pull.dimensions=='year')
                data.element = total
        if(setequal(pull.dimensions, c('year','age')))
                data.element = age
        if(setequal(pull.dimensions, c('year','sex')))
                data.element = sex
        if(setequal(pull.dimensions, c('year','subgroup')))
                data.element = subgroup
        if(setequal(pull.dimensions, c('year','age','subgroup')))
                data.element = age.subgroup
        if(setequal(pull.dimensions, c('year','sex','subgroup')))
                data.element = sex.subgroup
        if(setequal(pull.dimensions, c('year','age','sex','subgroup'))) # don't actually have this as an option right now
                data.element = age.sex.subgroup
        
        data = data.manager[[data.type]][[data.element]]
        
        
        if(!is.null(data)){

                years.to.get = intersect(as.character(years), dimnames(data)$year)
                
                if(length(keep.dimensions==1)){
                        rv[years.to.get] = data[years.to.get]
                }
                
                ## TO REVIEW/CHECK - 2/3 ##
                else if(setequal(pull.dimensions, c('year','age'))){
                        ages.to.get = intersect(ages, dimnames(data)$age)
                        rv[ages.to.get] = data[ages.to.get]
                }
                
                else if (setequal(pull.dimensions, c('year','sex'))){
                        sexes.to.get = intersect(sexes, dimnames(data)$sex)
                        rv[sexes.to.get] = data[sexes.to.get]
                }
                
                else if (setequal(pull.dimensions, c('year','subgroup'))){
                        subgroups.to.get = intersect(subgroups, dimnames(data)$subgroup)
                        rv[subgroups.to.get] = data[subgroups.to.get]
                }
                
                else if (setequal(pull.dimensions, c('year','age','subgroup'))){
                        ages.to.get = intersect(ages, dimnames(data)$age)
                        rv[ages.to.get] = data[ages.to.get]
                        subgroups.to.get = intersect(subgroups, dimnames(data)$subgroup)
                        rv[subgroups.to.get] = data[subgroups.to.get]
                }
                
                else if (setequal(pull.dimensions, c('year','sex','subgroup'))){
                        sexes.to.get = intersect(sexes, dimnames(data)$sex)
                        rv[sexes.to.get] = data[sexes.to.get]
                        subgroups.to.get = intersect(subgroups, dimnames(data)$subgroup)
                        rv[subgroups.to.get] = data[subgroups.to.get]
                }
                
                else if (setequal(pull.dimensions, c('year','age','sex','subgroup'))){
                        ages.to.get = intersect(ages, dimnames(data)$age)
                        rv[ages.to.get] = data[ages.to.get]
                        sexes.to.get = intersect(sexes, dimnames(data)$sex)
                        rv[sexes.to.get] = data[sexes.to.get]
                        subgroups.to.get = intersect(subgroups, dimnames(data)$subgroup)
                        rv[subgroups.to.get] = data[subgroups.to.get]
                }
                
                else stop("incorrect dimensions")
        
        }
        

        if(!setequal(keep.dimensions, pull.dimensions)){
                
                keep.dimensions = intersect(pull.dimensions, keep.dimensions) # keep.dimensions is a subset of pull.dimensions; but puts it in the right order
                
                rv = apply(rv, keep.dimensions, sum) # won't work for percentages - if we use in the future
                
                dim.names = dim.names[keep.dimensions]
                dim(rv) = sapply(dim.names, length)
                dimnames(rv) = dim.names
                
                
                # add in a check for numbers versus proportions 
        }
        rv
}

#### Read in all data types ####
# Calls lower-level function, read.surveillance.data.type
read.surveillance.data = function(dir){
        rv = list(date.created = Sys.Date(),
                  AGES=c('0-14','10-19','15-24','15-49','15+','50 and over')
                  )
        
        rv$new = read.surveillance.data.type(data.type = 'new')
        
        rv$prevalence = read.surveillance.data.type(data.type = 'prevalence')
        
        rv$SUBGROUPS = dimnames(rv$new$subgroups)$subgroup
        
        rv
}

#### Read by data type (e.g., new, prevalence, etc.) ####
# Calls lower-level function, either read.surveillance.data.files (no stratification) or read.surveillance.data.stratified
read.surveillance.data.type = function(data.type){
        rv=list()
        
        if(data.type=='new')
        {
                rv$total = read.surveillance.data.files(data.type='new', #Can I just make data.type = data.type here? Rather than the if statement?
                                                        age='All ages')
                
                
                rv$subgroup = read.surveillance.data.files(data.type='new',
                                                           age='All ages',
                                                           regions = T)
                
                ## Ages ##
                rv$age = read.surveillance.data.stratified(data.type='new',
                                                           strata = 'age',
                                                           regions = F)
                
                rv$age.subgroup = read.surveillance.data.stratified(data.type='new',
                                                                    strata = 'age',
                                                                    regions = T)
                
                ## Sexes ##
                rv$sex = NULL 
                
                rv$sex.subgroup = NULL
        }
        
        if(data.type=='prevalence')
        {
                rv$total = read.surveillance.data.files(data.type='prevalence',
                                                        age='All ages')
                
                
                rv$subgroup = read.surveillance.data.files(data.type='prevalence',
                                                           age='All ages',
                                                           regions = T)
                
                ## Ages ##
                rv$age = read.surveillance.data.stratified(data.type='prevalence',
                                                           strata = 'age',
                                                           regions = F)
                
                rv$age.subgroup = read.surveillance.data.stratified(data.type='prevalence',
                                                                    strata = 'age',
                                                                    regions = T)
                
                ## Sexes ##
                rv$sex = NULL 
                
                rv$sex.subgroup = NULL
        }
        
        rv
}
        
## TO REVIEW/CHECK - 3/3 ##
#### Read individual data files WITH stratification ####
# Calls lower-level function, read.surveillance.data.files
read.surveillance.data.stratified = function(data.type,
                                             strata,
                                             regions=T)
{
        ages=c('0-14','10-19','15-24','15-49','15+','50 and over','All ages')
        
        ## Pull array for age
        if(strata=='age')
        {
                ## Pull AGE array by REGION
                if(regions)
                {
                        age1 = read.surveillance.data.files(data.type=data.type,
                                                            age=ages[1],
                                                            regions = T)
                        
                        dim.names = c(dimnames(age1), list(age=ages))
                        dim.names = dim.names[c(1,3,2)]
                        
                        rv = array(NA,
                                   dim = sapply(dim.names, length),
                                   dimnames = dim.names)
                        
                        rv[,1,] = age1
                        
                        # I don't understand why this for loop isn't working 
                        for(i in 1:length(ages)){
                                x = read.surveillance.data.files(data.type=data.type,
                                                                 age=ages[i],
                                                                 regions = T)
                                
                                rv[,i,] = x }
                }
                
                ## Pull TOTAL AGE array
                else 
                {
                        age1 = read.surveillance.data.files(data.type=data.type,
                                                            age=ages[1],
                                                            regions = F)
                        
                        dim.names = c(dimnames(age1), list(age=ages))
                        dim.names = dim.names[c(1,3,2)]
                        
                        rv = array(NA,
                                   dim = sapply(dim.names, length),
                                   dimnames = dim.names)
                        
                        rv[,1,] = age1
                        
                        # FIX THIS ONE TOO
                        for(i in 1:length(ages)){
                                x = read.surveillance.data.files(data.type=data.type,
                                                                 age=ages[i],
                                                                 regions = F)
                                
                                rv[,i,] = x }
                }
                
        }
        
        else stop("only currently set up for age strata") ## fill in later with sex 

        
} 



#### Read individual data files without stratification (lowest-level function) ####
read.surveillance.data.files = function(dir = 'data/raw_data',
                                        data.type,
                                        regions = F,
                                        age,
                                        suffix = NA)
{
        sub.dir = file.path(dir, data.type)
        
        files = list.files(file.path(sub.dir))
        
        ## Total and subgroups
        file = files[grepl(age,files)]
        
        one.df = read.csv(file.path(sub.dir,file), row.names = 1)
        colnames(one.df) = substring(colnames(one.df),2)
        years = unique(substr(colnames(one.df),1,4))
        subgroup.names = rownames(one.df)[-nrow(one.df)]
        
        one.df.t = transpose(one.df)
        rownames(one.df.t) <- colnames(one.df)
        colnames(one.df.t) <- rownames(one.df)
        
        if (is.na(suffix))
        {
                ## Total ##
                dim.names.total = list(year=as.character(years)
                )
                
                total =  array(as.integer(gsub(" ","",one.df.t[years,ncol(one.df.t)])),
                               dim = sapply(dim.names.total, length), 
                               dimnames = dim.names.total)
                
                
                ## Subgroups ##
                dim.names.subgroups = list(year=as.character(years),
                                           subgroup=subgroup.names
                )
                
                
                subgroups =  array(as.integer(sapply(one.df.t[years,1:(length(subgroup.names))], gsub, pattern = " ",replacement = "")),
                                   dim = sapply(dim.names.subgroups, length), 
                                   dimnames = dim.names.subgroups)
                
        }
        
        ## Just added this in - check later 
        if(!is.na(suffix))
        {
                ## Total ##
                dim.names.total = list(year=as.character(paste0(years, "_",suffix))
                )
                
                total =  array(as.integer(gsub(" ","",one.df.t[paste0(years, "_",suffix),ncol(one.df.t)])),
                               dim = sapply(dim.names.total, length), 
                               dimnames = dim.names.total)
                
                
                ## Subgroups ##
                dim.names.subgroups = list(year=as.character(paste0(years, "_",suffix)),
                                           subgroup=subgroup.names
                )
                
                
                subgroups =  array(as.integer(sapply(one.df.t[paste0(years, "_",suffix),1:(length(subgroup.names))], gsub, pattern = " ",replacement = "")),
                                   dim = sapply(dim.names.subgroups, length), 
                                   dimnames = dim.names.subgroups)
        }
        
        
        if(regions==T)
                return(subgroups)
        
        else if(regions==F)
                return(total)
                
                
}
