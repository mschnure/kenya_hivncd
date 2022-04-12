################################################################################################
################
#Description: These functions are used to extract surveillance data used to calibrate the model
################
################################################################################################
library(data.table)

# Add in what are the eligible values for each of the arguments below; as a comment
# Add in protections against bad inputs - warnings, etc. (although this should be robust; can pass anything)
get.surveillance.data = function(data.manager,
                                 data.type,
                                 years = 2010:2015,
                                 ages = data.manager$AGES,
                                 sexes = data.manager$SEXES, #will have to fill this in later
                                 subgroups = data.manager$SUBGROUPS,
                                 keep.dimensions = 'year')
{
        # keep.dimensions must have year; or if we ask for some ages but don't keep ages, for example
        # check for these conditions ^; if they occur, give an error
       
        pull.years = TRUE
        pull.ages = any(keep.dimensions=='age') 
        pull.sexes = any(keep.dimensions=='sex') 
        pull.subgroups = any(keep.dimensions=='subgroup') 
        
        pull.dimensions = c('year','age','sex','subgroup')
        pull.dimensions = pull.dimensions[c(pull.years, pull.ages, pull.sexes, pull.subgroups)]
        
        dim.names = list(year=as.character(years),
                         age=ages,
                         sex=sexes,
                         subgroup=subgroups
        )
        dim.names = dim.names[pull.dimensions]
        
        rv = array(as.numeric(NA), 
                   dim = sapply(dim.names, length), 
                   dimnames = dim.names)
        
        if(setequal(pull.dimensions, 'year'))
                data.element = 'total'
        if(setequal(pull.dimensions, c('year','age')))
                data.element = 'age'
        if(setequal(pull.dimensions, c('year','sex')))
                data.element = 'sex'
        if(setequal(pull.dimensions, c('year','subgroup')))
                data.element = 'subgroup'
        if(setequal(pull.dimensions, c('year','age','sex')))
                data.element = 'age.sex'
        if(setequal(pull.dimensions, c('year','age','subgroup')))
                data.element = 'age.subgroup'
        if(setequal(pull.dimensions, c('year','sex','subgroup')))
                data.element = 'sex.subgroup'
        if(setequal(pull.dimensions, c('year','age','sex','subgroup'))) 
                data.element = 'age.sex.subgroup'
        
        data = data.manager[[data.type]][[data.element]]
       
        if(!is.null(data)){
                
                years.to.get = intersect(as.character(years), dimnames(data)$year)
                
                if(length(pull.dimensions)==1){
                        rv[years.to.get] = data[years.to.get]
                }
                
                else if(setequal(pull.dimensions, c('year','age'))){ 
                        ages.to.get = intersect(ages, dimnames(data)$age)
                        rv[years.to.get, ages.to.get] = data[years.to.get, ages.to.get]
                }
                
                else if (setequal(pull.dimensions, c('year','sex'))){
                        sexes.to.get = intersect(sexes, dimnames(data)$sex)
                        rv[years.to.get, sexes.to.get] = data[years.to.get, sexes.to.get]
                }
                
                else if (setequal(pull.dimensions, c('year','subgroup'))){
                        subgroups.to.get = intersect(subgroups, dimnames(data)$subgroup)
                        rv[years.to.get, subgroups.to.get] = data[years.to.get, subgroups.to.get]
                }
                
                else if (setequal(pull.dimensions, c('year','age','sex'))){
                        ages.to.get = intersect(ages, dimnames(data)$age)
                        sexes.to.get = intersect(sexes, dimnames(data)$sex)
                        rv[years.to.get, ages.to.get, sexes.to.get] = data[years.to.get, ages.to.get, sexes.to.get]
                }
                
                else if (setequal(pull.dimensions, c('year','age','subgroup'))){
                        ages.to.get = intersect(ages, dimnames(data)$age)
                        subgroups.to.get = intersect(subgroups, dimnames(data)$subgroup)
                        rv[years.to.get, ages.to.get, subgroups.to.get] = data[years.to.get, ages.to.get, subgroups.to.get]
                }
                
                else if (setequal(pull.dimensions, c('year','sex','subgroup'))){
                        sexes.to.get = intersect(sexes, dimnames(data)$sex)
                        subgroups.to.get = intersect(subgroups, dimnames(data)$subgroup)
                        rv[years.to.get, sexes.to.get, subgroups.to.get] = data[years.to.get, sexes.to.get, subgroups.to.get]
                }
                
                else if (setequal(pull.dimensions, c('year','age','sex','subgroup'))){
                        ages.to.get = intersect(ages, dimnames(data)$age)
                        sexes.to.get = intersect(sexes, dimnames(data)$sex)
                        subgroups.to.get = intersect(subgroups, dimnames(data)$subgroup)
                        rv[years.to.get, ages.to.get, sexes.to.get, subgroups.to.get] = data[years.to.get, ages.to.get, sexes.to.get, subgroups.to.get]
                }
                
                else stop("incorrect dimensions")
                
        }
        
        dim(rv) = sapply(dim.names, length)
        dimnames(rv) = dim.names
        
        rv
}

#### Read in all data types ####
# Calls lower-level function, read.surveillance.data.type
read.surveillance.data = function(dir = 'data/raw_data'){
        rv = list(date.created = Sys.Date(),
                  AGES=c('0-14','10-19','15-24','15-49','15+','50 and over')
                  )
        
        rv$incidence = read.surveillance.data.type(data.type = 'incidence')
        
        rv$prevalence = read.surveillance.data.type(data.type = 'prevalence')
        
        rv$population = read.population.data.files(data.type = "population")
        
        rv$SUBGROUPS = dimnames(rv$incidence$subgroup)$subgroup
        
        rv$SEXES = c('male','female')
        
        rv
}

#### Read by data type (e.g., incidence, prevalence, etc.) ####
# Calls lower-level function, either read.surveillance.data.files (no stratification) or read.surveillance.data.stratified
read.surveillance.data.type = function(data.type){
        rv=list()
        
        rv$total = read.surveillance.data.files(data.type=data.type, 
                                                age='All ages')
        
        
        rv$subgroup = read.surveillance.data.files(data.type=data.type,
                                                   age='All ages',
                                                   regions = T)
        
        ## Ages ##
        rv$age = read.surveillance.data.stratified(data.type=data.type,
                                                   strata = 'age',
                                                   regions = F)
        
        rv$age.subgroup = read.surveillance.data.stratified(data.type=data.type,
                                                            strata = 'age',
                                                            regions = T)
        
        ## Sexes ##
        rv$sex = NULL 
        
        rv$sex.subgroup = NULL
        
        rv
}
        
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
                        
                        for(i in 1:length(ages)){
                                x = read.surveillance.data.files(data.type=data.type,
                                                                 age=ages[i],
                                                                 regions = T)
                                
                                rv[,i,] = x 
                                }
                }
                
                ## Pull TOTAL AGE array
                else 
                {
                        age1 = read.surveillance.data.files(data.type=data.type,
                                                            age=ages[1],
                                                            regions = F)
                        
                        dim.names = c(dimnames(age1), list(age=ages))
                        
                        rv = array(NA,
                                   dim = sapply(dim.names, length),
                                   dimnames = dim.names)

                        
                        for(i in 1:length(ages)){
                                x = read.surveillance.data.files(data.type=data.type,
                                                                 age=ages[i],
                                                                 regions = F)
                                
                                rv[,i] = x }
                }
                
        }
        
        else stop("only currently set up for age strata") ## fill in later with sex 

        rv
        
} 



#### Read individual data files without stratification (lowest-level function) ####
read.surveillance.data.files = function(dir = 'data/raw_data',
                                        data.type,
                                        regions = F,
                                        age,
                                        suffix = "")
{
        sub.dir = file.path(dir, data.type)
        
        files = list.files(file.path(sub.dir))
        
        ## Total and subgroups
        age.to.match = gsub("\\+", "\\\\+", age)
        file = files[grepl(age.to.match,files)]
        
        if (length(file)!=1)
                stop("can only pull one file at a time")
        
        one.df = read.csv(file.path(sub.dir,file), row.names = 1)
        colnames(one.df) = substring(colnames(one.df),2)
        years = unique(substr(colnames(one.df),1,4))
        subgroup.names = rownames(one.df)[-nrow(one.df)]
        
        one.df.t = transpose(one.df)
        rownames(one.df.t) <- colnames(one.df)
        colnames(one.df.t) <- rownames(one.df)
        
        ## Put in checks so I manually pull things that will give NAs (e.g., "<", so that any actual errors will show up)
        
        ## Need to put in check for suffix somehow
        ## Total ##
        dim.names.total = list(year=as.character(paste0(years, suffix))
        )
        
        total =  array(as.integer(gsub(" ","",one.df.t[paste0(years, suffix),ncol(one.df.t)])),
                       dim = sapply(dim.names.total, length), 
                       dimnames = dim.names.total)
        
        
        ## Subgroups ##
        dim.names.subgroups = list(year=as.character(paste0(years, suffix)),
                                   subgroup=subgroup.names
        )
        
        
        subgroups =  array(as.integer(sapply(one.df.t[paste0(years, suffix),1:(length(subgroup.names))], gsub, pattern = " ",replacement = "")),
                           dim = sapply(dim.names.subgroups, length), 
                           dimnames = dim.names.subgroups)
        
        
        
        if(regions==T)
                return(subgroups)
        
        else if(regions==F)
                return(total)
                
                
}


#### Read demographic data (lowest-level function) ####
read.population.data.files = function(dir = 'data/raw_data',
                                      data.type = "population")
{
        sub.dir = file.path(dir, data.type)
        
        files = list.files(file.path(sub.dir))
        
        pop.file = "PopulationByAgeSex"
        file = files[grepl(pop.file,files)]
        
        if (length(file)!=1)
                stop("can only pull one file at a time")
        
        one.df = read.csv(file.path(sub.dir,file))
        one.df = one.df[one.df$Location=="Kenya",]
        years = unique(one.df$Time)
        ages = unique(one.df$AgeGrp)
        
        one.df$AgeGrp = factor(one.df$AgeGrp, levels = ages)
        one.df.sorted = one.df[order(one.df$AgeGrp),]
        
        pop.dim.names = list(year = as.character(years),
                              age = ages)
        
        total = array(0,
                      dim = sapply(pop.dim.names, length), 
                      dimnames = pop.dim.names)
        
        total[] = as.numeric(one.df.sorted[,"PopTotal"])
        total = cbind(total,rowSums(total))
        colnames(total) = c(ages,"total")
        
        male = array(0,
                      dim = sapply(pop.dim.names, length), 
                      dimnames = pop.dim.names)
        
        male[] = as.numeric(one.df.sorted[,"PopMale"])
        male = cbind(male,rowSums(male))
        colnames(male) = c(ages,"total")
        
        female = array(0,
                     dim = sapply(pop.dim.names, length), 
                     dimnames = pop.dim.names)
        
        female[] = as.numeric(one.df.sorted[,"PopFemale"])
        female = cbind(female,rowSums(female))
        colnames(female) = c(ages,"total")

        rv = list()
        rv$total = total[,"total"]*1000
        rv$both = total*1000
        rv$male = male*1000
        rv$female = female*1000

        rv
}


