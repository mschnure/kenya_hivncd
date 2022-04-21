################################################################################################
################
#Description: These functions are used to extract surveillance data used to calibrate the model
################
################################################################################################
library(data.table)

# Add in what are the eligible values for each of the arguments below; as a comment
# Add in protections against bad inputs - warnings, etc. (although this should be robust; can pass anything)

# Each data.manager[[data.type]] is a list with the following elements:  
# $AGES
# $SEXES 
# $SUBGROUPS
# $AGE.LOWERS (0-4 --> 0)
# $AGE.UPPERS (0-4 --> 5) - i.e., upper is exclusive
# $total, $age, $age.sex, etc. (this is the actual data - will vary by data type) 
# DON'T HAVE AGES/SEXES/SUBGROUPS FOR BIRTHS 

## DIDN'T WORK ON THIS FUNCTION
get.surveillance.data = function(data.manager,
                                 data.type,
                                 years = 2010:2015,
                                 ages = data.manager[[data.type]]$AGES, 
                                 sexes = data.manager[[data.type]]$SEXES, #only works for population now, not incidence/prevalence
                                 subgroups = data.manager[[data.type]]$SUBGROUPS,
                                 keep.dimensions = 'year')
{
#<<<<<<< main
        # keep.dimensions must have year; or if we ask for some ages but don't keep ages, for example
        # check for these conditions ^; if they occur, give an error
       
#        pull.years = TRUE
#        pull.ages = any(keep.dimensions=='age') 
#        pull.sexes = any(keep.dimensions=='sex') 
#        pull.subgroups = any(keep.dimensions=='subgroup') 
#=======
        pull.years = TRUE
        pull.ages = any(keep.dimensions=='age') || !setequal(ages, data.manager$AGES) #repeat for sex and subgroups
#>>>>>>> main
        
        pull.dimensions = c('year','age','sex','subgroup')
        pull.dimensions = pull.dimensions[c(pull.years, pull.ages, pull.sexes, pull.subgroups)]
        
        dim.names = list(year=as.character(years),
                         age=ages,
                         sex=sexes,
                         subgroup=subgroups
        )
        dim.names = dim.names[pull.dimensions]
        
#<<<<<<< main
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
       
#=======
        rv = array(NA, 
                   dim = sapply(dim.names, length), 
                   dimnames = dim.names)
        
        data = data.manager[[data.type]][[data.element]]
        
        # fix data element from below
        #pull.dimensions = year --> total
        #pull.dimensions = year, age --> age
        #pull.dimensions = year, subgroup --> subgroup
        #pull.dimensions = year, age, subgroup --> age.subgroup
        #pull.dimensions = year, age, sex, subgroup --> age.sex.subgroup (right now will be an error)
        
        #map the keep dimensions to what we want; remember pull.dimensions might not be passed in the right order
        
        #distinguish between keep dimensions and get dimensions 
        
#>>>>>>> main
        if(!is.null(data)){
                
                years.to.get = intersect(as.character(years), dimnames(data)$year)
                
#<<<<<<< main
                if(length(pull.dimensions)==1){
#=======
                if(length(keep.dimensions==1)){
                        
#>>>>>>> main
                        rv[years.to.get] = data[years.to.get]
                        
                }
                
#<<<<<<< main
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
#=======
                else if(setequal(pull.dimensions, c('year','age'))){
                        
                        
                        # years and ages --> pull years.to.get and ages.to.get
                        
                }
                
                # and all other combinations - redo below
                
                else if (length(pull.dimensions==3)){
                        
                }
                
                else stop("incorrect dimensions")
                
        }
        
        
        if(!setequal(keep.dimensions, pull.dimensions)){
                
                keep.dimensions = intersect(pull.dimensions, keep.dimensions) # keep.dimensions is a subset of pull.dimensions; but puts it in the right order
#>>>>>>> main
                
        }
        
        dim(rv) = sapply(dim.names, length)
        dimnames(rv) = dim.names
        
        rv
}

#<<<<<<< main
#### Read in all data types ####
# Calls lower-level function, read.surveillance.data.type
read.surveillance.data = function(dir = 'data/raw_data'){
        rv = list(date.created = Sys.Date()
                  )
#=======
read.surveillance.data = function(dir){
        rv = list(date.created = Sys.Date(),
                  AGES=c('0-14','10-19','15-24','15-49','15+','50 and over')
        )
#>>>>>>> main
        
        rv$incidence = read.surveillance.data.type(data.type = 'incidence')
        rv$incidence$AGES = c('0-14','10-19','15-24','15-49','15+','50 and over')
        rv$incidence$AGE.LOWERS = c(0,10,15,15,15,50)
        rv$incidence$AGE.UPPERS = c(15,20,25,50,Inf,Inf)
        rv$incidence$SEXES = c('male','female')
        rv$incidence$SUBGROUPS = dimnames(rv$incidence$subgroup)$subgroup
        
        rv$prevalence = read.surveillance.data.type(data.type = 'prevalence')
        rv$prevalence$AGES = c('0-14','10-19','15-24','15-49','15+','50 and over')
        rv$prevalence$AGE.LOWERS = c(0,10,15,15,15,50)
        rv$prevalence$AGE.UPPERS = c(15,20,25,50,Inf,Inf)
        rv$prevalence$SEXES = c('male','female')
        rv$prevalence$SUBGROUPS = dimnames(rv$prevalence$subgroup)$subgroup
        
        rv$population = read.population.data.files(data.type = "population")
        rv$population$AGES = c('0-4', '5-9','10-14','15-19','20-24','25-29','30-34',
                               '35-39','40-44','45-49','50-54','55-59','60-64','65-69',
                               '70-74','75-79','80-84','85-89','90-94','95-99','100+')
        rv$population$AGE.LOWERS = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100)
        rv$population$AGE.UPPERS = c(5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100,Inf)
        rv$population$SEXES = c('male','female')
        rv$population$SUBGROUPS = dimnames(rv$incidence$subgroup)$subgroup ## NO POPULATION SUBGROUPS FOR NOW
        
        rv$births = read.birth.data.files(data.type = "population")
        
        rv$deaths = read.death.data.files(data.type = "population")
        rv$deaths$AGES = c('0-4', '5-9','10-14','15-19','20-24','25-29','30-34',
                               '35-39','40-44','45-49','50-54','55-59','60-64','65-69',
                               '70-74','75-79','80-84','85-89','90-94','95+')
        rv$deaths$AGE.LOWERS = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95)
        rv$deaths$AGE.UPPERS = c(5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,Inf)
        rv$deaths$SEXES = c('male','female')
        rv$deaths$SUBGROUPS = dimnames(rv$incidence$subgroup)$subgroup ## NO POPULATION SUBGROUPS FOR NOW
        
        rv
}

#<<<<<<< main
#### Read by data type (e.g., incidence, prevalence, etc.) ####
# Calls lower-level function, either read.surveillance.data.files (no stratification) or read.surveillance.data.stratified
read.surveillance.data.type = function(data.type){
        rv=list()
        
        rv$total = read.surveillance.data.files(data.type=data.type, 
                                                age='All ages')
        
        
        rv$subgroup = read.surveillance.data.files(data.type=data.type,
#=======

read.surveillance.data.type = function(data.type #PK: eligible values? 
){
        rv=list()
        
        if(data.type=='new')
                rv$total = read.surveillance.data.files(data.type='new',
                                                        age='All ages',
                                                        regions = F)
        
        rv$subgroup = read.surveillance.data.files(data.type='new',
#>>>>>>> main
                                                   age='All ages',
                                                   regions = T)
        
        ## Ages ##
#<<<<<<< main
        rv$age = read.surveillance.data.stratified(data.type=data.type,
                                                   strata = 'age',
                                                   regions = F)
        
        rv$age.subgroup = read.surveillance.data.stratified(data.type=data.type,
                                                            strata = 'age',
                                                            regions = T)
        
        ## Sexes ##
        rv$sex = NULL 
        
        rv$sex.subgroup = NULL
=======
        # fill in with total 
        rv$age #PK: ???
        
        rv$age.subgroup = read.surveillance.data.stratified(data.type='new',
                                                            age=ages)
#>>>>>>> main
        
        rv
}


read.surveillance.data.stratified = function(strata){ #add regions argument
        
#<<<<<<< main
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
        
#=======
        ages=c('0-14','10-19','15-24','15-49','15+','50 and over','All ages')
        
        #fill in to do subgroups and total
        if(regions)
                age1 = read.surveillance.data.files(data.type='new',
                                                    age=ages[1],
                                                    regions = T)
        
        dim.names = c(dimnames(age1), list(age=ages))
        dim.names = dim.names[c(1,3,2)]
#>>>>>>> main
        
        rv = array(NA,
                   dim = sapply(dim.names, length),
                   dimnames = dim.names)
        
        rv[,1,] = age1
        
        # can for loop over the rest of them 
        
        age2 = read.surveillance.data.files(data.type='new',
                                            age=ages[2],
                                            regions = T)
        
        # total will be 2D array
        
        
        
        
}

#PK: function description?
# Reads the raw data file and returns "total" vs "regional" data over time 
read.surveillance.data.files = function(dir = 'data/raw_data',
#<<<<<<< main
                                        data.type,
                                        regions = F,
                                        age,
                                        suffix = "")
#=======
                                        data.type, #PK: eligible data.types? "cascade" "new" "prevalence" ? 
                                        regions = F, #PK: what does F/T represent?
                                        age #PK: acceptable values? is this only used as a filename extension or is actually related to age?
)
#>>>>>>> main
{
        #navigate to the subdirectory and list available files
        sub.dir = file.path(dir, data.type)
        all.files = list.files(file.path(sub.dir))
        
        # select files
        selected.files = all.files[grepl(age,all.files)]
        
#<<<<<<< main
        ## Total and subgroups
        age.to.match = gsub("\\+", "\\\\+", age)
        file = files[grepl(age.to.match,files)]
        
        if (length(file)!=1)
                stop("can only pull one file at a time")
#=======
        #PK: what happens if there is more than one selected file or no files ? need some code to protect against errors
        
        file=selected.files[1]
#>>>>>>> main
        
        one.df = read.csv(file.path(sub.dir,file), row.names = 1)
        colnames(one.df) = substring(colnames(one.df),2)
        years = unique(substr(colnames(one.df),1,4))
        subgroup.names = rownames(one.df)[-nrow(one.df)] #PK: exclude National? 
        
        one.df.t = t(one.df) #PK why transposing? 
        # rownames(one.df.t) <- colnames(one.df) #PK: this is redundant
        # colnames(one.df.t) <- rownames(one.df) #PK: this is redundant
        
#<<<<<<< main
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
        
#=======
        ## extract total values over time ##
        dim.names.total = list(year=as.character(years))
        
        total =  array(
                as.integer(gsub(" ","",one.df.t[years,ncol(one.df.t)])),#last column representing national estimates 
                dim = sapply(dim.names.total, length), 
                dimnames = dim.names.total)
        
        
        ## extract subgroup values over time ##
        dim.names.subgroups = list(year=as.character(years),
                                   subgroup=subgroup.names )
        
        
        subgroups =  array(
                as.integer(sapply(one.df.t[years,1:(length(subgroup.names))], gsub, pattern = " ",replacement = "")),
                dim = sapply(dim.names.subgroups, length), 
                dimnames = dim.names.subgroups)
#>>>>>>> main
        
        
        if(regions==T)
                return(subgroups)
        
        else if(regions==F)
                return(total)
        
        
}


#### Read population data (lowest-level function) ####
read.population.data.files = function(dir = 'data/raw_data',
                                      data.type = "population")
{
        sub.dir = file.path(dir, data.type)
        
        files = list.files(file.path(sub.dir))
        
        pop.file = "PopulationByAgeSex"
        file = files[grepl(pop.file,files)]
        
        if (length(file)!=1)
                stop("can only pull one file at a time")
        
        df = read.csv(file.path(sub.dir,file))
        df = df[df$Location=="Kenya",]
        years = unique(df$Time)
        ages = unique(df$AgeGrp)
        
        df$AgeGrp = factor(df$AgeGrp, levels = ages)
        df.sorted = df[order(df$AgeGrp),]
        
        ## Age array
        age.dim.names = list(year = as.character(years),
                              age = ages)
        
        age = array(0,
                    dim = sapply(age.dim.names, length), 
                    dimnames = age.dim.names)
        
        age[] = as.numeric(df.sorted[,"PopTotal"])*1000
        
        ## Total array
        total = array(rowSums(age),
                      dimnames = list(year = as.character(years)))

        ## Age.Sex array
        sexes = c("male","female")
        age.sex.dim.names = list(year = as.character(years),
                                 age = ages,
                                 sex = sexes)
        
        male.age = array(0,
                         dim = sapply(age.dim.names, length), 
                         dimnames = age.dim.names)
        
        male.age[] = as.numeric(df.sorted[,"PopMale"])*1000
        
        female.age = array(0,
                           dim = sapply(age.dim.names, length), 
                           dimnames = age.dim.names)
        
        female.age[] = as.numeric(df.sorted[,"PopFemale"])*1000
        
        age.sex = array(0,
                        dim = sapply(age.sex.dim.names, length), 
                        dimnames = age.sex.dim.names)
        
        age.sex[,,"male"] = male.age
        age.sex[,,"female"] = female.age
        
        ## Sex array
        male = array(rowSums(male.age),
                     dimnames = list(year = as.character(years)))
        
        female = array(rowSums(female.age),
                       dimnames = list(year = as.character(years)))
        
        sex.dim.names = list(year = as.character(years),
                             sex = sexes)
        
        sex = array(0,
                    dim = sapply(sex.dim.names, length), 
                    dimnames = sex.dim.names)
        
        sex[,"male"] = male
        sex[,"female"] = female
        
        ## Returns a list
        rv = list()
        rv$total = total
        rv$age = age
        rv$sex = sex
        rv$age.sex = age.sex

        rv
}


#### Read birth data (lowest-level function) ####
read.birth.data.files = function(dir = 'data/raw_data',
                           data.type = "population")
{
        sub.dir = file.path(dir, data.type)
        
        files = list.files(file.path(sub.dir))
        
        pop.file = "Period_Indicators"
        file = files[grepl(pop.file,files)]
        
        if (length(file)!=1)
                stop("can only pull one file at a time")
        
        df = read.csv(file.path(sub.dir,file))
        df = df[df$Location=="Kenya",]
        years = unique(df$Time)
        
        rv = list()
        
        births = array(as.numeric(df$CBR),
                       dimnames = list(year = as.character(years)))
                       
        # deaths = array(as.numeric(df$CDR),
        #                dimnames = list(year = as.character(years)))               
                                      
        rv$births = births
        # rv$deaths = deaths
                                      
        rv
}


#### Read death data (lowest-level function) ####
read.death.data.files = function(dir = 'data/raw_data',
                                 data.type = "population")
{
        sub.dir = file.path(dir, data.type)
        
        files = list.files(file.path(sub.dir))
        
        pop.file = "NumberDeaths"
        file = files[grepl(pop.file,files)]
        
        if (length(file)!=1)
                stop("can only pull one file at a time")
        
        df = read.csv(file.path(sub.dir,file))
        df = cbind(df[,c(-1,-2,-5)],df[,5])
        colnames(df) = df[1,]
        ages = as.character(df[1,3:ncol(df)])
        df = df[-5:-1,]
        
        years = unique(df$Time)
        
        # start.year = as.numeric(substr(years[1],1,4))
        # end.year = as.numeric(substr(years[length(years)],8,11))
        # years.to.fill = as.character(start.year:end.year)
        
        dim.names = list(year = years,
                         age = ages)
        
        rv = list()
        
        rv$total = array(0,
                         dim = sapply(dim.names, length), 
                         dimnames = dim.names)
        
        for (i in 3:ncol(df)){
                rv$total[,(i-2)] = as.integer(gsub(" ","",df[df$Sex=="Both sexes combined",i]))*1000
        }
        
        rv$male = array(0,
                        dim = sapply(dim.names, length), 
                        dimnames = dim.names)
        
        for (i in 3:ncol(df)){
                rv$male[,(i-2)] = as.integer(gsub(" ","",df[df$Sex=="Male",i]))*1000
        }
        
        rv$female = array(0,
                          dim = sapply(dim.names, length), 
                          dimnames = dim.names)
        
        for (i in 3:ncol(df)){
                rv$female[,(i-2)] = as.integer(gsub(" ","",df[df$Sex=="Female",i]))*1000
        }
        
        rv
        
        # Eventually, divide one period of deaths (e.g., deaths for 1950-1955) by the sum of the population from 1950-1954
        # Could alternatively divide one period of deaths by 5 and then get individual-year death rates, but those aren't exact and probably not necessary
        
}