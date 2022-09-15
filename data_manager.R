####################################################################################################
# Description: Functions used to read in and extract surveillance data for model calibration/inputs; 
# also used in plotting functions (analog to extract_data functions for simulation data) 
####################################################################################################

library(data.table)

# Higher-level core functions 
#     1. get.surveillance.data 
#     2. read.surveillance.data 
# Lower-level helper functions 
#     1. read.surveillance.data.type
#     2. read.surveillance.data.stratified 
#     3. read.surveillance.data.files 
# Specialty functions
#     1. combine.pdf.years 
#     2. read.pdf.data.files
#     3. read.population.data.files.model.ages 
#     4. read.population.data.files.all.ages 
#     5. read.fertility.data.files
#     6. read.death.data.files
#     7. read.birth.data.files (no longer using)


# Each data.manager[[data.type]] is a list with the following elements:  
# $AGES
# $SEXES 
# $SUBGROUPS
# $AGE.LOWERS (0-4 --> 0)
# $AGE.UPPERS (0-4 --> 5) - i.e., upper is exclusive
# $total, $age, $age.sex, etc. (this is the actual data - will vary by data type) 
# DON'T HAVE AGES/SEXES/SUBGROUPS FOR BIRTHS 


# TO DO NOTES: 
# Add in what are the eligible values for each of the arguments below; as a comment
# Add in protections against bad inputs - warnings, etc. (although this should be robust; can pass anything)


##---------------------------------##
##-- HIGHER-LEVEL/CORE FUNCTIONS --##
##---------------------------------##

# Extracts data from data.manager object (created using call to read.surveillance.data function); called in 
# plotting functions but can be used on its own; allows user to specify which dimensions to look at and/or 
# stratify by (see simplot function) 
get.surveillance.data = function(data.manager,
                                 data.type,
                                 years = 2010:2015,
                                 ages = data.manager[[data.type]]$AGES, 
                                 sexes = data.manager[[data.type]]$SEXES, #only works for population now, not incidence/prevalence
                                 subgroups = data.manager[[data.type]]$SUBGROUPS,
                                 keep.dimensions = 'year'){
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

# Creates data.manager object; called in source code; calls different lower-level functions for each data 
# type (based on input file type) 
read.surveillance.data = function(dir = 'data/raw_data'){
    rv = list(date.created = Sys.Date()
    )
    
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
    
    rv$hiv.mortality = read.surveillance.data.type(data.type = 'hiv.mortality')
    rv$hiv.mortality$AGES = c('0-14','10-19','15-24','15-49','15+','50 and over')
    rv$hiv.mortality$AGE.LOWERS = c(0,10,15,15,15,50)
    rv$hiv.mortality$AGE.UPPERS = c(15,20,25,50,Inf,Inf)
    rv$hiv.mortality$SEXES = NULL
    rv$hiv.mortality$SUBGROUPS = dimnames(rv$hiv.mortality$subgroup)$subgroup
    
    rv$awareness = read.cascade.data.type(sub.data.type = "status",
                                          denominator = "allPLHIV")
    rv$awareness$AGES = c('15+')
    rv$awareness$AGE.LOWERS = c(15)
    rv$awareness$AGE.UPPERS = c(Inf)
    rv$awareness$SEXES = c('male','female')
    rv$awareness$SUBGROUPS = dimnames(rv$awareness$subgroup)$subgroup

    ## Default engagement denominator = all aware PLHIV (option for all PLHIV below)
    rv$engagement = read.cascade.data.type(sub.data.type = "ART",
                                           denominator = "aware")
    rv$engagement$AGES = c('15+')
    rv$engagement$AGE.LOWERS = c(15)
    rv$engagement$AGE.UPPERS = c(Inf)
    rv$engagement$SEXES = c('male','female')
    rv$engagement$SUBGROUPS = dimnames(rv$engagement$subgroup)$subgroup
        
    rv$engagement.allPLHIV = read.cascade.data.type(sub.data.type = "ART",
                                                    denominator = "allPLHIV")
    rv$engagement.allPLHIV$AGES = c('15+')
    rv$engagement.allPLHIV$AGE.LOWERS = c(15)
    rv$engagement.allPLHIV$AGE.UPPERS = c(Inf)
    rv$engagement.allPLHIV$SEXES = c('male','female')
    rv$engagement.allPLHIV$SUBGROUPS = dimnames(rv$engagement.allPLHIV$subgroup)$subgroup
    
    ## Default suppression denominator = all aware PLHIV (option for all PLHIV below)
    rv$suppression = read.cascade.data.type(sub.data.type = "suppress",
                                            denominator = "aware")
    rv$suppression$total = (rv$suppression$total*rv$awareness$total)/100
    rv$suppression$subgroup = (rv$suppression$subgroup*rv$awareness$subgroup)/100
    rv$suppression$age.sex = (rv$suppression$age.sex*rv$awareness$age.sex)/100
    rv$suppression$age.sex.subgroup = (rv$suppression$age.sex.subgroup*rv$awareness$age.sex.subgroup)/100
    
    rv$suppression$AGES = c('15+')
    rv$suppression$AGE.LOWERS = c(15)
    rv$suppression$AGE.UPPERS = c(Inf)
    rv$suppression$SEXES = c('male','female')
    rv$suppression$SUBGROUPS = dimnames(rv$suppression$subgroup)$subgroup
    
    rv$suppression.allPLHIV = read.cascade.data.type(sub.data.type = "suppress",
                                                      denominator = "allPLHIV")
    rv$suppression.allPLHIV$AGES = c('15+')
    rv$suppression.allPLHIV$AGE.LOWERS = c(15)
    rv$suppression.allPLHIV$AGE.UPPERS = c(Inf)
    rv$suppression.allPLHIV$SEXES = c('male','female')
    rv$suppression.allPLHIV$SUBGROUPS = dimnames(rv$suppression.allPLHIV$subgroup)$subgroup
    
    # Population data aggregated into model age groups 
    rv$population = read.population.data.files.model.ages(data.type = "population", model.age.cutoffs = MODEL.AGE.CUTOFFS)
    rv$population$AGES = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34",
                           "35-39","40-44","45-49","50-54","55-59","60-64","65-69",
                           "70-74","75-79","80 and over")
    rv$population$AGE.LOWERS = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80)
    rv$population$AGE.UPPERS = c(5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,Inf)
    rv$population$SEXES = c('male','female')
    rv$population$SUBGROUPS = dimnames(rv$incidence$subgroup)$subgroup ## NO POPULATION SUBGROUPS FOR NOW
    
    # Full population data
    rv$population.full = read.population.data.files.all.ages(data.type = "population")
    rv$population.full$AGES = c('0-4', '5-9','10-14','15-19','20-24','25-29','30-34',
                                '35-39','40-44','45-49','50-54','55-59','60-64','65-69',
                                '70-74','75-79','80-84','85-89','90-94','95-99','100 and over')
    rv$population.full$AGE.LOWERS = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100)
    rv$population.full$AGE.UPPERS = c(5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100,Inf)
    rv$population.full$SEXES = c('male','female')
    rv$population.full$SUBGROUPS = dimnames(rv$incidence$subgroup)$subgroup ## NO POPULATION SUBGROUPS FOR NOW
    
    # Age-specific fertility rate 
    rv$fertility = read.fertility.data.files(data.type = "population")
    rv$fertility$YEARS = c("1953","1958","1963","1968","1973","1978","1983","1988","1993","1998","2003",
                           "2008","2013","2018","2023","2028","2033","2038","2043","2048","2053","2058",
                           "2063","2068","2073","2078","2083","2088","2093","2098")
    rv$fertility$AGES = c("15-19","20-24","25-29","30-34","35-39","40-44","45-49")
    rv$fertility$AGE.LOWERS = c(15,20,25,30,35,40,45)
    rv$fertility$AGE.UPPERS = c(20,25,30,35,40,45,50)
    
    # Crude birth rate
    rv$births = read.birth.data.files(data.type = "population")
    rv$births$YEARS = c("1950-1955","1955-1960","1960-1965","1965-1970","1970-1975","1975-1980","1980-1985","1985-1990",
                        "1990-1995","1995-2000","2000-2005","2005-2010","2010-2015","2015-2020","2020-2025","2025-2030",
                        "2030-2035","2035-2040","2040-2045","2045-2050","2050-2055","2055-2060","2060-2065","2065-2070",
                        "2070-2075","2075-2080","2080-2085","2085-2090","2090-2095","2095-2100")
    
    #Deaths
    rv$deaths = read.death.data.files(data.type = "population")
    rv$deaths$YEARS = c("1950 - 1955","1955 - 1960","1960 - 1965","1965 - 1970","1970 - 1975","1975 - 1980","1980 - 1985",
                        "1985 - 1990","1990 - 1995","1995 - 2000","2000 - 2005","2005 - 2010","2010 - 2015","2015 - 2020")
    rv$deaths$AGES = c('0-4', '5-9','10-14','15-19','20-24','25-29','30-34',
                       '35-39','40-44','45-49','50-54','55-59','60-64','65-69',
                       '70-74','75-79','80-84','85-89','90-94','95+')
    rv$deaths$AGE.LOWERS = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95)
    rv$deaths$AGE.UPPERS = c(5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,Inf)
    rv$deaths$SEXES = c('male','female')
    rv$deaths$SUBGROUPS = dimnames(rv$incidence$subgroup)$subgroup ## NO POPULATION SUBGROUPS FOR NOW
    
    rv
}


##----------------------------------##
##-- LOWER-LEVEL/HELPER FUNCTIONS --##
##----------------------------------##

# Called once for each data type within read.surveillance.data function; creates list with an array for 
# each data stratification (i.e., total, by age, by sex, by subgroup, by age and subgroup, etc.); calls 
# lower-level function, either read.surveillance.data.files (no stratification) or read.surveillance.data.stratified
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
    if(data.type!="hiv.mortality")
    {
        rv$sex = read.surveillance.data.stratified(data.type=data.type,
                                                   strata = 'sex',
                                                   regions = F) 
        
        rv$sex.subgroup = NULL
    }
    
    rv
}

# Called once for each dimension (age/sex) within read.surveillance.data.type; loops through lower-level 
# function for each stratum of that dimension (i.e., calls function once for every age group) 
#     Age: read.surveillance.data.files
#     Sex: combine.pdf.years
read.surveillance.data.stratified = function(data.type,
                                             strata,
                                             regions=T){
    ages=c('0-14','10-19','15-24','15-49','15+','50 and over','All ages')
    sexes = c("female","male")
    
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
    
    else if(strata=='sex')
    {
        female = combine.pdf.years(data.type=data.type,
                                   sex = "female")
        
        dim.names = c(dimnames(female), list(sex=sexes))
        
        rv = array(NA,
                   dim = sapply(dim.names, length),
                   dimnames = dim.names)
        
        for(i in 1:length(sexes)){
            x = combine.pdf.years(data.type=data.type,
                                  sex = sexes[i])
            
            rv[,i] = x }
    }
    
    else stop("only currently set up for age and sex strata")
    
    rv
    
    
} 

# Called once for each stratum of specified dimension within read.surveillance.data.stratified 
# Reads in csv files; formats data; returns an array of data with correct dimensions based on strata; 
# option to read in lower/upper bound files  
read.surveillance.data.files = function(dir = 'data/raw_data',
                                        data.type,
                                        regions = F,
                                        age,
                                        suffix = ""){
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
    
    total =  suppressWarnings(array(as.numeric(gsub(" ","",one.df.t[paste0(years, suffix),ncol(one.df.t)])),
                                    dim = sapply(dim.names.total, length), 
                                    dimnames = dim.names.total))
    
    
    ## Subgroups ##
    dim.names.subgroups = list(year=as.character(paste0(years, suffix)),
                               subgroup=subgroup.names
    )
    
    
    subgroups =  suppressWarnings(array(as.integer(sapply(one.df.t[paste0(years, suffix),1:(length(subgroup.names))], gsub, pattern = " ",replacement = "")),
                                        dim = sapply(dim.names.subgroups, length), 
                                        dimnames = dim.names.subgroups))
    
    
    
    if(regions==T)
        return(subgroups)
    
    else if(regions==F)
        return(total)
    
    
}

read.cascade.data.type = function(data.type="cascade",
                                  denominator,
                                  sub.data.type){
    rv=list()
    
    rv$total = read.cascade.data.files(data.type=data.type, 
                                       sub.data.type=sub.data.type,
                                       denominator=denominator,
                                       age='All ages',
                                       sex="All")
    
    
    rv$subgroup = read.cascade.data.files(data.type=data.type,
                                          sub.data.type=sub.data.type,
                                          denominator=denominator,
                                          age='All ages',
                                          sex="All",
                                          regions = T)
    
    ## Age.Sex ##
    rv$age.sex = read.cascade.data.stratified(data.type=data.type,
                                              sub.data.type=sub.data.type,
                                              denominator=denominator,
                                              regions = F)
    ## Age.Sex.Subgroup ##
    rv$age.sex.subgroup = read.cascade.data.stratified(data.type=data.type,
                                                       sub.data.type=sub.data.type,
                                                       denominator=denominator,
                                                       regions = T)
    
    rv
}

read.cascade.data.stratified = function(data.type,
                                        sub.data.type,
                                        denominator,
                                        regions=T){
    ## Pull AGE array by REGION
    if(regions)
    {
        age.sex.subgroup = read.cascade.data.files(data.type=data.type,
                                                   sub.data.type = sub.data.type,
                                                   denominator = denominator,
                                                   age="15+",
                                                   sex = "Female",
                                                   regions = T)
        
        dim.names = c(dimnames(age.sex.subgroup),list(age = "15+"),list(sex=c("male","female")))
        dim.names = dim.names[c(1,3,4,2)]
        
        rv = array(NA,
                   dim = sapply(dim.names, length),
                   dimnames = dim.names)
        
        
        
        rv[,,"female",] = age.sex.subgroup
        rv[,,"male",] = read.cascade.data.files(data.type=data.type,
                                                sub.data.type = sub.data.type,
                                                denominator = denominator,
                                                age="15+",
                                                sex = "Male",
                                                regions = T)
    }
    ## Pull TOTAL AGE array
    else 
    {
        age.sex = read.cascade.data.files(data.type=data.type,
                                          sub.data.type = sub.data.type,
                                          denominator = denominator,
                                          age="15+",
                                          sex = "Male",
                                          regions = F)
        
        dim.names = c(dimnames(age.sex),list(age = "15+"),list(sex=c("male","female")))
        
        rv = array(NA,
                   dim = sapply(dim.names, length),
                   dimnames = dim.names)
        
        rv[,,"male"] = age.sex
        rv[,,"female"] = read.cascade.data.files(data.type=data.type,
                                                 sub.data.type = sub.data.type,
                                                 denominator = denominator,
                                                 age="15+",
                                                 sex = "Female",
                                                 regions = F)  
    }
    
    rv
}

read.cascade.data.files = function(dir = 'data/raw_data',
                                   data.type,
                                   sub.data.type,
                                   denominator,
                                   regions = F,
                                   age,
                                   sex,
                                   suffix = ""){
    sub.dir = file.path(dir, paste0(data.type,"_",denominator))
    
    files = list.files(file.path(sub.dir))
    
    ## Total and subgroups
    age.to.match = gsub("\\+", "\\\\+", age)
    file = files[grepl(age.to.match,files) & grepl(sex,files)]
    
    if (length(file)!=1)
        stop("can only pull one file at a time")
    
    one.df = read.csv(file.path(sub.dir,file), row.names = 1)
    colnames(one.df) = substring(colnames(one.df),2)
    years = unique(substr(colnames(one.df),1,4))
    subgroup.names = rownames(one.df)[c(-1,-nrow(one.df))]
    
    sub.data.types = unique(trimws(one.df[1,]))
    one.df[1,] = rep(c(rep("status",3),rep("ART",3),rep("suppress",3)),length(years))
    one.df = one.df[,grepl(sub.data.type,one.df[1,])]
    one.df = one.df[-1,]
    if(sub.data.type!="status")
        colnames(one.df) = substring(colnames(one.df),1,nchar(colnames(one.df))-2)
    one.df.t = transpose(one.df)
    rownames(one.df.t) = colnames(one.df)
    colnames(one.df.t) = rownames(one.df)
    
    dim.names.total = list(year=as.character(paste0(years, suffix)))

    total =  suppressWarnings(array(as.numeric(gsub(" ","",
                                                    gsub(">","",one.df.t[paste0(years, suffix),ncol(one.df.t)]))),
                                    dim = sapply(dim.names.total, length), 
                                    dimnames = dim.names.total))
    
    
    ## Subgroups ##
    dim.names.subgroups = list(year=as.character(paste0(years, suffix)),
                               subgroup=subgroup.names
    )
    
    
    subgroups =  suppressWarnings(array(as.integer(sapply(one.df.t[paste0(years, suffix),1:(length(subgroup.names))], 
                                                          gsub, pattern = ">",replacement = "")),
                                        dim = sapply(dim.names.subgroups, length), 
                                        dimnames = dim.names.subgroups))
    
    
    
    if(regions==T)
        return(subgroups)
    
    else if(regions==F)
        return(total)
    
}


##-------------------------##
##-- SPECIALTY FUNCTIONS --##
##-------------------------##


# 1. Called for sex-specific incidence/prevalence data in read.surveillance.data.stratified
# 2. Combines multiple pdfs from different years; calls read.pdf.data.files for each year
combine.pdf.years = function(dir = 'data/raw_data/pdfs',
                             data.type,
                             pdf.years.to.combine = 2018:2019,
                             sex){
    
    # RIGHT NOW HARD-CODED FOR ONLY 2 YEARS OF PDFS
    year.1 = read.pdf.data.files(data.type=data.type,
                                 pdf.year=pdf.years.to.combine[1],
                                 sex=sex)
    
    year.2 = read.pdf.data.files(data.type=data.type,
                                 pdf.year=pdf.years.to.combine[2],
                                 sex=sex)
    
    
    other.years = dimnames(year.1)[[1]]
    most.recent.years = dimnames(year.2)[[1]]
    other.years.to.keep = other.years[!(other.years %in% most.recent.years)]
    all.years = c(other.years.to.keep, most.recent.years)
    dim.names = list(year = all.years)
    
    rv = array(c(year.1[other.years.to.keep], year.2),
               dim = sapply(dim.names, length),
               dimnames = dim.names)
    
    years.sorted = sort(all.years)
    rv = rv[years.sorted]
    
    rv
}


#### THIS IS 15+ DATA - NEED TO CHANGE 
# For each pdf year, read in csv file from tabula, return sex-specific incidence or prevalence 
read.pdf.data.files = function(dir = 'data/raw_data/pdfs',
                               data.type,
                               pdf.year,
                               sex){
    files = list.files(file.path(dir,pdf.year))
    file = files[grepl(data.type,files)]
    
    if (length(file)!=1)
        stop("can only pull one file at a time")
    
    df = read.csv(file.path(paste0(dir,"/",as.character(pdf.year)),file))
    df = df[-1,]
    var.names = df[,1]
    var.names = var.names[var.names!=""]
    
    if(data.type=="incidence"){
        var.names = var.names[-length(var.names)]
    }
    
    years = substr(names(df),2,5)
    years = years[-1]
    
    values = c(t(df[,-1]))
    values = values[(values!="" & !grepl("]", values))]
    
    dim.names = list(year = years,
                     var = var.names)
    
    rv = array(as.numeric(gsub(" ","",values)),
               dim = sapply(dim.names, length),
               dimnames = dim.names)        
    
    if (sex=="female"){
        rv = array(rv[,grepl("women",dimnames(rv)$var)],
                   dim = length(years),
                   dimnames = list(years = years))}
    
    if (sex=="male"){
        rv = array(rv[,(grepl("men",dimnames(rv)$var) & !grepl("women", dimnames(rv)$var))],
                   dim = length(years),
                   dimnames = list(years = years))}
    
    rv
}


# Reads in population data files and returns data in the AGE BRACKETS FOR MODEL; returns list with 
# an array for each stratification (total, age, sex, age*sex) 
read.population.data.files.model.ages = function(dir = 'data/raw_data',
                                                 data.type = "population",
                                                 model.age.cutoffs){
    POPULATION.AGE.MAPPING = POPULATION.AGE.MAPPING.HARD.CODE # can't use below function because it calls the datamanager
    
    # POPULATION.AGE.MAPPING = map.population.ages(data.manager = data.manager,
    #                                              data.type = "population",
    #                                              model.age.cutoffs = model.age.cutoffs)
    
    sub.dir = file.path(dir, data.type)
    
    files = list.files(file.path(sub.dir))
    
    pop.file = "PopulationByAgeSex"
    file = files[grepl(pop.file,files)]
    
    if (length(file)!=1)
        stop("can only pull one file at a time")
    
    df = read.csv(file.path(sub.dir,file))
    df = df[df$Location=="Kenya",]
    years = as.numeric(unique(df$Time))
    ages = unique(df$AgeGrp)
    ages = c(ages[-length(ages)],"100 and over")
    
    
    df$AgeGrp = factor(df$AgeGrp, levels = ages)
    df.sorted = df[order(df$AgeGrp),]
    
    ## Age array
    age.dim.names = list(year = as.character(years),
                         age = ages)
    
    age.full = array(0,
                     dim = sapply(age.dim.names, length), 
                     dimnames = age.dim.names)
    
    age.full[] = as.numeric(df.sorted[,"PopTotal"])*1000
    
    # MAP TO MODEL AGES
    age = sapply(1:length(POPULATION.AGE.MAPPING), function(a){
        sapply(1:length(years), function(y){
            age.to = names(POPULATION.AGE.MAPPING)[a] # names of mapping are the model ages - what I want to map TO
            ages.from = POPULATION.AGE.MAPPING[[a]] # list elements are the population ages - what I want to map FROM
            sum(age.full[y,ages.from])
        })
    })
    
    dimnames(age) = list(year = as.character(years),
                         age = names(POPULATION.AGE.MAPPING))
    
    
    ## Total array
    total = array(rowSums(age),
                  dimnames = list(year = as.character(years)))
    
    ## Age.Sex array
    sexes = c("male","female")
    age.sex.dim.names = list(year = as.character(years),
                             age = names(POPULATION.AGE.MAPPING),
                             sex = sexes)
    
    male.age.full = array(0,
                          dim = sapply(age.dim.names, length), 
                          dimnames = age.dim.names)
    
    male.age.full[] = as.numeric(df.sorted[,"PopMale"])*1000
    
    # MAP TO MODEL AGES
    male.age = sapply(1:length(POPULATION.AGE.MAPPING), function(a){
        sapply(1:length(years), function(y){
            age.to = names(POPULATION.AGE.MAPPING)[a] # names of mapping are the model ages - what I want to map TO
            ages.from = POPULATION.AGE.MAPPING[[a]] # list elements are the population ages - what I want to map FROM
            sum(male.age.full[y,ages.from])
        })
    })
    
    dimnames(male.age) = list(year = as.character(years),
                              age = names(POPULATION.AGE.MAPPING))
    
    female.age.full = array(0,
                            dim = sapply(age.dim.names, length), 
                            dimnames = age.dim.names)
    
    female.age.full[] = as.numeric(df.sorted[,"PopFemale"])*1000
    
    # MAP TO MODEL AGES
    female.age = sapply(1:length(POPULATION.AGE.MAPPING), function(a){
        sapply(1:length(years), function(y){
            age.to = names(POPULATION.AGE.MAPPING)[a] # names of mapping are the model ages - what I want to map TO
            ages.from = POPULATION.AGE.MAPPING[[a]] # list elements are the population ages - what I want to map FROM
            sum(female.age.full[y,ages.from])
        })
    })
    
    dimnames(female.age) = list(year = as.character(years),
                                age = names(POPULATION.AGE.MAPPING))
    
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

# Reads in population data files and returns data in the AGE BRACKETS AS REPORTED; returns list with 
# an array for each stratification (total, age, sex, age*sex) 
read.population.data.files.all.ages = function(dir = 'data/raw_data',
                                               data.type = "population"){
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
    ages = c(ages[-length(ages)],"100 and over")
    
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

# Reads in age-specific fertility rate
read.fertility.data.files = function(dir = 'data/raw_data',
                                     data.type = "population"){
    sub.dir = file.path(dir, data.type)
    
    files = list.files(file.path(sub.dir))
    
    pop.file = "Fertility"
    file = files[grepl(pop.file,files)]
    
    if (length(file)!=1)
        stop("can only pull one file at a time")
    
    df = read.csv(file.path(sub.dir,file))
    df = df[df$Location=="Kenya",]
    years = unique(df$MidPeriod)
    ages = unique(df$AgeGrp)
    
    # reverse dim order so that array fills correctly (will transpose later)
    dim.names = list(age = ages,
                     year = as.character(years))
    
    fertility.rate = array(df$ASFR/1000,
                           dim = sapply(dim.names, length),
                           dimnames = dim.names)
    
    fertility.rate = t(fertility.rate) # transpose so that year dimension is now first
    
    # combine 30-34 and 35-39 into 30-39; same for 40-49 - REMOVING THIS BECAUSE RETURNING TO JUST 5-YEAR AGE GROUPS
    # ages.new = c(ages[1:3], "30-39","40-49")
    # new.dim.names = list(year = as.character(years),
    #                      age = ages.new)
    # 
    # fertility.rate.new = array(c(fertility.rate[,c(1:3)],(fertility.rate[,4]+fertility.rate[,5])/2,(fertility.rate[,6]+fertility.rate[,7])/2),
    #                            dim = sapply(new.dim.names, length),
    #                            dimnames = new.dim.names)
    
    rv = list()
    rv$age = fertility.rate
    
    rv
}

# Reads in age/sex-specific death data 
read.death.data.files = function(dir = 'data/raw_data',
                                 data.type = "population"){
    sub.dir = file.path(dir, data.type)
    
    files = list.files(file.path(sub.dir))
    
    pop.file = "NumberDeaths"
    file = files[grepl(pop.file,files)]
    
    if (length(file)!=1)
        stop("can only pull one file at a time")
    
    df = read.csv(file.path(sub.dir,file))
    df = cbind(df[,c(-1,-2,-5)],df[,5])
    colnames(df) = df[1,]
    ages = as.character(df[1,3:(ncol(df)-1)])
    df = df[-5:-1,]
    
    years = unique(df$Time)
    
    ## Age array
    age.dim.names = list(year = as.character(years),
                         age = ages)
    
    age = array(0,
                dim = sapply(age.dim.names, length), 
                dimnames = age.dim.names)
    
    for (i in 3:(ncol(df)-1)){
        age[,(i-2)] = as.integer(gsub(" ","",df[df$Sex=="Both sexes combined",i]))*1000
    }
    
    ## Total array 
    total = array(as.integer(gsub(" ","",df[df$Sex=="Both sexes combined",ncol(df)]))*1000,
                  dimnames = list(year = as.character(years)))
    
    ## Age.sex array
    sexes = c("male","female")
    age.sex.dim.names = list(year = as.character(years),
                             age = ages,
                             sex = sexes)
    male.age = array(0,
                     dim = sapply(age.dim.names, length), 
                     dimnames = age.dim.names)
    
    for (i in 3:(ncol(df)-1)){
        male.age[,(i-2)] = as.integer(gsub(" ","",df[df$Sex=="Male",i]))*1000
    }
    
    female.age = array(0,
                       dim = sapply(age.dim.names, length), 
                       dimnames = age.dim.names)
    
    for (i in 3:(ncol(df)-1)){
        female.age[,(i-2)] = as.integer(gsub(" ","",df[df$Sex=="Female",i]))*1000
    }
    
    age.sex = array(0,
                    dim = sapply(age.sex.dim.names, length), 
                    dimnames = age.sex.dim.names)
    
    age.sex[,,"male"] = male.age
    age.sex[,,"female"] = female.age
    
    ## Sex array 
    male = array(as.integer(gsub(" ","",df[df$Sex=="Male",ncol(df)]))*1000,
                 dimnames = list(year = as.character(years)))
    
    female = array(as.integer(gsub(" ","",df[df$Sex=="Female",ncol(df)]))*1000,
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

# No longer using; read in total births to create a crude birth rate 
read.birth.data.files = function(dir = 'data/raw_data',
                                 data.type = "population"){
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
    
    births = array((as.numeric(df$CBR)/1000),
                   dimnames = list(year = as.character(years)))
    
    rv$total = births
    
    rv
}
