################################################################################################
################
#Description: These functions are used to extract surveillance data used to calibrate the model
################
################################################################################################
library(data.table)


## DIDN'T WORK ON THIS FUNCTION
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
       pull.ages = any(keep.dimensions=='age') || !setequal(ages, data.manager$AGES) #repeat for sex and subgroups
        
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
        
        data = data.manager[[data.type]][[data.element]]
        
        # fix data element from below
        #pull.dimensions = year --> total
        #pull.dimensions = year, age --> age
        #pull.dimensions = year, subgroup --> subgroup
        #pull.dimensions = year, age, subgroup --> age.subgroup
        #pull.dimensions = year, age, sex, subgroup --> age.sex.subgroup (right now will be an error)
        
        #map the keep dimensions to what we want; remember pull.dimensions might not be passed in the right order
        
        #distinguish between keep dimensions and get dimensions 
        
        if(!is.null(data)){

                years.to.get = intersect(as.character(years), dimnames(data)$year)
                
                if(length(keep.dimensions==1)){
                        
                        rv[years.to.get] = data[years.to.get]
                        
                }
                
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
                
                rv = apply(rv, keep.dimensions, sum) # won't work for percentages - if we use in the future
                
                dim.names = dim.names[keep.dimensions]
                dim(rv) = sapply(dim.names, length)
                dimnames(rv) = dim.names
                
                
                # add in a check for numbers versus proportions 
        }
        rv
}

read.surveillance.data = function(dir){
        rv = list(date.created = Sys.Date(),
                  AGES=c('0-14','10-19','15-24','15-49','15+','50 and over')
                  )
        
        rv$new = read.surveillance.data.type(data.type = 'new')
        
        rv$prevalence = read.surveillance.data.type(data.type = 'prevalence')
        
        rv$SUBGROUPS = dimnames(rv$new$subgroups)$subgroup
        
        rv
}


read.surveillance.data.type = function(data.type){
        rv=list()
        
       
        if(data.type=='new')
                
        rv$total = read.surveillance.data.files(data.type='new',
                                                 age='All ages')
        

        rv$subgroup = read.surveillance.data.files(data.type='new',
                                                    age='All ages',
                                                    regions = T)
        
        ## Ages ##
        # fill in with total 
        rv$age
        
        rv$age.subgroup = read.surveillance.data.stratified(data.type='new',
                                                    age=ages)
        
        rv
}
        

read.surveillance.data.stratified = function(strata){ #add regions argument
        
        ages=c('0-14','10-19','15-24','15-49','15+','50 and over','All ages')
        
        #fill in to do subgroups and total
        if(regions)
        age1 = read.surveillance.data.files(data.type='new',
                                            age=ages[1],
                                            regions = T)
        
        dim.names = c(dimnames(age1), list(age=ages))
        dim.names = dim.names[c(1,3,2)]
        
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

read.surveillance.data.files = function(dir = 'data/raw_data',
                                        data.type,
                                        regions = F,
                                        age
)
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
        
        
        if(regions==T)
                return(subgroups)
        
        else if(regions==F)
                return(total)
                
                
}
