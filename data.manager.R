################################################################################################
################
#Description: These functions are used to extract surveillance data used to calibrate the model
################
################################################################################################
library(data.table)


## DIDN'T WORK ON THIS FUNCTION
get.surveillance.data = function(data.manager,
                                 subgroups,
                                 data.type,
                                 years,
                                 ages,
                                 sexes,
                                 keep.dimensions,
                                 error.statement)
{
       dim.names = list(year=as.character(years),
                        age=ages,
                        sex=sexes,
                        subgroup=subgroups
                        )
       dim.names = dim.names[keep.dimensions]
       
        rv = array(100, 
                   dim = sapply(dim.names, length), 
                   dimnames = dim.names)
        
        # put a skeleton here with NAs with the dimensions we want, pull into a separate array, then overwrite 
        # can set up for 2-4 dimensions
        # needs to return an array indexed like simulation data 
        
        # error statement can either throw an error or fill in missing data with NAs 
        
        # keep dimensions --> which array do we need to go get
        
        # subset array 
        
        rv
        
}




read.surveillance.data.type = function(data.type){
        rv=list()
        
       
        if(data.type=='new')
                
        rv$total = read.surveillance.data.files(data.type='new',
                                                 age='All ages')
        

        rv$subgroups = read.surveillance.data.files(data.type='new',
                                                    age='All ages',
                                                    regions = T)
        
        ## Ages ##
        # Need to figure this out in the stratified function below 
        # rv$ages = read.surveillance.data.stratified(data.type='new',
        #                                             age=ages)
        
        rv
}
        

read.surveillance.data.stratified = function(strata){
        
        ages=c('0-14','10-19','15-24','15-49','15+','50 and over','All ages')
        
        
        age1 = read.surveillance.data.files(data.type='new',
                                            age=ages[1],
                                            regions = T)
        
        age2 = read.surveillance.data.files(data.type='new',
                                            age=ages[2],
                                            regions = T)
        
        
        ## Need to figure out how to combine arrays?

        
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
                                   subgroups=subgroup.names
        )
        
        
        subgroups =  array(as.integer(sapply(one.df.t[years,1:(length(subgroup.names))], gsub, pattern = " ",replacement = "")),
                           dim = sapply(dim.names.subgroups, length), 
                           dimnames = dim.names.subgroups)
        
        
        if(regions==T)
                return(subgroups)
        
        else if(regions==F)
                return(total)
                
                
}
