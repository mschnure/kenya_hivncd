################################################################################################
################
#Description: These functions are used to extract survilance data used to calibrate the model
################
################################################################################################


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

# sub function for overwrite 2D, 3D, 4D


read.surveillance.data = function(dir = 'data/raw_data' #where the tables are
                                  )
{
        ## Prevalence
        sub.dir = file.path(dir,'plhiv')
        
        files = list.files(file.path(sub.dir))

        df.prevalence = NULL
        
        for(i in 1:length(files)){
                one.df = read.csv(file.path(sub.dir,files[i]))
                df.prevalence = rbind(df.prevalence, one.df)   
        }
        
        
        ## Diagnoses
        sub.dir = file.path(dir,'new')
        
        files = list.files(file.path(sub.dir))
        
        df.new = NULL
        
        for(i in 1:length(files)){
                one.df = read.csv(file.path(sub.dir,files[i]))
                df.new = rbind(df.new, one.df)   
        }
        
        
        
        ## Below things are empty - starting to set up 
        
        diagnoses = list()
        
        diagnoses.total = array()
        diagnoses$TOTAL = diagnoses.total
        
        diagnoses.subgroup = array()
        diagnoses$SUBGROUP = diagnoses.subgroup
        
        diagnoses.subgroup.age = array()
        diagnoses$SUBGROUP.AGE = diagnoses.subgroup.age
        
        diagnoses.subgroup.sex = array()
        diagnoses$SUBGROUP.SEX = diagnoses.subgroup.sex
        
        diagnoses.subgroup.age.sex = array()
        diagnoses$SUBGROUP.AGE.SEX = diagnoses.subgroup.age.sex
        
        # sub list for every data type, each element of diagnoses sublist has an array for what we have 
        # (total/year, year/subgroup; year/subgroup/sex; year/subgroup/age; year/subgroup/age/sex)
        
        # prevalence, suppressed, on ART, etc. 
        # can start with diagnoses and prevalence 
        
        
}
        

# first, function to read in data; store it (an array for every combination of data we have)
# takes a directory, reads every csv file from that directory into arrays 
        
        
        
 
                
                