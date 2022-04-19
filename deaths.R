read.birth.data.files = function(dir = 'data/raw_data',
                                 data.type = "population")
{
        sub.dir = file.path(dir, data.type)
        
        files = list.files(file.path(sub.dir))
        
        pop.file = "Period_Indicators"
        file = files[grepl(pop.file,files)]
        
        if (length(file)!=1)
                stop("can only pull one file at a time")
        
        one.df = read.csv(file.path(sub.dir,file))
        one.df = one.df[one.df$Location=="Kenya",]
        years = unique(one.df$Time)
        
        rv = list()
        
        births = array(as.numeric(one.df$CBR),
                       dimnames = list(year = as.character(years)))
        
        deaths = array(as.numeric(one.df$CDR),
                       dimnames = list(year = as.character(years)))               
        
        rv$births = births
        rv$deaths = deaths
        
        rv
}

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
                rv$total[,(i-2)] = as.integer(gsub(" ","",df[df$Sex=="Both sexes combined",i]))
        }

        rv$male = array(0,
                         dim = sapply(dim.names, length), 
                         dimnames = dim.names)
        
        for (i in 3:ncol(df)){
                rv$male[,(i-2)] = as.integer(gsub(" ","",df[df$Sex=="Male",i]))
        }
        
        rv$female = array(0,
                         dim = sapply(dim.names, length), 
                         dimnames = dim.names)
        
        for (i in 3:ncol(df)){
                rv$female[,(i-2)] = as.integer(gsub(" ","",df[df$Sex=="Female",i]))
        }
        
        
        
        # Eventually, divide one period of deaths (e.g., deaths for 1950-1955) by the sum of the population from 1950-1954
        # Could alternatively divide one period of deaths by 5 and then get individual-year death rates, but those aren't exact and probably not necessary
        
}


## GETTING DEATH RATE - NEED TO ADD INTO DATA MANAGER 

deaths.years = dimnames(DATA.MANAGER$deaths$total)[[1]]

ages = dimnames(DATA.MANAGER$deaths$total)[[2]]
start.years = as.numeric(substr(deaths.years,1,4))
end.years = as.numeric(substr(deaths.years,8,11))

dr.dim.names = list(year = deaths.years,
                    age = ages)

pop = DATA.MANAGER$population$Total
# combine 95-99 and 100+ into 95+
pop2 = cbind(pop[,1:(ncol(pop)-3)],cbind(rowSums(pop[,(ncol(pop)-2):(ncol(pop)-1)]),pop[,ncol(pop)]))
colnames(pop2) = ages

five.year.age.groups = array(0,
                             dim = sapply(dr.dim.names, length),
                             dimnames = dr.dim.names)


for (i in 1:length(start.years)){
        five.year.age.groups[i,] = colSums(pop2[(i*5-4):(i*5),])
        
}

DR = 1000*DATA.MANAGER$deaths$total/five.year.age.groups

# end.year = as.numeric(substr(years[length(years)],8,11))
# years.to.fill = as.character(start.year:end.year)

