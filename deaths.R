## GETTING DEATH RATE - NEED TO ADD INTO PARAMETERS 

deaths.years = dimnames(DATA.MANAGER$deaths$total)[[1]] #use get.surveillance.manager function for this rather than calling DATA.MANAGER directly

ages = dimnames(DATA.MANAGER$deaths$age)[[2]]
start.years = as.numeric(substr(deaths.years,1,4))
end.years = as.numeric(substr(deaths.years,8,11))

dr.age.dim.names = list(year = deaths.years,
                        age = ages)

dr.dim.names = list(year = deaths.years)

pop = DATA.MANAGER$population$age
# combine 95-99 and 100+ into 95+
pop.age = cbind(pop[,1:(ncol(pop)-2)],cbind(rowSums(pop[,(ncol(pop)-1):(ncol(pop))])))
colnames(pop.age) = ages

pop.total = DATA.MANAGER$population$total

five.year.age.groups.age = array(0,
                                 dim = sapply(dr.age.dim.names, length),
                                 dimnames = dr.age.dim.names)

for (i in 1:length(start.years)){
        five.year.age.groups.age[i,] = colSums(pop.age[(i*5-4):(i*5),])
        
}

five.year.age.groups.total = array(0,
                                 dim = sapply(dr.dim.names, length),
                                 dimnames = dr.dim.names)

for (i in 1:length(start.years)){
        five.year.age.groups.total[i] = sum(pop.total[(i*5-4):(i*5)])
        
}

DR.AGE = 1000*DATA.MANAGER$deaths$age/five.year.age.groups.age
DR.TOTAL = 1000*DATA.MANAGER$deaths$total/five.year.age.groups.total

mid.years = (start.years + (end.years-1))/2

dimnames(DR.TOTAL) = list(year=as.character(mid.years))
dimnames(DR.AGE) = list(year=as.character(mid.years),
                         age = ages)
