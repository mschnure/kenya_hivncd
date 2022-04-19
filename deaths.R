## GETTING DEATH RATE - NEED TO ADD INTO PARAMETERS 

deaths.years = dimnames(DATA.MANAGER$deaths$total)[[1]] #use get.surveillance.manager function for this rather than calling DATA.MANAGER directly

ages = dimnames(DATA.MANAGER$deaths$total)[[2]]
start.years = as.numeric(substr(deaths.years,1,4))
end.years = as.numeric(substr(deaths.years,8,11))

dr.dim.names = list(year = deaths.years,
                    age = ages)

pop = DATA.MANAGER$population$age
# combine 95-99 and 100+ into 95+
pop2 = cbind(pop[,1:(ncol(pop)-2)],cbind(rowSums(pop[,(ncol(pop)-1):(ncol(pop))]),rowSums(pop)))
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

