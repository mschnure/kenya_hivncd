source('data_manager.R')
source('model/diffeq.R')
source('model/age_mappings.R') 

DATA.MANAGER = read.surveillance.data() 

save(DATA.MANAGER,file="cached/data.manager.Rdata")
