source('data_manager.R')
source('model/diffeq.R')
source('model/age_mappings.R') 
source('data/scaling_prevalence.R')

scale.data = T

DATA.MANAGER = read.surveillance.data() 

if(scale.data){
    DATA.MANAGER$prevalence = scale.calibration.data(data.type = "prevalence")
    DATA.MANAGER$prevalence.lowers = scale.calibration.data(data.type = "prevalence.lowers")
    DATA.MANAGER$prevalence.uppers = scale.calibration.data(data.type = "prevalence.uppers")
    
    DATA.MANAGER$incidence = scale.calibration.data(data.type = "incidence")
    DATA.MANAGER$incidence.lowers = scale.calibration.data(data.type = "incidence.lowers")
    DATA.MANAGER$incidence.uppers = scale.calibration.data(data.type = "incidence.uppers")
    
    DATA.MANAGER$hiv.mortality = scale.calibration.data(data.type = "hiv.mortality")
    DATA.MANAGER$hiv.mortality.lowers = scale.calibration.data(data.type = "hiv.mortality.lowers")
    DATA.MANAGER$hiv.mortality.uppers = scale.calibration.data(data.type = "hiv.mortality.uppers")  
}

save(DATA.MANAGER,file=paste0("cached/data.manager_",Sys.Date(),".Rdata"))

