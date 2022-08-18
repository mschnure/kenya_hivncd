source('data_manager.R')
source('model/diffeq.R')
source('model/age_mappings.R') 

DATA.MANAGER = read.surveillance.data()
source('south_africa_age_mixing.R')
source('model/age_sex_mixing_proportions.R')
source('model/parameters.R')
source('model/plots.R')