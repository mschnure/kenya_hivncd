source('data_manager.R')
source('model/diffeq.R')
source('model/age_mappings.R') 
source('interventions/intervention_set_v2.R') # for draft v6; making all values 90/10 and 80/20

# load('cached/data.manager.Rdata') # without scaling calibration targets
load('cached/data.manager_2023-02-06.Rdata') # with scaled calibration targets
source('calibration/parameter_mappings/south_africa_age_mixing.R')
source('calibration/parameter_mappings/age_sex_mixing_proportions.R')
source('calibration/parameter_mappings/age_sex_transmission_multipliers.R')
source('calibration/parameter_mappings/testing_projection.R')
source('calibration/parameter_mappings/engagement_disengagement_data.R')
source('model/parameters.R')
source('model/plots.R')
source('calibration/likelihood/likelihood.R')
source('calibration/parameter_mappings/prior_distributions.R')