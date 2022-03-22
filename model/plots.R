################################################################################################
################
#Description: Plotting functions
################
################################################################################################


library(ggplot2)        
library(reshape2)
source('model/extract_data.R')

#General structure: 
# ... can include more that one simulation  
# data.type: e.g., incidence, diagnosis,...
# facet.by: how to categorize data 
# split.by: ??
# simplot = function(..., 
#                    years, 
#                    data.types, 
#                    facet.by,
#                    split.by){
#         
# }

#Basic plotting function regardeless of subgroups
simplot.basic = function(..., 
                         years = 2010:2020,
                         data.types=c('incidence','diagnoses') 
)
{
        sims = list(...)
        
        #empty dataframe to combine different simulations
        df.sim = NULL
        
        for(d in data.types){
                for(i in 1:length(sims)){
                        #select a simulation, and add it to the df        
                        sim = sims[[i]]
                        
                        # Extract the data from simulation
                        value = as.numeric(extract.data(sim, years = years, data.type=d))
                        
                        # set up a dataframe with 4 columns: year, value, sim id, data.type 
                        one.df = data.frame(year=years, value=value, sim.id=i, data.type=d)
                        
                        df.sim = rbind(df.sim, one.df)   
                }
        }
        df.sim$sim.id = as.character(df.sim$sim.id)
        
        # Observed (true) data:
        df.truth = NULL # later make it null 
        
        for(d in data.types){
                
                # Extract the data from simulation
                value = as.numeric(get.surveillance.data(data.manager = data.manager, years = years, data.type=d))
                
                # set up a dataframe with 4 columns: year, value, sim id, data.type 
                one.df = data.frame(year=years, value=value, sim.id='truth', data.type=d)
                
                df.truth = rbind(df.truth, one.df)   

        }

        
         # we will add another for loop here that populates similarly for surveillance data; only for data type loop not sim loop 
        
        
        df.sim
        #plotting function
        ggplot() + 
        geom_line(data = df.sim, aes(x = year, y = value, color = sim.id)) +
                geom_point(data = df.truth, aes(x = year, y = value, color = sim.id)) +
                facet_wrap( ~ data.type, scales = "free_y") + #facet by data.type only
                ylim(0,NA)
       # facet_wrap(facets = vars(sex,age,data.type)) #facet by several factors
        
}




