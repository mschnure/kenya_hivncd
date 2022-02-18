# PLOTTING FUNCTIONS ########################
library(ggplot2)        
library(reshape2)
source('model/extract_data.R')
#General structure: 
# ... can include more that one simulation 
# data.type: e.g., incidence, diagnosis,...
# facet.by: how to categorize data 
# split.by:
simplot = function(..., 
                   years, 
                   data.types, 
                   facet.by,
                   split.by){
        
}

#QQ: what about info on ages/sex/subgroup: are we collapsing all of those on top of each other?
simplot.basic = function(..., 
                         years = 2010:2020,
                         data.types=c('incidence','diagnoses') #QQ: is this what we mean by datatype?
)
{
        sims = list(...)
        
        #empty dataframe to combine different simulations
        df.sim = NULL
        
        for(d in data.types){
                for(i in 1:length(sims)){
                        #select a simulation, and add it to the df        
                        sim = sims[[i]]
                        
                        
                        #Parastu - can you explain this? 
                        # one.df=melt(sim[d])
                        # one.df = cbind(one.df,i)
                        # names(one.df)=c('year','age','sex','subgroup','value','data.type','sim.id')
                        
                        
                        # set up a dataframe for one simulation 4 columns: year, value, sim, data.type 
                        value = as.numeric(extract.data(sim, years = years, data.type=d))
                        
                        one.df = data.frame(year=years, value=value ,sim=i, data.type=d)
                        
                        
                        df.sim = rbind(df.sim, one.df)   
                }
        }
        
        df.truth = df.sim # later make it null 
        
        df.truth$sim = 'truth'
        df.truth$value = df.truth$value - 10
        
         # another for loop that populates similarly for surveillance data; only for data type loop not sim loop 
        
        # dim(df)
        unique(df$data.type)
        
        df.sim$sim = as.character(df.sim$sim)
        
        ggplot() + 
        geom_line(data = df.sim, aes(x = year, y = value, color = sim)) +
                geom_point(data = df.truth, aes(x = year, y = value, color = sim)) +
                 facet_wrap( ~ data.type, scales = "free_y") + #facet by data.type only
                ylim(0,NA)
       # facet_wrap(facets = vars(sex,age,data.type)) #facet by several factors
        
}




