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
                         years,
                         data.types=c('incidence','diagnoses') #QQ: is this what we mean by datatype?
)
{
        sims = list(...)
        
        #empty dataframe to combine different simulations
        df = NULL
        
        for(d in data.types){
                for(i in 1:length(sims)){
                        #select a simulation, and add it to the df        
                        sim = sims[[i]]
                        
                        
                        #Parastu - can you explain this? 
                        one.df=melt(sim[d])
                        one.df = cbind(one.df,i)
                        names(one.df)=c('year','age','sex','subgroup','value','data.type','sim.id')
                        
                        
                        # set up a dataframe for one simulation 4 columns: year, value, sim, data.type 
                        if (d=='incidence')
                                value = as.numeric(extract.incidence(sim, years=years))
                        if (d=='diagnoses')
                                value = as.numeric(extract.new.diagnoses(sim, years=years))
                        
                        one.df = data.frame(year=years, value=value ,sim=i, data.type=d)
                        
                        
                        df = rbind(df, one.df)   
                }
        }
        # dim(df)
        unique(df$data.type)
        
        ggplot(df, aes(x = year, y = value, color =sim.id)) +
                geom_line() +
                # facet_wrap( ~ data.type) #facet by data.type only
        facet_wrap(facets = vars(sex,age,data.type)) #facet by several factors
        
}



