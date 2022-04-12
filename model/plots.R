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


simplot = function(...,
                   data.manager = DATA.MANAGER,
                   years = 2010:2020,
                   data.types = c('incidence','prevalence'),
                   facet.by = NULL,
                   split.by = NULL,
                   ages = data.manager$AGES, #use what's in the data as the default
                   sexes = data.manager$SEXES
                   #subgroups = data.manager$SUBGROUPS
                   ){
        
        
        # data frame will need columns from basic function; plus column for every facet.by and every split.by
        # then combine all the split.by and sim id's into one column 
        
        sims = list(...)
        
        keep.dimensions = union('year',union(facet.by, split.by))
        
        #empty dataframe to combine different simulations
        df.sim = NULL
        
        for(d in data.types){
                for(i in 1:length(sims)){
                        #select a simulation, and add it to the df        
                        sim = sims[[i]]
                        
                        # Extract the data from simulation
                        value = extract.data(sim, years = years, age=ages, sex = sexes, data.type=d, keep.dimensions = keep.dimensions)
                        
                        # set up a dataframe with columns: year, value, sim id, data.type 
                        one.df = reshape2::melt(value) 
                        one.df$sim.id = i
                        one.df$data.type = d
                        
                        df.sim = rbind(df.sim, one.df)   
                }
        }
        df.sim$sim.id = as.character(df.sim$sim.id)
        df.sim$group.id = paste0("sim ",df.sim$sim.id)
        
        for(s in split.by){
                df.sim$group.id = paste0(df.sim$group.id,", ",s,"=",df.sim[,s])
        }
        
        # Observed (true) data:
        df.truth = NULL  
        
        for(d in data.types){
                
                # Extract the data from simulation
                value = get.surveillance.data(data.manager = data.manager, 
                                              years = years, 
                                              age = ages, 
                                              sex = sexes, 
                                              data.type=d, 
                                              keep.dimensions = keep.dimensions)
                
                # set up a dataframe with 4 columns: year, value, sim id, data.type 
                one.df = reshape2::melt(value) 
                one.df$sim.id = "truth"
                one.df$data.type = d
                
                df.truth = rbind(df.truth, one.df)   
                
        }
        
        df.truth$group.id = "truth"
        
        df.truth$split = "all"
        
        for(s in split.by){
                df.truth$group.id = paste0(df.truth$group.id,", ",s,"=",df.truth[,s])
                if(s==split.by[1])
                        df.truth$split = paste0(s,"=",df.truth[,s])
                else
                        df.truth$split = paste0(df.truth$split,", ",s,"=",df.truth[,s])
        }
        

        
        # setting up facet.by
        facet_string = '~data.type'
        if(length(facet.by)>0)
                facet_string = paste0(facet_string, '+', paste0(facet.by,collapse = '+'))
        facet_formula = as.formula(facet_string)
       
        
        ggplot() + 
                geom_line(data = df.sim, aes(x = year, y = value, color = sim.id, group = group.id)) +
                geom_point(data = df.truth, aes(x = year, y = value, color = sim.id, group = group.id, shape = split)) +
                facet_wrap(facet_formula, scales = "free_y") + 
                ylim(0,NA)


}

#Basic plotting function regardeless of subgroups
simplot.basic = function(..., 
                         data.manager = DATA.MANAGER,
                         years = 2010:2020,
                         data.types=c('incidence','prevalence') 
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
        df.truth = NULL  
        
        for(d in data.types){
                
                # Extract the data from simulation
                value = as.numeric(get.surveillance.data(data.manager = data.manager, years = years, data.type=d))
                
                # set up a dataframe with 4 columns: year, value, sim id, data.type 
                one.df = data.frame(year=years, value=value, sim.id='truth', data.type=d)
                
                df.truth = rbind(df.truth, one.df)   

        }

        
        df.sim
        #plotting function
        ggplot() + 
        geom_line(data = df.sim, aes(x = year, y = value, color = sim.id)) +
                geom_point(data = df.truth, aes(x = year, y = value, color = sim.id)) +
                facet_wrap( ~ data.type, scales = "free_y") + #facet by data.type only
                ylim(0,NA)
       # facet_wrap(facets = vars(sex,age,data.type)) #facet by several factors
        
}




