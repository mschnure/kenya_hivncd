################################################################################################
# Description: Plotting functions that pull data from both the simulation and surveillance data 
################################################################################################

# Functions 
#     1. simplot
#         Generate plot comparing simulation results to surveillance data; allows the user to specify which 
#         dimensions they’d like to look at (i.e., only look at data among women) and/or which dimensions they’d 
#         like to stratify by (i.e., separate panels or shapes by age group)
#     2. simplot.basic
#         Generate plot comparing simulation results to surveillance data; only allowed to facet by data type 



library(ggplot2)        
library(reshape2)
source('model/extract_data.R')


simplot = function(...,
                   data.manager = DATA.MANAGER,
                   years = 2010:2020,
                   data.types = c('incidence','prevalence'),
                   facet.by = NULL,
                   split.by = NULL,
                   proportion = F, # default denominator for engagement/suppression is awareness
                   ages = data.manager[[data.types[1]]]$AGES, #use what's in the data as the default - there is a problem here if you have multiple data types
                   sexes = data.manager[[data.types[1]]]$SEXES
                   #subgroups = data.manager$SUBGROUPS
){
    sims = list(...)
    keep.dimensions = union('year',union(facet.by, split.by))
    
    if(any(data.types=="hiv.mortality"))
        sexes = sim$SEXES
    
    if(any(data.types=="hiv.mortality") & any(keep.dimensions=="sex"))
        stop("no hiv mortality data by sex")
    
    
    ##----------------------##
    ##----- SIM OUTPUT -----##
    ##----------------------##
    
    df.sim = NULL
    for(d in data.types){
        for(i in 1:length(sims)){
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
    if(proportion)
    {
        df.sim.denominator = NULL
        for(d in data.types){
            for(i in 1:length(sims)){
                sim = sims[[i]]
                # Extract denominator from simulation (denom for eng/supp is aware; denom for aware/mort is prev)
                if(any(data.types==c("engagement","suppression"))){
                    denominator = extract.data(sim, 
                                               years = years, 
                                               age=ages, 
                                               sex = sexes, 
                                               data.type="awareness", 
                                               keep.dimensions = keep.dimensions)
                }

                if(any(data.types==c("awareness","hiv.mortality"))){
                    denominator = extract.data(sim, 
                                               years = years, 
                                               age=ages, 
                                               sex = sexes, 
                                               data.type="prevalence", 
                                               keep.dimensions = keep.dimensions)
                }
                
                # set up a dataframe with columns: year, value, sim id, data.type 
                one.df = reshape2::melt(denominator) 
                one.df$sim.id = i
                one.df$data.type = d
                
                df.sim.denominator = rbind(df.sim.denominator, one.df)   
            }
        }
        
        
        df.sim$value = (df.sim$value/df.sim.denominator$value)*100
        
    }
    
    df.sim$sim.id = as.character(df.sim$sim.id)
    df.sim$group.id = paste0("sim ",df.sim$sim.id)
    
    for(s in split.by){
        df.sim$group.id = paste0(df.sim$group.id,", ",s,"=",df.sim[,s])
    }
    
    ##--------------------------------##
    ##----- OBSERVED (TRUE) DATA -----##
    ##--------------------------------##
    
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
    
    # Other proportions (awareness, engagement, suppression) are reported as proportions, so no need to divide for df.truth
    # HIV mortality is reported as absolute number of AIDS-related deaths, so need denominator for df.truth
    if(proportion & any(data.types=="hiv.mortality"))
    {
        df.truth.denominator = NULL
        
        for(d in data.types){
            # Extract denominator from data
            denominator = get.surveillance.data(data.manager = data.manager, 
                                                years = years, 
                                                age = ages, 
                                                sex = sexes, 
                                                data.type="prevalence", 
                                                keep.dimensions = keep.dimensions)
            
            # set up a dataframe with columns: year, value, sim id, data.type 
            one.df = reshape2::melt(denominator) 
            one.df$sim.id = "truth"
            one.df$data.type = d
            
            df.truth.denominator = rbind(df.truth.denominator, one.df)   
            
            
            df.truth$value = (df.truth$value/df.truth.denominator$value)*100
        }
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

simplot.basic = function(..., 
                         data.manager = DATA.MANAGER,
                         years = 2010:2020,
                         data.types=c('incidence','prevalence')){
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




