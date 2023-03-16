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
                   show.individual.sims=T,
                   proportion = F, # default denominator for engagement/suppression is awareness
                   ages = data.manager[[data.types[1]]]$AGES, #use what's in the data as the default - there is a problem here if you have multiple data types
                   sexes = data.manager[[data.types[1]]]$SEXES,
                   for.paper=F,
                   show.calibration.data=T
                   #subgroups = data.manager$SUBGROUPS
){
    sims = list(...)
    keep.dimensions = union('year',union(facet.by, split.by))

    if(any(data.types=="hiv.mortality") & any(keep.dimensions=="sex"))
        stop("no hiv mortality data by sex")
    
    
    ##----------------------##
    ##----- SIM OUTPUT -----##
    ##----------------------##
    
    df.sim = NULL
    for(d in data.types){
        
        for(i in 1:length(sims)){
            
            if(is(sims[[i]],"simset")) # if this is an MCMC results, i.e., a simset
                sims.for.i = sims[[i]]@simulations
            
            # for plotting single simulations
            else {
                sim = sims[[i]]
                sims.for.i = list(sim)}
            
            if(any(data.types=="hiv.mortality"))
                sexes = sims.for.i[[1]]$SEXES
            
            # SHOW LINES FOR ALL INDIVIDUAL SIMS
            if(show.individual.sims | length(sims.for.i)==1){
                
                for(j in 1:length(sims.for.i)){
                    
                    sim = sims.for.i[[j]]

                    # Extract the data from simulation
                    value = extract.data(sim, years = years, age=ages, sex = sexes, data.type=d, keep.dimensions = keep.dimensions)

                    if(proportion){
                        # Extract denominator from simulation (denom for eng/supp is aware; denom for aware/mort is prev)
                        if((d=="engagement") | (d=="suppression")){
                            denominator = extract.data(sim, 
                                                       years = years, 
                                                       age=ages, 
                                                       sex = sexes, 
                                                       data.type="awareness", 
                                                       keep.dimensions = keep.dimensions)
                        }
                        
                        else if((d=="awareness") | (d=="hiv.mortality")){
                            denominator = extract.data(sim, 
                                                       years = years, 
                                                       age=ages, 
                                                       sex = sexes, 
                                                       data.type="prevalence", 
                                                       keep.dimensions = keep.dimensions)
                        } else 
                            stop("invalid denominator")
                        
                        value = value/denominator
                    }
                    
                    # set up a dataframe with columns: year, value, sim id, data.type 
                    one.df = reshape2::melt(value) 
                    one.df$sim.id = i
                    one.df$sim.number = j
                    one.df$data.type = d
                    one.df$lower = as.numeric(NA)
                    one.df$upper = as.numeric(NA)
                    
                    df.sim = rbind(df.sim, one.df)   
                }
                
                # SHOW 95% CONFIDENCE INTERVAL FROM SIMS
            } else {
                
                value.1 = extract.data(sims.for.i[[1]], years = years, age=ages, sex = sexes, data.type=d, keep.dimensions = keep.dimensions)
                values = sapply(sims.for.i, extract.data, years = years, age=ages, sex = sexes, data.type=d, keep.dimensions = keep.dimensions)
                
                dim.names = c(dimnames(value.1),list(sim=1:length(sims.for.i)))
                dim(values) = sapply(dim.names, length)
                dimnames(values) = dim.names
        
                if(proportion){
                    # Extract denominator from simulation (denom for eng/supp is aware; denom for aware/mort is prev)
                    if((d=="engagement") | (d=="suppression")){
                        denominator.1 = extract.data(sims.for.i[[1]],
                                                     years = years,
                                                     age=ages,
                                                     sex = sexes,
                                                     data.type="awareness",
                                                     keep.dimensions = keep.dimensions)
                        denominator = sapply(sims.for.i, extract.data, 
                                             years = years, age=ages, sex = sexes, 
                                             data.type="awareness", keep.dimensions = keep.dimensions)
                    } else if((d=="awareness") | (d=="hiv.mortality")){
                        denominator.1 = extract.data(sims.for.i[[1]],
                                                     years = years,
                                                     age=ages,
                                                     sex = sexes,
                                                     data.type="prevalence",
                                                     keep.dimensions = keep.dimensions)
                        denominator = sapply(sims.for.i, extract.data, 
                                             years = years, age=ages, sex = sexes, 
                                             data.type="prevalence", keep.dimensions = keep.dimensions)
                    } else
                        stop("invalid denominator")
                    
                    dim.names.denominator = c(dimnames(denominator.1),list(sim=1:length(sims.for.i)))
                    dim(denominator) = sapply(dim.names.denominator, length)
                    dimnames(denominator) = dim.names.denominator
                    
                    values = values/denominator
                }
    
                value = apply(values,names(dim(value.1)),median, na.rm=T)
                lower = apply(values,names(dim(value.1)),quantile,probs=.025, na.rm=T)
                upper = apply(values,names(dim(value.1)),quantile,probs=.975, na.rm=T)
                
                dim.names.value = dimnames(value.1)
                dim(value) = dim(lower) = dim(upper) = sapply(dim.names.value, length)
                dimnames(value) = dimnames(lower) = dimnames(upper) = dim.names.value
                
                one.df = reshape2::melt(value) 
                one.df$sim.id = i
                one.df$sim.number = 1
                one.df$data.type = d
                one.df$lower = as.numeric(lower)
                one.df$upper = as.numeric(upper)
                
                df.sim = rbind(df.sim, one.df)  
                
            }
        }
    }
    
    df.sim$sim.id = as.character(df.sim$sim.id)
    df.sim$group.id = paste0("sim ",df.sim$sim.id,"_",df.sim$sim.number)
    
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
            
            
            df.truth$value = (df.truth$value/df.truth.denominator$value)
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
    
    if(proportion){
        
        if(for.paper==T){
            if(show.calibration.data==T){
                plot = ggplot() + 
                    geom_ribbon(data = df.sim, aes(x = year, ymin = lower, ymax = upper, fill = sim.id),alpha = 0.3) + 
                    geom_line(data = df.sim, aes(x = year, y = value, color = sim.id, group = group.id), show.legend = F) +
                    geom_point(data = df.truth, aes(x = year, y = value, color = sim.id, group = group.id, shape = split), show.legend = F) +
                    facet_wrap(facet_formula, scales = "free_y") + 
                    scale_y_continuous(labels = scales::percent,name = NULL, limits = c(0,NA)) + 
                    theme_bw() +
                    theme(legend.position = "bottom") + # move legend to the bottom
                    scale_fill_discrete(labels=c("1" = "No intervention","2" = "Combined interventions"), 
                                        name=NULL) + 
                    xlab("Year") # x axis label 
            } else {
                plot = ggplot() + 
                    geom_ribbon(data = df.sim, aes(x = year, ymin = lower, ymax = upper, fill = sim.id),alpha = 0.3) + 
                    geom_line(data = df.sim, aes(x = year, y = value, color = sim.id, group = group.id), show.legend = F) +
                    # geom_point(data = df.truth, aes(x = year, y = value, color = sim.id, group = group.id, shape = split), show.legend = F) +
                    facet_wrap(facet_formula, scales = "free_y") + 
                    scale_y_continuous(labels = scales::percent,name = NULL, limits = c(0,NA)) + 
                    theme(panel.background = element_blank(), legend.position = "bottom") + # move legend to the bottom
                    scale_fill_discrete(labels=c("1" = "No intervention","2" = "Combined interventions"), 
                                        name=NULL) + 
                    xlab("Year") # x axis label 
            }
        } else {
            plot = ggplot() + 
                geom_ribbon(data = df.sim, aes(x = year, ymin = lower, ymax = upper, fill = sim.id),alpha = 0.3) + 
                geom_line(data = df.sim, aes(x = year, y = value, color = sim.id, group = group.id)) +
                geom_point(data = df.truth, aes(x = year, y = value, color = sim.id, group = group.id, shape = split)) +
                facet_wrap(facet_formula, scales = "free_y") + 
                ylim(0,NA)
        }
    } else {
        if(for.paper==T){
            if(show.calibration.data==T){
                plot = ggplot() + 
                    geom_ribbon(data = df.sim, aes(x = year, ymin = lower, ymax = upper, fill = sim.id),alpha = 0.3) + 
                    geom_line(data = df.sim, aes(x = year, y = value, color = sim.id, group = group.id), show.legend = F) +
                    geom_point(data = df.truth, aes(x = year, y = value, color = sim.id, group = group.id, shape = split), show.legend = F) +
                    facet_wrap(facet_formula, scales = "free_y") + 
                    scale_y_continuous(labels = function(x){format(x,big.mark=",")},name = NULL, limits = c(0,NA)) + 
                    theme_bw() +
                    theme(legend.position = "bottom") + # move legend to the bottom
                    scale_fill_discrete(labels=c("1" = "No intervention","2" = "Combined interventions"), 
                                        name=NULL) + 
                    xlab("Year") # x axis label 
            } else {
                plot = ggplot() + 
                    geom_ribbon(data = df.sim, aes(x = year, ymin = lower, ymax = upper, fill = sim.id),alpha = 0.3) + 
                    geom_line(data = df.sim, aes(x = year, y = value, color = sim.id, group = group.id), show.legend = F) +
                    # geom_point(data = df.truth, aes(x = year, y = value, color = sim.id, group = group.id, shape = split), show.legend = F) +
                    facet_wrap(facet_formula, scales = "free_y") + 
                    scale_y_continuous(labels = function(x){format(x,big.mark=",")},name = NULL, limits = c(0,NA)) + 
                    theme(panel.background = element_blank(), legend.position = "bottom") + # move legend to the bottom
                    scale_fill_discrete(labels=c("1" = "No intervention","2" = "Combined interventions"), 
                                        name=NULL) + 
                    xlab("Year") # x axis label 
            }
        } else {
            plot = ggplot() + 
                geom_ribbon(data = df.sim, aes(x = year, ymin = lower, ymax = upper, fill = sim.id),alpha = 0.3) + 
                geom_line(data = df.sim, aes(x = year, y = value, color = sim.id, group = group.id)) +
                geom_point(data = df.truth, aes(x = year, y = value, color = sim.id, group = group.id, shape = split)) +
                facet_wrap(facet_formula, scales = "free_y") + 
                ylim(0,NA)
        }
    }
    
    

    
    suppressWarnings(print(plot))
    
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




