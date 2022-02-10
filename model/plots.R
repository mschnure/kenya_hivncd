simplot = function(..., 
                   years, 
                   data.types, 
                   facet.by,
                   split.by){
        
}

simplot.basic = function(...,
                         years,
                         data.types)
{
        sims = list(...)
        
        df = NULL
        
        for(data.type in data.types){
                for(i in 1:length(sims)){
                        
                 sim = sims[[i]]
                 
                 one.df =        
               
                         # set up a dataframe for one simulation 4 columns: year, value, sim, data.type 
                         
                        df = rbind(df, one.df)   

                        
                }
        }
        
        
        ggplot(df, aes(x = year, y = value, color = sim)) + geom_line() + facet_wrap( ~ data.type)
        
        
}