
# outcome: proportion over 50, all/max intervention 
# need: 
# parameters (one value per simulation)
# outcome (one value per simulation)
library(epiR)

outcome = sapply(simset.all.max@simulations, function(sim){
    calculate.percent.over.age.for.sim(sim, 
                                       age.point = 50,
                                       data.type = "prevalence",
                                       years = 2040,
                                       sexes = c("male","female"))
})
 

calculate.prcc = function(covariates, # assuming covariates is a named matrix with 1 column for each covariate
                          covariate.names = dimnames(covariates)[[2]],
                          outcome,
                          return.ci=F){
    
    df = data.frame(covariates=covariates[,covariate.names,drop=F], # apply rank function for every column in covariates
                    outcome=outcome) 
    
    dimnames(df)[[2]]=c(covariate.names, "outcome")
    
  #  tryCatch({    # if this code triggers an error, it will evaluate to NA 
        prccs = epi.prcc(df)
        
        if(return.ci){
            rv = data.frame(prccs$est,prccs$lower,prccs$upper)
            dimnames(rv) = list(parameter = prccs$var,
                                stat = c("est","lower","upper"))
        } else{
            rv = prccs$est
            names(rv) = prccs$var
        }

        
        rv
 #   }, error=function(e){NA})
    
    
    
}


prccs = calculate.prcc(covariates = simset.all.max@parameters,
                       outcome=outcome,
                       return.ci = T)

o = order(abs(prccs$est),decreasing = T)
prccs = prccs[o,]



# qplot(simset.all.max@parameters[,"age.40.to.49.transmission.multiplier.3"],outcome) + geom_smooth()

calculate.low.high.quantiles = function(parameters,
                                        parameter.name,
                                        outcome,
                                        probs=c(0.025,.25,.5,.75,.975),
                                        n=250){
    
    o=order(parameters[,parameter.name])
    low = quantile(outcome[o[1:n]],probs)
    high = quantile(outcome[o[(length(outcome)+1)-1:n]],probs)
    
    return(c(low,high))
    
}


low.high = sapply(simset.all.max@parameter.names,
                  calculate.low.high.quantiles, 
                  parameters=simset.all.max@parameters,
                  outcome=outcome,
                  n=250)

dim.names = list(quantile=c("lower.2","lower.1","median","upper.1","upper.2"),
                 subset = c("low","high"), # these are the low sims and high sims
                 parameter=simset.all.max@parameter.names)

dim(low.high) = sapply(dim.names,length)
dimnames(low.high) = dim.names


parameters.to.plot = dimnames(prccs)[[1]][1:10] # picking the top 10 based on PRCC value (multivariate)

diff = low.high["median","high",]-low.high["median","low",]
parameters.to.plot = dimnames(low.high)$parameter[order(abs(diff), decreasing = T)][1:10] # here is if I pick top 10 based on diff (univariate)

plot.low.high = function(low.high,
                         parameters.to.plot,
                         parameter.names.for.labels){

    mat = low.high[,,parameters.to.plot, drop=F]
    dimnames(mat)[[3]] = parameter.names.for.labels
    diff = mat["median","high",]-mat["median","low",]
        
    df = cbind(reshape2::melt(mat["median",,]),
               data.frame(lower.2=as.numeric(mat["lower.2",,]),
                          lower.1=as.numeric(mat["lower.1",,]),
                          upper.1=as.numeric(mat["upper.1",,]),
                          upper.2=as.numeric(mat["upper.2",,])))
    
    df$parameter=factor(df$parameter,levels=parameter.names.for.labels[order(abs(diff))])
    
    ggplot(df) + geom_boxplot(aes(y=parameter,xmiddle=value,xlower = lower.1,xupper = upper.1, xmin = lower.2, xmax = upper.2, 
                              fill=subset), stat="identity", position="dodge")
    
}



plot.low.high(low.high = low.high,parameters.to.plot = parameters.to.plot)

parameters.to.plot = dimnames(prccs)[[1]][1:12] # picking the top 10 based on PRCC value (multivariate)
parameters.to.plot = parameters.to.plot[c(-3,-4)] # only picking the mortality slopes, not intercepts 

parameter.names.for.labels = c("Age 45-65 all-cause \nmortality (-0.80)","Age 65+ all-cause \nmortality (-0.68)",
                             "Transmission rate \nafter 2030 (-0.32)","Age 40-49 transmission, \n2018 (0.22)",
                             "Age 0-14 HIV mortality, \n2008 (0.22)","Age 20-29 transmission, \n2018 (-0.21)",
                             "Rate of disengagement, \nsuppressed (-0.16)","Age 15-24 HIV mortality, \n2008 (0.15)",
                             "Age 50+ transmission, \n2018 (0.14)","Maternal transmission \nrisk, 2020 (-0.14)")

library(scales)
cols = hue_pal()(2)

jpeg(file=paste0("results/Figure3.jpeg"), width = 1500,height = 1000,res=200)
plot.low.high(low.high = low.high,parameters.to.plot = parameters.to.plot, 
              parameter.names.for.labels=parameter.names.for.labels) + 
    geom_vline(xintercept = 0.41, linetype="dashed") + 
    theme(panel.background = element_blank(),
          legend.position = "bottom",
          legend.justification = "right",
          axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_text(margin = margin(r = -20))) +
    labs(x = "Proportion of PLWH over age 50, 2040") + 
    scale_fill_manual(name = element_blank(),
                      labels = c("low" = "Simulations with the lowest 25% of parameter values",
                                 "high" = "Simulations with the highest 25% of parameter values"),
                      values = cols[1:2])

dev.off()





