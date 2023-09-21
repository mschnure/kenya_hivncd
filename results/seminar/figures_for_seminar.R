## Figure 1 - new
jpeg(file=paste0("results/seminar/Figure1.top.seminar.jpeg"), width = 3000,height = 900,res=200)
simplot(simset.no.int,
        data.types = "incidence",
        ages = c("All ages","0-14","15-49","50 and over"),
        facet.by = "age",
        years=1990:2020, 
        show.individual.sims = F,
        for.paper = T,
        ncol=4) +
    theme(strip.text.x = element_blank(),
          text = element_text(size = 20),
          legend.position = "none")+
    scale_y_continuous(labels = function(x){format(x,big.mark=",",scientific = FALSE)},name = NULL, limits = c(0,NA))
dev.off() 

jpeg(file=paste0("results/seminar/Figure1.bottom.seminar.jpeg"), width = 3000,height = 900,res=200)
simplot(simset.no.int,
        data.types = "prevalence",
        ages = c("All ages","0-14","15-49","50 and over"),
        facet.by = "age",
        years=1990:2020, 
        show.individual.sims = F,
        for.paper = T,
        ncol=4) +
    theme(strip.text.x = element_blank(),
          text = element_text(size = 20),
          legend.position = "none")+
    scale_y_continuous(labels = function(x){format(x,big.mark=",",scientific = FALSE)},name = NULL, limits = c(0,NA))
dev.off() 

## Figure 1 - only 2020-2040
jpeg(file=paste0("results/seminar/Figure1.top.seminar.zoom.jpeg"), width = 3000,height = 900,res=200)
simplot(simset.no.int,
        data.types = "incidence",
        ages = c("All ages","0-14","15-49","50 and over"),
        facet.by = "age",
        years=2000:2040, 
        show.individual.sims = F,
        for.paper = T,
        ncol=4) +
    theme(strip.text.x = element_blank(),
          text = element_text(size = 20),
          legend.position = "none")+
    scale_y_continuous(labels = function(x){format(x,big.mark=",",scientific = FALSE)},name = NULL, limits = c(0,NA))
dev.off() 

jpeg(file=paste0("results/seminar/Figure1.bottom.seminar.zoom.jpeg"), width = 3000,height = 900,res=200)
simplot(simset.no.int,
        data.types = "prevalence",
        ages = c("All ages","0-14","15-49","50 and over"),
        facet.by = "age",
        years=2000:2040, 
        show.individual.sims = F,
        for.paper = T,
        ncol=4) +
    theme(strip.text.x = element_blank(),
          text = element_text(size = 20),
          legend.position = "none")+
    scale_y_continuous(labels = function(x){format(x,big.mark=",",scientific = FALSE)},name = NULL, limits = c(0,NA))
dev.off() 


## Figure 1 - with int
jpeg(file=paste0("results/seminar/Figure1.top.seminar.int.jpeg"), width = 3000,height = 900,res=200)
simplot(simset.no.int,simset.all.max,
        data.types = "incidence",
        ages = c("All ages","0-14","15-49","50 and over"),
        facet.by = "age",
        years=2000:2040, 
        show.individual.sims = F,
        for.paper = T,
        ncol=4) +
    theme(strip.text.x = element_blank(),
          text = element_text(size = 20),
          legend.position = "none")+
    scale_y_continuous(labels = function(x){format(x,big.mark=",",scientific = FALSE)},name = NULL, limits = c(0,NA))+
    geom_vline(xintercept = 2025, linetype="dashed",alpha=0.5) 
dev.off() 

jpeg(file=paste0("results/seminar/Figure1.bottom.seminar.int.jpeg"), width = 3000,height = 900,res=200)
simplot(simset.no.int,simset.all.max,
        data.types = "prevalence",
        ages = c("All ages","0-14","15-49","50 and over"),
        facet.by = "age",
        years=2000:2040, 
        show.individual.sims = F,
        for.paper = T,
        ncol=4) +
    theme(strip.text.x = element_blank(),
          text = element_text(size = 20),
          legend.position = "none")+
    scale_y_continuous(labels = function(x){format(x,big.mark=",",scientific = FALSE)},name = NULL, limits = c(0,NA))+
    geom_vline(xintercept = 2025, linetype="dashed",alpha=0.5) 
dev.off() 

# Figure S4
jpeg(file=paste0("results/seminar/FigureS4.jpeg"), width = 2000,height = 1500,res=200)
generate.age.distribution(full.results.array, 
                          outcome="engagement", 
                          intervention.1 = "no.int",year.1="2025",
                          intervention.2 = "no.int",year.2="2040",
                          intervention.3 = "all.max",year.3="2040",
                          percent=F,
                          sexes = c("female","male"),
                          plot.limits=c(0,175000)) + 
    scale_fill_manual(labels = c("no.int/2025" = "2025",
                                 "no.int/2040" = "No intervention, 2040",
                                 "all.max/2040" = "Full intervention, 2040"), 
                      values=alpha(c("no.int/2025" = pal[1],
                                     "no.int/2040" = pal[2],
                                     "all.max/2040" = pal[3]),alpha), # change legend and color scheme  
                      name=NULL) +
    theme(text = element_text(size = 20))+
    labs(title = NULL,subtitle = NULL) +
    guides(x =  guide_axis(angle = 45))
dev.off()


jpeg(file=paste0("results/seminar/Figure2.1.jpeg"), width = 2000,height = 1500,res=200)
generate.age.distribution(full.results.array, 
                          outcome="prevalence", 
                          intervention.1 = "no.int",year.1="2025",
                          intervention.2 = "no.int",year.2="2040",
                          intervention.3 = "all.max",year.3="2040",
                          percent=F,
                          sexes = c("female","male"),
                          plot.limits=c(0,200000)) + 
    scale_fill_manual(labels = c("no.int/2025" = "2025",
                                 "no.int/2040" = "No intervention, 2040",
                                 "all.max/2040" = "Full intervention, 2040"), 
                      values=alpha(c("no.int/2025" = pal[1],
                                     "no.int/2040" = "white",
                                     "all.max/2040" = "white"),alpha), # change legend and color scheme  
                      name=NULL) +
    theme(text = element_text(size = 20))+
    labs(title = NULL,subtitle = NULL) +
    guides(x =  guide_axis(angle = 45))
dev.off()

jpeg(file=paste0("results/seminar/Figure2.2.jpeg"), width = 2000,height = 1500,res=200)
generate.age.distribution(full.results.array, 
                          outcome="prevalence", 
                          intervention.1 = "no.int",year.1="2025",
                          intervention.2 = "no.int",year.2="2040",
                          intervention.3 = "all.max",year.3="2040",
                          percent=F,
                          sexes = c("female","male"),
                          plot.limits=c(0,200000)) + 
    scale_fill_manual(labels = c("no.int/2025" = "2025",
                                 "no.int/2040" = "No intervention, 2040",
                                 "all.max/2040" = "Full intervention, 2040"), 
                      values=alpha(c("no.int/2025" = pal[1],
                                     "no.int/2040" = pal[2],
                                     "all.max/2040" = "white"),alpha), # change legend and color scheme  
                      name=NULL) +
    theme(text = element_text(size = 20))+
    labs(title = NULL,subtitle = NULL) +
    guides(x =  guide_axis(angle = 45))
dev.off()


# General population age structure 
jpeg(file=paste0("results/seminar/gen_pop_structure.jpeg"), width = 2000,height = 1500,res=200)
generate.age.distribution(full.results.array, 
                          outcome="population", 
                          intervention.1 = "no.int",year.1="2025",
                          intervention.2 = "no.int",year.2="2040",
                          intervention.3 = "all.max",year.3="2040",
                          percent=F,
                          sexes = c("female","male"),
                          plot.limits=c(0,9000000)) + 
    scale_fill_manual(labels = c("no.int/2025" = "2025",
                                 "no.int/2040" = "No intervention, 2040",
                                 "all.max/2040" = "Full intervention, 2040"), 
                      values=alpha(c("no.int/2025" = pal[1],
                                     "no.int/2040" = pal[2],
                                     "all.max/2040" = pal[3]),alpha), # change legend and color scheme  
                      name=NULL) +
    theme(text = element_text(size = 20))+
    labs(title = NULL,subtitle = NULL) +
    guides(x =  guide_axis(angle = 45))
dev.off()

{ # General population summary stats
    outcomes = c("population")
    dim.names = list(intervention=dimnames(full.results.array)$intervention,
                     outcome.year = paste0(rep(outcomes,each=2),rep(c(".2025",".2040"),2)))
    
    gen.pop.median.age.table = generate.median.age.table(simset.list = simset.list.full,
                                                                       data.types = c("population"),
                                                                       years = c(2025,2040))
    
    gen.pop.over.50.table = generate.percent.over.age.table(simset.list = simset.list.full,
                                                                          age.point=50,
                                                                          data.types = c("population"),
                                                                          years=c(2025,2040))
    
}



# Factors impacting aging 
# incidence among young vs old 
# mortality among young vs old 
        # hiv.mortality.array.new = results.array.new[,,,"hiv.mortality",,]
        # non.hiv.mortality.array.new = results.array.new[,,,"non.hiv.mortality",,]

prp.over.50.2025 = sapply(simset.all.max@simulations, function(sim){
    calculate.percent.over.age.for.sim(sim, 
                                       age.point = 50,
                                       data.type = "prevalence",
                                       years = "2025",
                                       sexes = c("female","male"))
})

prp.over.50.2040.int = sapply(simset.all.max@simulations, function(sim){
    calculate.percent.over.age.for.sim(sim, 
                                       age.point = 50,
                                       data.type = "prevalence",
                                       years = "2040",
                                       sexes = c("female","male"))
})

prp.over.50.2040.no.int = sapply(simset.no.int@simulations, function(sim){
    calculate.percent.over.age.for.sim(sim, 
                                       age.point = 50,
                                       data.type = "prevalence",
                                       years = "2040",
                                       sexes = c("female","male"))
})


# CHOOSE ONE OF THESE
{
    int.scenario = 6 # Intervention scenario
    delta.prp.over.50 = prp.over.50.2040.int - prp.over.50.2025
}

{
    int.scenario = 1 # Baseline scenario   
    delta.prp.over.50 = prp.over.50.2040.no.int - prp.over.50.2025
}

{
    results.array.new = full.results.array[c("2025","2040"),,,,,int.scenario] # either baseline or full intervention
    inc = results.array.new[,,,"incidence",]
    
    under.30.inc = inc[,c(1:6),,]
    under.30.inc.total = apply(under.30.inc,c(1,4),sum)
    under.30.inc.diff = (under.30.inc.total[1,]-under.30.inc.total[2,])/under.30.inc.total[1,] # 2025-2040
    
    age.30.to.50.inc = inc[,c(7:10),,]
    age.30.to.50.inc.total = apply(age.30.to.50.inc,c(1,4),sum)
    age.30.to.50.inc.diff = (age.30.to.50.inc.total[1,]-age.30.to.50.inc.total[2,])/age.30.to.50.inc.total[1,]
    
    age.50.and.over.inc = inc[,c(11:17),,]
    age.50.and.over.inc.total = apply(age.50.and.over.inc,c(1,4),sum)
    age.50.and.over.inc.diff = (age.50.and.over.inc.total[1,]-age.50.and.over.inc.total[2,])/age.50.and.over.inc.total[1,]
    
    non.hiv.mort = results.array.new[,,,"non.hiv.mortality",]
    
    under.30.non.hiv.mort = non.hiv.mort[,c(1:6),,]
    under.30.non.hiv.mort.total = apply(under.30.non.hiv.mort,c(1,4),sum)
    under.30.non.hiv.mort.diff = (under.30.non.hiv.mort.total[1,]-under.30.non.hiv.mort.total[2,])/under.30.non.hiv.mort.total[1,]
    
    age.30.to.50.non.hiv.mort = non.hiv.mort[,c(7:10),,]
    age.30.to.50.non.hiv.mort.total = apply(age.30.to.50.non.hiv.mort,c(1,4),sum)
    age.30.to.50.non.hiv.mort.diff = (age.30.to.50.non.hiv.mort.total[1,]-age.30.to.50.non.hiv.mort.total[2,])/age.30.to.50.non.hiv.mort.total[1,]
    
    age.50.and.over.non.hiv.mort = non.hiv.mort[,c(11:17),,]
    age.50.and.over.non.hiv.mort.total = apply(age.50.and.over.non.hiv.mort,c(1,4),sum)
    age.50.and.over.non.hiv.mort.diff = (age.50.and.over.non.hiv.mort.total[1,]-age.50.and.over.non.hiv.mort.total[2,])/age.50.and.over.non.hiv.mort.total[1,]
    
}


# Looking at the change in the proportion over 50 in the intervention scenario vs baseline
jpeg(file=paste0("results/seminar/new.SA.1.jpeg"), width = 2000,height = 1500,res=200)
plot(under.30.inc.diff,delta.prp.over.50,
     xlab = "Percent reduction in incidence, 2025-2040, age <30",
     ylab = "Change in proportion over age 50, 2025-2040") + abline(lm(delta.prp.over.50~under.30.inc.diff))
dev.off()
jpeg(file=paste0("results/seminar/new.SA.2.jpeg"), width = 2000,height = 1500,res=200)
plot(age.30.to.50.inc.diff,delta.prp.over.50,
     xlab = "Percent reduction in incidence, 2025-2040, age 30-50",
     ylab = "Change in proportion over age 50, 2025-2040") + abline(lm(delta.prp.over.50~age.30.to.50.inc.diff))
dev.off()
jpeg(file=paste0("results/seminar/new.SA.3.jpeg"), width = 2000,height = 1500,res=200)
plot(age.50.and.over.inc.diff,delta.prp.over.50,
     xlab = "Percent reduction in incidence, 2025-2040, age 50+",
     ylab = "Change in proportion over age 50, 2025-2040") + abline(lm(delta.prp.over.50~age.50.and.over.inc.diff))
dev.off()

# Looking at the change in the proportion over 50 in the intervention scenario vs baseline
jpeg(file=paste0("results/seminar/new.SA.4.jpeg"), width = 2000,height = 1500,res=200)
plot(under.30.non.hiv.mort.diff,delta.prp.over.50,
     xlab = "Percent reduction in non-HIV mortality, 2025-2040, age <30",
     ylab = "Change in proportion over age 50, 2025-2040") + abline(lm(delta.prp.over.50~under.30.non.hiv.mort.diff))
dev.off()
jpeg(file=paste0("results/seminar/new.SA.5.jpeg"), width = 2000,height = 1500,res=200)
plot(age.30.to.50.non.hiv.mort.diff,delta.prp.over.50,
     xlab = "Percent reduction in non-HIV mortality, 2025-2040, age 30-50",
     ylab = "Change in proportion over age 50, 2025-2040") + abline(lm(delta.prp.over.50~age.30.to.50.non.hiv.mort.diff))
dev.off()
jpeg(file=paste0("results/seminar/new.SA.6.jpeg"), width = 2000,height = 1500,res=200)
plot(age.50.and.over.non.hiv.mort.diff,delta.prp.over.50,
     xlab = "Percent reduction in non-HIV mortality, 2025-2040, age 50+",
     ylab = "Change in proportion over age 50, 2025-2040") + abline(lm(delta.prp.over.50~age.50.and.over.non.hiv.mort.diff))
dev.off()

fit = lm(delta.prp.over.50~under.30.inc.diff+age.30.to.50.inc.diff+age.50.and.over.inc.diff+
             under.30.non.hiv.mort.diff+age.30.to.50.non.hiv.mort.diff+age.50.and.over.non.hiv.mort.diff)

df = cbind(under.30.inc.diff,age.30.to.50.inc.diff,age.50.and.over.inc.diff,
               under.30.non.hiv.mort.diff,age.30.to.50.non.hiv.mort.diff,age.50.and.over.non.hiv.mort.diff,delta.prp.over.50)

# set up data frame
prccs = epi.prcc(df)

fit.2 = lm(prp.over.50~under.30.inc.diff+age.30.to.50.inc.diff+age.50.and.over.inc.diff+
             under.30.non.hiv.mort.diff+age.30.to.50.non.hiv.mort.diff+age.50.and.over.non.hiv.mort.diff)


# OLD CODE
{
    # Looking at just the proportion over 50 in the intervention scenario
    plot(under.30.inc.diff,prp.over.50) + abline(lm(prp.over.50~under.30.inc.diff))
    plot(age.30.to.50.inc.diff,prp.over.50)+ abline(lm(prp.over.50~age.30.to.50.inc.diff))
    plot(age.50.and.over.inc.diff,prp.over.50)+ abline(lm(prp.over.50~age.50.and.over.inc.diff))
    
    
    # Looking at just the proportion over 50 in the intervention scenario
    plot(under.30.non.hiv.mort.diff,prp.over.50) + abline(lm(prp.over.50~under.30.non.hiv.mort.diff))
    plot(age.30.to.50.non.hiv.mort.diff,prp.over.50)+ abline(lm(prp.over.50~age.30.to.50.non.hiv.mort.diff))
    # x-axis is (2025 mortality-2040 mortality)/2025 mortality
    # so more negative x-axis --> more to the left side --> higher 2040 mortality --> lower proportion over 50 
    # so, as general mortality in this age group increases from 2025 to 2040, the proportion over age 50 decreases
    
    plot(age.50.and.over.non.hiv.mort.diff,prp.over.50)+ abline(lm(prp.over.50~age.50.and.over.non.hiv.mort.diff))
        
}
