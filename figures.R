##############################################################################
# Description: Generate results/figures from interventions run on simset.final 
##############################################################################
# install.packages("RColorBrewer")
library("RColorBrewer")
library("scales")
library("ggsci")

# Types of figures
#     1. Calibration plots (no intervention, 2000-2020)
#     2. Intervention plots (2000-2040)
#     3. Age distribution plots (2040)

# source this file if I actually need to run the interventions again 
# source("interventions/run_interventions_on_simset.R")

# load these results if I already ran the interventions 
source("model/run_systematic.R")
source("interventions/extract_intervention_results.R")
load("cached/all.results_2023-05-06.Rdata")
simset.no.int = simset.list.full$no.int    
simset.all.max = simset.list.full$all.max
simset.engagement.retention = simset.list.full$engagement.retention


calibration.plot.years = 2000:2020
intervention.plot.years = 2000:2040

##--------------------##
##-- PAPER FIGURES  --##
##--------------------##

# pal = ggsci::pal_locuszoom()(5) # function to pick from a journal's color scheme; then specify how many colors you need
# pal = c(brewer.pal(n=12,"Paired")[1],brewer.pal(n=12,"Paired")[5],brewer.pal(n=12,"Paired")[3]) # alph = 1
# pal = c(hue_pal()(3)[3],hue_pal()(3)[1],hue_pal()(3)[2]) # alpha = 1

# paired, 2-5-4 or 1-5-4 or 1-5-3
# or, original colors but flip pink and blue (hue_pal) 3-1-2
pal = c(brewer.pal(n=12,"Paired")[2],brewer.pal(n=12,"Paired")[5],brewer.pal(n=12,"Paired")[4]) # use ALPHA = 0.8
alpha = 0.8



## Figure 1
# jpeg(file=paste0("results/Figure1.jpeg"), width = 3000,height = 2000,res=200)
# simplot(simset.no.int, simset.all.max,
#         # data.types = "incidence",
#         ages = c("All ages","0-14","15-49","50 and over"),
#         facet.by = "age",
#         years=intervention.plot.years, 
#         show.individual.sims = F,
#         for.paper = T,
#         ncol=4) 
# dev.off() 

## Figure 1 - new
jpeg(file=paste0("results/Figure1.top.jpeg"), width = 3000,height = 1000,res=200)
simplot(simset.no.int, simset.all.max,
        data.types = "incidence",
        ages = c("All ages","0-14","15-49","50 and over"),
        facet.by = "age",
        years=intervention.plot.years, 
        show.individual.sims = F,
        for.paper = T,
        ncol=4) +
    theme(strip.text.x = element_blank(),
          text = element_text(size = 20),
          legend.position = "none")+
    scale_y_continuous(labels = function(x){format(x,big.mark=",",scientific = FALSE)},name = NULL, limits = c(0,NA))
dev.off() 

jpeg(file=paste0("results/Figure1.jpeg"), width = 3000,height = 2000,res=200)
simplot(simset.no.int, simset.all.max,
        #data.types = "prevalence",
        ages = c("All ages","0-14","15-49","50 and over"),
        facet.by = "age",
        years=intervention.plot.years, 
        show.individual.sims = F,
        for.paper = T,
        ncol=4) +
    theme(strip.text.x = element_blank(),
          text = element_text(size = 20))+
    scale_y_continuous(labels = function(x){format(x,big.mark=",",scientific = FALSE)},name = NULL, limits = c(0,NA))+
    geom_vline(xintercept = 2025, linetype="dashed",alpha=0.5) 
dev.off() 

## Figure 2
jpeg(file=paste0("results/Figure2.jpeg"), width = 2000,height = 1500,res=200)
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
                               "all.max/2040" = pal[3]),alpha), # change legend and color scheme  
                      name=NULL) +
    theme(text = element_text(size = 20))+
    labs(title = NULL,subtitle = NULL) +
    guides(x =  guide_axis(angle = 45))
dev.off()


##---------------------------##
##-- SUPPLEMENTAL FIGURES  --##
##---------------------------##
# Figure S1
jpeg(file=paste0("results/FigureS1.jpeg"), width = 2000,height = 1500,res=200)
generate.age.distribution(full.results.array, 
                          outcome="prevalence", 
                          intervention.1 = "no.int",year.1="2025",
                          intervention.2 = "no.int",year.2="2040",
                          intervention.3 = "all.max",year.3="2040",
                          percent=T,
                          sexes = c("female","male"),
                          plot.limits=c(0,0.13)) + 
    scale_fill_manual(labels = c("no.int/2025" = "2025",
                                 "no.int/2040" = "No intervention, 2040",
                                 "all.max/2040" = "Full intervention, 2040"), 
                      values=alpha(c("no.int/2025" = pal[1],
                                     "no.int/2040" = pal[2],
                                     "all.max/2040" = pal[3]),alpha), # change legend and color scheme  
                      name=NULL) 

dev.off()

# Figure S2A
jpeg(file=paste0("results/FigureS2_A.jpeg"), width = 2000,height = 1500,res=200)
generate.age.distribution(full.results.array, 
                          outcome="engagement", 
                          intervention.1 = "no.int",year.1="2025",
                          intervention.2 = "no.int",year.2="2040",
                          intervention.3 = "all.max",year.3="2040",
                          percent=F,
                          sexes = c("female","male"),
                          plot.limits = c(0,175000)) + 
    scale_fill_manual(labels = c("no.int/2025" = "2025",
                                 "no.int/2040" = "No intervention, 2040",
                                 "all.max/2040" = "Full intervention, 2040"), 
                      values=alpha(c("no.int/2025" = pal[1],
                                     "no.int/2040" = pal[2],
                                     "all.max/2040" = pal[3]),alpha), # change legend and color scheme  
                      name=NULL) 

dev.off()

# Figure S2B
jpeg(file=paste0("results/FigureS2_B.jpeg"), width = 2000,height = 1500,res=200)
generate.age.distribution(full.results.array, 
                          outcome="engagement", 
                          intervention.1 = "no.int",year.1="2025",
                          intervention.2 = "no.int",year.2="2040",
                          intervention.3 = "all.max",year.3="2040",
                          percent=T,
                          sexes = c("female","male"),
                          plot.limits = c(0,0.14)) + 
    scale_fill_manual(labels = c("no.int/2025" = "2025",
                                 "no.int/2040" = "No intervention, 2040",
                                 "all.max/2040" = "Full intervention, 2040"), 
                      values=alpha(c("no.int/2025" = pal[1],
                                     "no.int/2040" = pal[2],
                                     "all.max/2040" = pal[3]),alpha), # change legend and color scheme  
                      name=NULL) 
dev.off()

# Figure S3
jpeg(file=paste0("results/FigureS3_A.jpeg"), width = 2000,height = 1500,res=200)
generate.age.distribution(full.results.array, 
                          outcome="prevalence", 
                          intervention.1 = "no.int",year.1="2025",
                          intervention.2 = "no.int",year.2="2040",
                          intervention.3 = "all.max",year.3="2040",
                          percent=F,
                          sexes = c("female"),
                          plot.limits = c(0,107000)) + 
    scale_fill_manual(labels = c("no.int/2025" = "2025",
                                 "no.int/2040" = "No intervention, 2040",
                                 "all.max/2040" = "Full intervention, 2040"), 
                      values=alpha(c("no.int/2025" = pal[1],
                                     "no.int/2040" = pal[2],
                                     "all.max/2040" = pal[3]),alpha), # change legend and color scheme  
                      name=NULL) 
dev.off()

jpeg(file=paste0("results/FigureS3_B.jpeg"), width = 2000,height = 1500,res=200)
generate.age.distribution(full.results.array, 
                          outcome="prevalence", 
                          intervention.1 = "no.int",year.1="2025",
                          intervention.2 = "no.int",year.2="2040",
                          intervention.3 = "all.max",year.3="2040",
                          percent=F,
                          sexes = c("male"),
                          plot.limits = c(0,107000)) + 
    scale_fill_manual(labels = c("no.int/2025" = "2025",
                                 "no.int/2040" = "No intervention, 2040",
                                 "all.max/2040" = "Full intervention, 2040"), 
                      values=alpha(c("no.int/2025" = pal[1],
                                     "no.int/2040" = pal[2],
                                     "all.max/2040" = pal[3]),alpha), # change legend and color scheme  
                      name=NULL) 
dev.off()

# Figure S4
jpeg(file=paste0("results/FigureS4_A.jpeg"), width = 2000,height = 1500,res=200)
generate.age.distribution(full.results.array, 
                          outcome="prevalence", 
                          intervention.1 = "no.int",year.1="2025",
                          intervention.2 = "no.int",year.2="2040",
                          intervention.3 = "all.max",year.3="2040",
                          percent=T,
                          sexes = c("female"),
                          plot.limits = c(0,.15)) + 
    scale_fill_manual(labels = c("no.int/2025" = "2025",
                                 "no.int/2040" = "No intervention, 2040",
                                 "all.max/2040" = "Full intervention, 2040"), 
                      values=alpha(c("no.int/2025" = pal[1],
                                     "no.int/2040" = pal[2],
                                     "all.max/2040" = pal[3]),alpha), # change legend and color scheme  
                      name=NULL) 
dev.off()

jpeg(file=paste0("results/FigureS4_B.jpeg"), width = 2000,height = 1500,res=200)
generate.age.distribution(full.results.array, 
                          outcome="prevalence", 
                          intervention.1 = "no.int",year.1="2025",
                          intervention.2 = "no.int",year.2="2040",
                          intervention.3 = "all.max",year.3="2040",
                          percent=T,
                          sexes = c("male"),
                          plot.limits = c(0,.15)) + 
    scale_fill_manual(labels = c("no.int/2025" = "2025",
                                 "no.int/2040" = "No intervention, 2040",
                                 "all.max/2040" = "Full intervention, 2040"), 
                      values=alpha(c("no.int/2025" = pal[1],
                                     "no.int/2040" = pal[2],
                                     "all.max/2040" = pal[3]),alpha), # change legend and color scheme  
                      name=NULL) 
dev.off()

# Figure S5
jpeg(file=paste0("results/FigureS5_A.jpeg"), width = 2000,height = 1500,res=200)
generate.age.distribution(full.results.array, 
                          outcome="engagement", 
                          intervention.1 = "no.int",year.1="2025",
                          intervention.2 = "no.int",year.2="2040",
                          intervention.3 = "all.max",year.3="2040",
                          percent=F,
                          sexes = c("female"),
                          plot.limits = c(0,107000)) + 
    scale_fill_manual(labels = c("no.int/2025" = "2025",
                                 "no.int/2040" = "No intervention, 2040",
                                 "all.max/2040" = "Full intervention, 2040"), 
                      values=alpha(c("no.int/2025" = pal[1],
                                     "no.int/2040" = pal[2],
                                     "all.max/2040" = pal[3]),alpha), # change legend and color scheme  
                      name=NULL) 
dev.off()

jpeg(file=paste0("results/FigureS5_B.jpeg"), width = 2000,height = 1500,res=200)
generate.age.distribution(full.results.array, 
                          outcome="engagement", 
                          intervention.1 = "no.int",year.1="2025",
                          intervention.2 = "no.int",year.2="2040",
                          intervention.3 = "all.max",year.3="2040",
                          percent=F,
                          sexes = c("male"),
                          plot.limits = c(0,107000)) + 
    scale_fill_manual(labels = c("no.int/2025" = "2025",
                                 "no.int/2040" = "No intervention, 2040",
                                 "all.max/2040" = "Full intervention, 2040"), 
                      values=alpha(c("no.int/2025" = pal[1],
                                     "no.int/2040" = pal[2],
                                     "all.max/2040" = pal[3]),alpha), # change legend and color scheme  
                      name=NULL) 
dev.off()

# Figure S6
jpeg(file=paste0("results/FigureS6_A.jpeg"), width = 2000,height = 1500,res=200)
generate.age.distribution(full.results.array, 
                          outcome="engagement", 
                          intervention.1 = "no.int",year.1="2025",
                          intervention.2 = "no.int",year.2="2040",
                          intervention.3 = "all.max",year.3="2040",
                          percent=T,
                          sexes = c("female"),
                          plot.limits = c(0,.15)) + 
    scale_fill_manual(labels = c("no.int/2025" = "2025",
                                 "no.int/2040" = "No intervention, 2040",
                                 "all.max/2040" = "Full intervention, 2040"), 
                      values=alpha(c("no.int/2025" = pal[1],
                                     "no.int/2040" = pal[2],
                                     "all.max/2040" = pal[3]),alpha), # change legend and color scheme  
                      name=NULL) 
dev.off()

jpeg(file=paste0("results/FigureS6_B.jpeg"), width = 2000,height = 1500,res=200)
generate.age.distribution(full.results.array, 
                          outcome="engagement", 
                          intervention.1 = "no.int",year.1="2025",
                          intervention.2 = "no.int",year.2="2040",
                          intervention.3 = "all.max",year.3="2040",
                          percent=T,
                          sexes = c("male"),
                          plot.limits = c(0,.15)) + 
    scale_fill_manual(labels = c("no.int/2025" = "2025",
                                 "no.int/2040" = "No intervention, 2040",
                                 "all.max/2040" = "Full intervention, 2040"), 
                      values=alpha(c("no.int/2025" = pal[1],
                                     "no.int/2040" = pal[2],
                                     "all.max/2040" = pal[3]),alpha), # change legend and color scheme  
                      name=NULL) 
dev.off()

# Figure S7
jpeg(file=paste0("results/FigureS7_A.jpeg"), width = 2000,height = 1500,res=200)
generate.age.distribution(full.results.array, 
                          outcome="incidence", 
                          intervention.1 = "no.int",year.1="2025",
                          intervention.2 = "no.int",year.2="2040",
                          intervention.3 = "all.max",year.3="2040",
                          percent=T,
                          sexes = c("female","male"),
                          plot.limits = c(0,0.31)) + 
    scale_fill_manual(labels = c("no.int/2025" = "2025",
                                 "no.int/2040" = "No intervention, 2040",
                                 "all.max/2040" = "Full intervention, 2040"), 
                      values=alpha(c("no.int/2025" = pal[1],
                                     "no.int/2040" = pal[2],
                                     "all.max/2040" = pal[3]),alpha), # change legend and color scheme  
                      name=NULL) 
dev.off()

jpeg(file=paste0("results/FigureS7_B.jpeg"), width = 2000,height = 1500,res=200)
generate.age.distribution(full.results.array, 
                          outcome="incidence", 
                          intervention.1 = "no.int",year.1="2025",
                          intervention.2 = "no.int",year.2="2040",
                          intervention.3 = "all.max",year.3="2040",
                          percent=T,
                          sexes = c("female"),
                          plot.limits = c(0,0.31)) + 
    scale_fill_manual(labels = c("no.int/2025" = "2025",
                                 "no.int/2040" = "No intervention, 2040",
                                 "all.max/2040" = "Full intervention, 2040"), 
                      values=alpha(c("no.int/2025" = pal[1],
                                     "no.int/2040" = pal[2],
                                     "all.max/2040" = pal[3]),alpha), # change legend and color scheme  
                      name=NULL) 
dev.off()

jpeg(file=paste0("results/FigureS7_C.jpeg"), width = 2000,height = 1500,res=200)
generate.age.distribution(full.results.array, 
                          outcome="incidence", 
                          intervention.1 = "no.int",year.1="2025",
                          intervention.2 = "no.int",year.2="2040",
                          intervention.3 = "all.max",year.3="2040",
                          percent=T,
                          sexes = c("male"),
                          plot.limits = c(0,0.31)) + 
    scale_fill_manual(labels = c("no.int/2025" = "2025",
                                 "no.int/2040" = "No intervention, 2040",
                                 "all.max/2040" = "Full intervention, 2040"), 
                      values=alpha(c("no.int/2025" = pal[1],
                                     "no.int/2040" = pal[2],
                                     "all.max/2040" = pal[3]),alpha), # change legend and color scheme  
                      name=NULL) 
dev.off()

# HISTORICAL
jpeg(file=paste0("results/FigureS8.jpeg"), width = 2000,height = 1500,res=200)
generate.age.distribution(full.results.array, 
                          outcome="prevalence", 
                          intervention.1 = "no.int",year.1="2000",
                          intervention.2 = "no.int",year.2="2010",
                          intervention.3 = "no.int",year.3="2020",
                          percent=F,
                          sexes = c("female","male"),
                          plot.limits=c(0,315000)) + 
    scale_fill_manual(labels = c("no.int/2000" = "2000",
                                 "no.int/2010" = "2010",
                                 "no.int/2020" = "2020"), 
                      values=alpha(c("no.int/2000" = pal[1],
                                     "no.int/2010" = pal[2],
                                     "no.int/2020" = pal[3]),alpha), # change legend and color scheme  
                      name=NULL) 
dev.off()

##------------------------##
##-- SUMMARY STATISTICS --##
##------------------------##
{ # Female and male
    outcomes = c("prevalence","engagement","incidence","annual.engagement")
    dim.names = list(intervention=dimnames(full.results.array)$intervention,
                     outcome.year = paste0(rep(outcomes,each=2),rep(c(".2025",".2040"),2)))
    
    median.export.to.csv = c(prevalence.engagement.median.age.table,incidence.annual.engagement.median.age.table)
    dim(median.export.to.csv) = sapply(dim.names,length)
    dimnames(median.export.to.csv) = dim.names
    
    prop.over.age.export.to.csv = c(prevalence.engagement.over.50.table,incidence.over.30.table,annual.engagement.over.30.table)
    dim(prop.over.age.export.to.csv) = sapply(dim.names,length)
    dimnames(prop.over.age.export.to.csv) = dim.names
    
    # median.export.to.csv
    # prop.over.age.export.to.csv
    
    both.sexes.export = cbind(median.export.to.csv,prop.over.age.export.to.csv)
    
    }

{ # Female only 
    median.export.to.csv.female = c(prevalence.engagement.median.age.table.female,incidence.annual.engagement.median.age.table.female)
    dim(median.export.to.csv.female) = sapply(dim.names,length)
    dimnames(median.export.to.csv.female) = dim.names
    
    prop.over.age.export.to.csv.female = c(prevalence.engagement.over.50.table.female,incidence.over.30.table.female,annual.engagement.over.30.table.female)
    dim(prop.over.age.export.to.csv.female) = sapply(dim.names,length)
    dimnames(prop.over.age.export.to.csv.female) = dim.names
    
    # median.export.to.csv.female
    # prop.over.age.export.to.csv.female
    
    female.export = cbind(median.export.to.csv.female,prop.over.age.export.to.csv.female)
}


{ # Male only 
    median.export.to.csv.male = c(prevalence.engagement.median.age.table.male,incidence.annual.engagement.median.age.table.male)
    dim(median.export.to.csv.male) = sapply(dim.names,length)
    dimnames(median.export.to.csv.male) = dim.names
    
    prop.over.age.export.to.csv.male = c(prevalence.engagement.over.50.table.male,incidence.over.30.table.male,annual.engagement.over.30.table.male)
    dim(prop.over.age.export.to.csv.male) = sapply(dim.names,length)
    dimnames(prop.over.age.export.to.csv.male) = dim.names
    
    # median.export.to.csv.male
    # prop.over.age.export.to.csv.male
    
    male.export = cbind(median.export.to.csv.male,prop.over.age.export.to.csv.male)
}

full.export = rbind(both.sexes.export,female.export,male.export)
write.csv(full.export, file = paste0("results/full.export_",Sys.Date(),".csv"))

calculate.outcome.reduction(full.results.array,target.year="2040",
                            data.type="incidence",
                            intervention="all.max")
calculate.outcome.reduction(full.results.array,target.year="2040",
                            data.type="incidence",
                            intervention="all.intermediate")
calculate.outcome.reduction(full.results.array,target.year="2040",
                            data.type="incidence",
                            intervention="engagement.retention")

calculate.outcome.reduction(full.results.array,target.year="2040",
                            data.type="prevalence",
                            intervention="all.max")


# base.2025
inc.2025 = round(quantile(apply(full.results.array["2025",,,"incidence",,"no.int"],c("sim"),sum), probs = c(.025,.5,.975), na.rm=T),-3)
prev.2025 = round(quantile(apply(full.results.array["2025",,,"prevalence",,"no.int"],c("sim"),sum), probs = c(.025,.5,.975), na.rm=T),-4)
# base.2040
inc.2040.no.int = round(quantile(apply(full.results.array["2040",,,"incidence",,"no.int"],c("sim"),sum), probs = c(.025,.5,.975), na.rm=T),-3) 
prev.2040.no.int = round(quantile(apply(full.results.array["2040",,,"prevalence",,"no.int"],c("sim"),sum), probs = c(.025,.5,.975), na.rm=T),-4) 
# intervention.2040
inc.2040.full.int = round(quantile(apply(full.results.array["2040",,,"incidence",,"all.max"],c("sim"),sum), probs = c(.025,.5,.975), na.rm=T),-3) 
prev.2040.full.int = round(quantile(apply(full.results.array["2040",,,"prevalence",,"all.max"],c("sim"),sum), probs = c(.025,.5,.975), na.rm=T),-4) 

inc.prev.results = rbind(inc.2025,inc.2040.no.int,inc.2040.full.int,
                         prev.2025,prev.2040.no.int,prev.2040.full.int)

# NEW - CI around delta (per simulation)
quantile((apply(full.results.array["2025",,,"prevalence",,"no.int"],c("sim"),sum) - 
        apply(full.results.array["2040",,,"prevalence",,"no.int"],c("sim"),sum)), 
    probs = c(.025,.5,.975), na.rm=T)
qplot(apply(full.results.array["2025",,,"prevalence",,"no.int"],c("sim"),sum) - 
          apply(full.results.array["2040",,,"prevalence",,"no.int"],c("sim"),sum))
table((apply(full.results.array["2025",,,"prevalence",,"no.int"],c("sim"),sum) - 
           apply(full.results.array["2040",,,"prevalence",,"no.int"],c("sim"),sum))>0)/1000 
# 74.9% are greater than 0 (i.e., reduction in prevalence)

quantile((apply(full.results.array["2025",,,"incidence",,"no.int"],c("sim"),sum) - 
              apply(full.results.array["2040",,,"incidence",,"no.int"],c("sim"),sum)), 
         probs = c(.025,.5,.975), na.rm=T)
qplot(apply(full.results.array["2025",,,"incidence",,"no.int"],c("sim"),sum) - 
          apply(full.results.array["2040",,,"incidence",,"no.int"],c("sim"),sum)) 
table((apply(full.results.array["2025",,,"incidence",,"no.int"],c("sim"),sum) - 
           apply(full.results.array["2040",,,"incidence",,"no.int"],c("sim"),sum))>0)/1000 
# 73.7% are greater than 0 (i.e., reduction in incidence)


# more supplemental plots 
jpeg(file=paste0("results/calibration/1_incidence.jpeg"), width = 2000,height = 1500,res=200)
simplot(simset.no.int, data.types='incidence', years=1990:2040, facet.by='age', show.individual.sims = F, for.paper = T) + 
    theme(legend.position = "none")
dev.off()

jpeg(file=paste0("results/calibration/2_prevalence.jpeg"), width = 2000,height = 1500,res=200)
simplot(simset.no.int, data.types='prevalence', years=1990:2040, facet.by='age', show.individual.sims = F, for.paper = T) + 
    theme(legend.position = "none")
dev.off()

jpeg(file=paste0("results/calibration/3_cascade.jpeg"), width = 1000,height = 1500,res=200)
simplot(simset.no.int, data.types=c('awareness',"engagement","suppression"), facet.by = c("age","sex"),
        years=2010:2040, proportion=T, show.individual.sims = F, for.paper = T, ncol=2) + 
    theme(legend.position = "none")
dev.off()

jpeg(file=paste0("results/calibration/4_hiv.mortality.jpeg"), width = 2000,height = 1500,res=200)
simplot(simset.no.int, data.types='hiv.mortality', years=1990:2040, facet.by='age', proportion = F, 
        show.individual.sims = F, for.paper = T) + 
    theme(legend.position = "none")
dev.off()

jpeg(file=paste0("results/calibration/5_population.jpeg"), width = 2000,height = 1500,res=200)
simplot(simset.no.int, data.types='population', years=1990:2030, facet.by='age', show.individual.sims = F, for.paper = T) + 
    theme(legend.position = "none")
dev.off()



##-----------------------------------------##
##-- CALIBRATION PLOTS (no intervention) --##
##-----------------------------------------##

## Incidence 
simplot(simset.no.int, data.types='incidence', years=calibration.plot.years, show.individual.sims = F)
simplot(simset.no.int, data.types='incidence', years=calibration.plot.years, facet.by='age', show.individual.sims = F)
simplot(simset.no.int, data.types='incidence', years=calibration.plot.years, facet.by=c('age',"sex"), ages = "15+", show.individual.sims = F)

## Prevalence 
simplot(simset.no.int, data.types='prevalence', years=calibration.plot.years, show.individual.sims = F)
simplot(simset.no.int, data.types='prevalence', years=calibration.plot.years, facet.by='age', show.individual.sims = F)
simplot(simset.no.int, data.types='prevalence', years=calibration.plot.years, facet.by=c('age',"sex"), ages = "15+", show.individual.sims = F)

## Cascade 
simplot(simset.no.int, data.types=c('awareness',"engagement","suppression"), years=calibration.plot.years, proportion=T, show.individual.sims = F)
simplot(simset.no.int, data.types='awareness', years=calibration.plot.years, facet.by=c('age','sex'), proportion=T, show.individual.sims = F)
simplot(simset.no.int, data.types='engagement', years=calibration.plot.years, facet.by=c('age','sex'), proportion=T, show.individual.sims = F)
simplot(simset.no.int, data.types='suppression', years=calibration.plot.years, facet.by=c('age','sex'), proportion=T, show.individual.sims = F)

## HIV mortality 
simplot(simset.no.int, data.types='hiv.mortality', years=calibration.plot.years, facet.by='age', proportion = T, show.individual.sims = F)

##------------------------##
##-- INTERVENTION PLOTS --##
##------------------------##
# Below plots are all for all.max
# other (intermediate) interventions: 
# simset.all.intermediate (all interventions at lower level)
# simset.engagement.retention (engagement 90; disengagment 10)

# individual interventions (did not run individual ones):
# simset.testing (50 or 75)
# simset.engagement (80 or 90)
# simset.gain.suppression (80 or 90)
# simset.lose.suppression (10 or 5)
# simset.disengagement.unsuppressed (15 or 10)
# simset.disengagement.suppressed (15 or 10)

## Incidence 
simplot(simset.no.int, simset.all.max, data.types='incidence', years=intervention.plot.years, show.individual.sims = F)
simplot(simset.no.int, simset.all.max, data.types='incidence', years=intervention.plot.years, facet.by='age', show.individual.sims = F)
simplot(simset.no.int, simset.all.max, data.types='incidence', years=intervention.plot.years, facet.by=c('age',"sex"), ages = "15+", show.individual.sims = F)

## Prevalence 
simplot(simset.no.int, simset.all.max, data.types='prevalence', years=intervention.plot.years, show.individual.sims = F)
simplot(simset.no.int, simset.all.max, data.types='prevalence', years=intervention.plot.years, facet.by='age', show.individual.sims = F)
simplot(simset.no.int, simset.all.max, data.types='prevalence', years=intervention.plot.years, facet.by=c('age',"sex"), ages = "15+", show.individual.sims = F)

## Cascade 
simplot(simset.no.int, simset.all.max, years=intervention.plot.years, data.types=c('awareness',"engagement","suppression"), proportion=T, show.individual.sims = F)
simplot(simset.no.int, simset.all.max, years=intervention.plot.years, facet.by=c('age','sex'), data.types='awareness', proportion=T, show.individual.sims = F)
simplot(simset.no.int, simset.all.max, years=intervention.plot.years, facet.by=c('age','sex'), data.types='engagement', proportion=T, show.individual.sims = F)
simplot(simset.no.int, simset.all.max, years=intervention.plot.years, facet.by=c('age','sex'), data.types='suppression', proportion=T, show.individual.sims = F)


##----------------------------##
##-- AGE DISTRIBUTION PLOTS --##
##----------------------------##

## Incidence
generate.age.distribution(full.results.array, 
                          outcome = "incidence", 
                          intervention.1 = "no.int", year.1 = "2040",
                          intervention.2 = "all.max", year.2 = "2040",
                          display = "table") # default display is figure
generate.age.distribution(results.array = full.results.array,
                          outcome="incidence",
                          intervention.1 = "no.int",year.1="2040",
                          intervention.2 = "all.max",year.2="2040",
                          percent=T)
generate.age.distribution(results.array = full.results.array,
                          outcome="incidence", 
                          intervention.1 = "no.int",year.1="2040",
                          intervention.2 = "all.max",year.2="2040",
                          percent=F) 

## Prevalence
generate.age.distribution(results.array = full.results.array,
                          outcome="prevalence",
                          intervention.1 = "no.int",year.1="2040",
                          intervention.2 = "all.max",year.2="2040",
                          percent=F) 

## Cascade ANNUAL TRANSITIONS: new diagnoses, annual engagement, annual suppression
generate.age.distribution(results.array = full.results.array,
                          outcome="diagnoses", 
                          intervention.1 = "no.int",year.1="2040",
                          intervention.2 = "all.max",year.2="2040",
                          percent=T) 
generate.age.distribution(results.array = full.results.array,
                          outcome="annual.engagement", 
                          intervention.1 = "no.int",year.1="2040",
                          intervention.2 = "all.max",year.2="2040",
                          percent=T) 
generate.age.distribution(results.array = full.results.array,
                          outcome="annual.suppression", 
                          intervention.1 = "no.int",year.1="2040",
                          intervention.2 = "all.max",year.2="2040",
                          percent=T) 

## Cascade STATES: awareness, engagement, suppression 
generate.age.distribution(results.array = full.results.array,
                          outcome="awareness", 
                          intervention.1 = "no.int",year.1="2040",
                          intervention.2 = "all.max",year.2="2040",
                          percent=T) 
generate.age.distribution(results.array = full.results.array,
                          outcome="engagement", 
                          intervention.1 = "no.int",year.1="2040",
                          intervention.2 = "all.max",year.2="2040",
                          percent=T) 
generate.age.distribution(results.array = full.results.array,
                          outcome="suppression", 
                          intervention.1 = "no.int",year.1="2040",
                          intervention.2 = "all.max",year.2="2040",
                          percent=T) 





# old plot code 
if(1==2){
    
    # OLD FIGURE 1 (FROM DRAFT V5)
    # Option 1: 
    simplot(simset.no.int, simset.all.max,
            years=intervention.plot.years, 
            show.individual.sims = F,
            for.paper = T) 
    # Option 1 (two rows): 
    simplot(simset.no.int, simset.all.max, 
            data.types='incidence', 
            ages = c("0-14","15-49","50 and over"),
            facet.by = "age",
            years=intervention.plot.years, 
            show.individual.sims = F,
            for.paper = T)
    simplot(simset.no.int, simset.all.max, 
            data.types='prevalence', 
            ages = c("0-14","15-49","50 and over"),
            facet.by = "age",
            years=intervention.plot.years, 
            show.individual.sims = F,
            for.paper = T)
    
    
    generate.age.distribution.2.column(full.results.array, 
                                       outcome="prevalence", 
                                       intervention.1 = "no.int",year.1="2025",
                                       intervention.2 = "no.int",year.2="2040",
                                       percent=F) + 
        scale_fill_discrete(labels=c("no.int/2025" = "No intervention, 2025","no.int/2040" = "No intervention, 2040"), 
                            name=NULL) 
    
    generate.age.distribution.2.column(full.results.array, 
                                       outcome="incidence", 
                                       intervention.1 = "no.int",year.1="2025",
                                       intervention.2 = "all.max",year.2="2040",
                                       percent=T) + 
        scale_fill_discrete(labels=c("no.int/2025" = "No intervention, 2025","all.max/2040" = "Combined interventions, 2040"), 
                            name=NULL)
    
    
    
    
    generate.age.distribution(full.results.array, 
                              outcome="engagement", 
                              intervention.1 = "no.int",year.1="2040",
                              intervention.2 = "all.max",year.2="2040") + 
        theme(panel.background = element_blank(), legend.position = "bottom") + # move legend to the bottom
        scale_y_continuous(labels = "percent",name = NULL) + # convert y axis labels to percent 
        # x axis label 
        scale_fill_manual(labels=c("no.int/2040" = "No intervention, 2040","all.max/2040" = "Combined interventions, 2040"), 
                          name=NULL, values=c("no.int/2040"=pal[1],"all.max/2040"=pal[2])) # change legend and color scheme 
}
