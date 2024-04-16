
calibration.colors = rev(hue_pal()(2))
pal = c(brewer.pal(n=12,"Paired")[2],brewer.pal(n=12,"Paired")[5],brewer.pal(n=12,"Paired")[4]) # use ALPHA = 0.8
alpha = 0.8
intervention.plot.years = 2000:2040

## FIGURE 2 

# INCIDENCE
jpeg(file=paste0("results/new_for_manuscript/Figure2.A.jpeg"), width = 2500,height = 1500,res=400)
simplot(simset.all.max,simset.no.int,
        data.types = "incidence",
        ages = c("All ages"),
        facet.by = "age",
        years=intervention.plot.years, 
        show.individual.sims = F,
        for.paper = T,
        ncol=4) +
    theme(strip.text.x = element_blank(),
          text = element_text(size = 20),
          legend.position = "none",
          panel.border = element_blank(),
          axis.title.x = element_blank())+
    scale_fill_manual(labels=c("1" = "No intervention","2" = "Full intervention"), 
                      name=NULL,
                      values=calibration.colors) + 
    scale_color_manual(labels=c("1" = "No intervention","2" = "Full intervention"), 
                      name=NULL,
                      values=c(calibration.colors,"black")) + 
    #scale_y_continuous(labels = label_number(suffix = "M", scale = 1e-6),name = NULL, limits = c(0,NA)) + # millions
    scale_y_continuous(labels = label_number(suffix = "K", scale = 1e-3),name = NULL, limits = c(0,NA)) + # thousands
    geom_vline(xintercept = 2025, linetype="dashed",alpha=0.5) 
dev.off() 

jpeg(file=paste0("results/new_for_manuscript/Figure2.B.jpeg"), width = 2500,height = 1500,res=400)
simplot(simset.all.max,simset.no.int,
        data.types = "incidence",
        ages = c("0-14"),
        facet.by = "age",
        years=intervention.plot.years, 
        show.individual.sims = F,
        for.paper = T,
        ncol=4) +
    theme(strip.text.x = element_blank(),
          text = element_text(size = 20),
          legend.position = "none",
          panel.border = element_blank(),
          axis.title.x = element_blank())+
    scale_fill_manual(labels=c("1" = "No intervention","2" = "Full intervention"), 
                      name=NULL,
                      values=calibration.colors) + 
    scale_color_manual(labels=c("1" = "No intervention","2" = "Full intervention"), 
                       name=NULL,
                       values=c(calibration.colors,"black")) + 
    #scale_y_continuous(labels = label_number(suffix = "M", scale = 1e-6),name = NULL, limits = c(0,NA)) + # millions
    scale_y_continuous(labels = label_number(suffix = "K", scale = 1e-3),name = NULL, limits = c(0,NA)) + # thousands
    geom_vline(xintercept = 2025, linetype="dashed",alpha=0.5) 
dev.off() 

jpeg(file=paste0("results/new_for_manuscript/Figure2.C.jpeg"), width = 2500,height = 1500,res=400)
simplot(simset.all.max,simset.no.int,
        data.types = "incidence",
        ages = c("15-49"),
        facet.by = "age",
        years=intervention.plot.years, 
        show.individual.sims = F,
        for.paper = T,
        ncol=4) +
    theme(strip.text.x = element_blank(),
          text = element_text(size = 20),
          legend.position = "none",
          panel.border = element_blank(),
          axis.title.x = element_blank())+
    scale_fill_manual(labels=c("1" = "No intervention","2" = "Full intervention"), 
                      name=NULL,
                      values=calibration.colors) + 
    scale_color_manual(labels=c("1" = "No intervention","2" = "Full intervention"), 
                       name=NULL,
                       values=c(calibration.colors,"black")) + 
    #scale_y_continuous(labels = label_number(suffix = "M", scale = 1e-6),name = NULL, limits = c(0,NA)) + # millions
    scale_y_continuous(labels = label_number(suffix = "K", scale = 1e-3),name = NULL, limits = c(0,NA)) + # thousands
    geom_vline(xintercept = 2025, linetype="dashed",alpha=0.5) 
dev.off() 

jpeg(file=paste0("results/new_for_manuscript/Figure2.D.jpeg"), width = 2500,height = 1500,res=400)
simplot(simset.all.max,simset.no.int,
        data.types = "incidence",
        ages = c("50 and over"),
        facet.by = "age",
        years=intervention.plot.years, 
        show.individual.sims = F,
        for.paper = T,
        ncol=4) +
    theme(strip.text.x = element_blank(),
          text = element_text(size = 20),
          legend.position = "none",
          panel.border = element_blank(),
          axis.title.x = element_blank())+
    scale_fill_manual(labels=c("1" = "No intervention","2" = "Full intervention"), 
                      name=NULL,
                      values=calibration.colors) + 
    scale_color_manual(labels=c("1" = "No intervention","2" = "Full intervention"), 
                       name=NULL,
                       values=c(calibration.colors,"black")) + 
    #scale_y_continuous(labels = label_number(suffix = "M", scale = 1e-6),name = NULL, limits = c(0,NA)) + # millions
    scale_y_continuous(labels = label_number(suffix = "K", scale = 1e-3),name = NULL, limits = c(0,NA)) + # thousands
    geom_vline(xintercept = 2025, linetype="dashed",alpha=0.5) 
dev.off() 

# PREVLANCE
jpeg(file=paste0("results/new_for_manuscript/Figure2.E.jpeg"), width = 2500,height = 1500,res=400)
simplot(simset.all.max,simset.no.int,
        data.types = "prevalence",
        ages = c("All ages"),
        facet.by = "age",
        years=intervention.plot.years, 
        show.individual.sims = F,
        for.paper = T,
        ncol=4) +
    theme(strip.text.x = element_blank(),
          text = element_text(size = 20),
          legend.position = "none",
          panel.border = element_blank(),
          axis.title.x = element_blank())+
    scale_fill_manual(labels=c("1" = "No intervention","2" = "Full intervention"), 
                      name=NULL,
                      values=calibration.colors) + 
    scale_color_manual(labels=c("1" = "No intervention","2" = "Full intervention"), 
                       name=NULL,
                       values=c(calibration.colors,"black")) + 
    scale_y_continuous(labels = label_number(suffix = "M", scale = 1e-6),name = NULL, limits = c(0,NA)) + # millions
    #scale_y_continuous(labels = label_number(suffix = "K", scale = 1e-3),name = NULL, limits = c(0,NA)) + # thousands
    geom_vline(xintercept = 2025, linetype="dashed",alpha=0.5) 
dev.off() 

jpeg(file=paste0("results/new_for_manuscript/Figure2.F.jpeg"), width = 2500,height = 1500,res=400)
simplot(simset.all.max,simset.no.int,
        data.types = "prevalence",
        ages = c("0-14"),
        facet.by = "age",
        years=intervention.plot.years, 
        show.individual.sims = F,
        for.paper = T,
        ncol=4) +
    theme(strip.text.x = element_blank(),
          text = element_text(size = 20),
          legend.position = "none",
          panel.border = element_blank(),
          axis.title.x = element_blank())+
    scale_fill_manual(labels=c("1" = "No intervention","2" = "Full intervention"), 
                      name=NULL,
                      values=calibration.colors) + 
    scale_color_manual(labels=c("1" = "No intervention","2" = "Full intervention"), 
                       name=NULL,
                       values=c(calibration.colors,"black")) + 
    #scale_y_continuous(labels = label_number(suffix = "M", scale = 1e-6),name = NULL, limits = c(0,NA)) + # millions
    scale_y_continuous(labels = label_number(suffix = "K", scale = 1e-3),name = NULL, limits = c(0,NA)) + # thousands
    geom_vline(xintercept = 2025, linetype="dashed",alpha=0.5) 
dev.off() 

jpeg(file=paste0("results/new_for_manuscript/Figure2.G.jpeg"), width = 2500,height = 1500,res=400)
simplot(simset.all.max,simset.no.int,
        data.types = "prevalence",
        ages = c("15-49"),
        facet.by = "age",
        years=intervention.plot.years, 
        show.individual.sims = F,
        for.paper = T,
        ncol=4) +
    theme(strip.text.x = element_blank(),
          text = element_text(size = 20),
          legend.position = "none",
          panel.border = element_blank(),
          axis.title.x = element_blank())+
    scale_fill_manual(labels=c("1" = "No intervention","2" = "Full intervention"), 
                      name=NULL,
                      values=calibration.colors) + 
    scale_color_manual(labels=c("1" = "No intervention","2" = "Full intervention"), 
                       name=NULL,
                       values=c(calibration.colors,"black")) + 
    scale_y_continuous(labels = label_number(suffix = "M", scale = 1e-6),name = NULL, limits = c(0,NA)) + # millions
    #scale_y_continuous(labels = label_number(suffix = "K", scale = 1e-3),name = NULL, limits = c(0,NA)) + # thousands
    geom_vline(xintercept = 2025, linetype="dashed",alpha=0.5) 
dev.off() 

jpeg(file=paste0("results/new_for_manuscript/Figure2.H.jpeg"), width = 2500,height = 1500,res=400)
simplot(simset.all.max,simset.no.int,
        data.types = "prevalence",
        ages = c("50 and over"),
        facet.by = "age",
        years=intervention.plot.years, 
        show.individual.sims = F,
        for.paper = T,
        ncol=4) +
    theme(strip.text.x = element_blank(),
          text = element_text(size = 20),
          legend.position = "none",
          panel.border = element_blank(),
          axis.title.x = element_blank())+
    scale_fill_manual(labels=c("1" = "No intervention","2" = "Full intervention"), 
                      name=NULL,
                      values=calibration.colors) + 
    scale_color_manual(labels=c("1" = "No intervention","2" = "Full intervention"), 
                       name=NULL,
                       values=c(calibration.colors,"black")) + 
    #scale_y_continuous(labels = label_number(suffix = "M", scale = 1e-6),name = NULL, limits = c(0,NA)) + # millions
    scale_y_continuous(labels = label_number(suffix = "K", scale = 1e-3),name = NULL, limits = c(0,NA)) + # thousands
    geom_vline(xintercept = 2025, linetype="dashed",alpha=0.5) 
dev.off() 

## FIGURE 3
jpeg(file=paste0("results/new_for_manuscript/Figure3.jpeg"), width = 2000,height = 1500,res=200)
generate.age.distribution(full.results.array, 
                          outcome="prevalence", 
                          intervention.1 = "no.int",year.1="2025",
                          intervention.2 = "no.int",year.2="2040",
                          intervention.3 = "all.max",year.3="2040",
                          percent=F,
                          sexes = c("female","male"),
                          plot.limits=c(0,200000)) + 
    scale_fill_manual(labels = c("no.int/2025" = "2025",
                                 "no.int/2040" = "Status quo, 2040",
                                 "all.max/2040" = "Full intervention, 2040"), 
                      values=alpha(c("no.int/2025" = pal[1],
                                     "no.int/2040" = pal[2],
                                     "all.max/2040" = pal[3]),alpha), # change legend and color scheme  
                      name=NULL) +
    theme(text = element_text(size = 20),
          axis.title.y = element_text(colour = "grey37"))+
    labs(title = NULL,subtitle = NULL) +
    ylab(label = "Number of people living with HIV")+
    guides(x =  guide_axis(angle = 45))
dev.off()



    # Figure S4 - incidence as #s instead of % 
jpeg(file=paste0("results/new_for_manuscript/FigureS4_A.jpeg"), width = 2000,height = 1500,res=200)
generate.age.distribution(full.results.array, 
                          outcome="incidence", 
                          intervention.1 = "no.int",year.1="2025",
                          intervention.2 = "no.int",year.2="2040",
                          intervention.3 = "all.max",year.3="2040",
                          percent=F,
                          sexes = c("female","male"),
                          plot.limits = c(0,NA)) + 
    scale_fill_manual(labels = c("no.int/2025" = "2025",
                                 "no.int/2040" = "No intervention, 2040",
                                 "all.max/2040" = "Full intervention, 2040"), 
                      values=alpha(c("no.int/2025" = pal[1],
                                     "no.int/2040" = pal[2],
                                     "all.max/2040" = pal[3]),alpha), # change legend and color scheme  
                      name=NULL) +
    theme(text = element_text(size = 20),
          legend.position = "none")+
    labs(title = NULL,subtitle = NULL) +
    guides(x =  guide_axis(angle = 45))
dev.off()

jpeg(file=paste0("results/new_for_manuscript/FigureS4_B.jpeg"), width = 2000,height = 1500,res=200)
generate.age.distribution(full.results.array, 
                          outcome="incidence", 
                          intervention.1 = "no.int",year.1="2025",
                          intervention.2 = "no.int",year.2="2040",
                          intervention.3 = "all.max",year.3="2040",
                          percent=F,
                          sexes = c("female"),
                          plot.limits = c(0,NA)) + 
    scale_fill_manual(labels = c("no.int/2025" = "2025",
                                 "no.int/2040" = "No intervention, 2040",
                                 "all.max/2040" = "Full intervention, 2040"), 
                      values=alpha(c("no.int/2025" = pal[1],
                                     "no.int/2040" = pal[2],
                                     "all.max/2040" = pal[3]),alpha), # change legend and color scheme  
                      name=NULL) +
    theme(text = element_text(size = 20),
          legend.position = "none")+
    labs(title = NULL,subtitle = NULL) +
    guides(x =  guide_axis(angle = 45))
dev.off()

jpeg(file=paste0("results/new_for_manuscript/FigureS4_C.jpeg"), width = 2000,height = 1500,res=200)
generate.age.distribution(full.results.array, 
                          outcome="incidence", 
                          intervention.1 = "no.int",year.1="2025",
                          intervention.2 = "no.int",year.2="2040",
                          intervention.3 = "all.max",year.3="2040",
                          percent=F,
                          sexes = c("male"),
                          plot.limits = c(0,NA)) + 
    scale_fill_manual(labels = c("no.int/2025" = "2025",
                                 "no.int/2040" = "No intervention, 2040",
                                 "all.max/2040" = "Full intervention, 2040"), 
                      values=alpha(c("no.int/2025" = pal[1],
                                     "no.int/2040" = pal[2],
                                     "all.max/2040" = pal[3]),alpha), # change legend and color scheme  
                      name=NULL) +
    theme(text = element_text(size = 20),
          legend.position = "none")+
    labs(title = NULL,subtitle = NULL) +
    guides(x =  guide_axis(angle = 45))
dev.off()



# Figure S6 - formatting updates
jpeg(file=paste0("results/new_for_manuscript/FigureS6_A.jpeg"), width = 2000,height = 1500,res=200)
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
                      name=NULL) +
    theme(text = element_text(size = 20),
          legend.position = "none")+
    labs(title = NULL,subtitle = NULL) +
    guides(x =  guide_axis(angle = 45))
dev.off()

jpeg(file=paste0("results/new_for_manuscript/FigureS6_B.jpeg"), width = 2000,height = 1500,res=200)
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
                      name=NULL) +
    theme(text = element_text(size = 20),
          legend.position = "none")+
    labs(title = NULL,subtitle = NULL) +
    guides(x =  guide_axis(angle = 45))
dev.off()

# Figure S7 - formatting updates
jpeg(file=paste0("results/new_for_manuscript/FigureS7_A.jpeg"), width = 2000,height = 1500,res=200)
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
                      name=NULL) +
    theme(text = element_text(size = 20),
          legend.position = "none")+
    labs(title = NULL,subtitle = NULL) +
    guides(x =  guide_axis(angle = 45))
dev.off()

jpeg(file=paste0("results/new_for_manuscript/FigureS7_B.jpeg"), width = 2000,height = 1500,res=200)
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
                      name=NULL) +
    theme(text = element_text(size = 20),
          legend.position = "none")+
    labs(title = NULL,subtitle = NULL) +
    guides(x =  guide_axis(angle = 45))
dev.off()

# POPULATION CALIBRATION - MIGHT NOT USE 
jpeg(file=paste0("results/new_for_manuscript/general_pop_calibration.jpeg"), width = 2000,height = 1500,res=200)
simplot(simset.no.int, 
        data.types='population', years=1990:2030, facet.by='age', show.individual.sims = F, for.paper = T) + 
    theme(strip.text.x = element_text(),
          text = element_text(size = 10),
          legend.position = "none",
          panel.border = element_blank(),
          axis.title.x = element_blank())
    scale_y_continuous(labels = label_number(suffix = "M", scale = 1e-6),name = NULL, limits = c(0,NA))  # millions
    # scale_y_continuous(labels = label_number(suffix = "K", scale = 1e-3),name = NULL, limits = c(0,NA))  # thousands
dev.off()
