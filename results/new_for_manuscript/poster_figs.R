
## FIGURE 2 

# INCIDENCE
jpeg(file=paste0("results/poster/Figure2.A.jpeg"), width = 2500,height = 1500,res=400)
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

jpeg(file=paste0("results/poster/Figure2.B.jpeg"), width = 2500,height = 1500,res=400)
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

jpeg(file=paste0("results/poster/Figure2.C.jpeg"), width = 2500,height = 1500,res=400)
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

jpeg(file=paste0("results/poster/Figure2.D.jpeg"), width = 2500,height = 1500,res=400)
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
jpeg(file=paste0("results/poster/Figure2.E.jpeg"), width = 2500,height = 1500,res=400)
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

jpeg(file=paste0("results/poster/Figure2.F.jpeg"), width = 2500,height = 1500,res=400)
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

jpeg(file=paste0("results/poster/Figure2.G.jpeg"), width = 2500,height = 1500,res=400)
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

jpeg(file=paste0("results/poster/Figure2.H.jpeg"), width = 2500,height = 1500,res=400)
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