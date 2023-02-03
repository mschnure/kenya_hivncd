
scale.calibration.data = function(data.type){
    
    dim.names.1 = dimnames(DATA.MANAGER[[data.type]]$age)
    
    if(!grepl("hiv.mortality",data.type)) # no age/sex for hiv.mortality
        dim.names.2 = dimnames(DATA.MANAGER[[data.type]]$age.sex)
    
    # First, scale 0-14 and 15+ to match total 
    scaling.factor.1 = DATA.MANAGER[[data.type]]$age[,c("All ages")]/rowSums(DATA.MANAGER[[data.type]]$age[,c("0-14","15+")])
    
    NEW.0.TO.14 = DATA.MANAGER[[data.type]]$age[,"0-14"]*scaling.factor.1
    NEW.15.AND.OVER = DATA.MANAGER[[data.type]]$age[,"15+"]*scaling.factor.1
    
    # Next, scale 15+ male/female to new, scaled 15+ 
    if(!grepl("hiv.mortality",data.type)) {
        scaling.factor.2 = c(NEW.15.AND.OVER[dim.names.2$year]/apply(DATA.MANAGER[[data.type]]$age.sex,c(1:2),sum))
        
        NEW.15.AND.OVER.BY.SEX = DATA.MANAGER[[data.type]]$age.sex*scaling.factor.2
    }

    
    # Next, scale 15-49 and 50+ to new, scaled 15+ 
    scaling.factor.3 = NEW.15.AND.OVER/rowSums(DATA.MANAGER[[data.type]]$age[,c("15-49","50 and over")])
    
    NEW.15.TO.49 = DATA.MANAGER[[data.type]]$age[,"15-49"]*scaling.factor.3
    NEW.50.AND.OVER = DATA.MANAGER[[data.type]]$age[,"50 and over"]*scaling.factor.3
    
    
    # Next, scale 10-19 and 15-24 using average of scaling factors (?)
    mean.scaling.factor = apply(cbind(scaling.factor.1,scaling.factor.3),1,mean)
    
    NEW.10.TO.19 = DATA.MANAGER[[data.type]]$age[,"10-19"]*mean.scaling.factor
    NEW.15.TO.24 = DATA.MANAGER[[data.type]]$age[,"15-24"]*mean.scaling.factor
    
    rv = list()
    rv$total = DATA.MANAGER[[data.type]]$total
    rv$subgroup = DATA.MANAGER[[data.type]]$subgroup
    
    rv$age = cbind(NEW.0.TO.14,
                   NEW.10.TO.19,
                   NEW.15.TO.24,
                   NEW.15.TO.49,
                   NEW.15.AND.OVER,
                   NEW.50.AND.OVER,
                   DATA.MANAGER[[data.type]]$age[,"All ages"])
    dimnames(rv$age) = dimnames(DATA.MANAGER[[data.type]]$age)
    
    rv$age.subgroup = DATA.MANAGER[[data.type]]$age.subgroup
    
    if(!grepl("hiv.mortality",data.type)) {
        rv$age.sex = NEW.15.AND.OVER.BY.SEX    
    }
    
    
    rv
}




if(1==2){
    
    # Checking 0-14 and 15+ scaling
    comparison.1 = cbind(rowSums(DATA.MANAGER[[data.type]]$age[,c("0-14","15+")]),
                         DATA.MANAGER[[data.type]]$age[,c("All ages")])
    scaling.factor.1 = comparison.1[,2]/comparison.1[,1]
    comparison.1 = cbind(comparison.1,scaling.factor.1)
    dimnames(comparison.1) = list(year = dim.names.1$year,
                                  age = c("0-14, 15+","All ages","scaling factor"))
    comparison.1.scaled=cbind(rowSums(cbind(NEW.0.TO.14,NEW.15.AND.OVER)),DATA.MANAGER[[data.type]]$age[,"All ages"])
    round(rowSums(cbind(NEW.0.TO.14,NEW.15.AND.OVER)))==DATA.MANAGER[[data.type]]$age[,"All ages"]
    
    # Checking 15+ by sex scaling
    comparison.2 = cbind(apply(DATA.MANAGER[[data.type]]$age.sex,c(1:2),sum),
                         NEW.15.AND.OVER[dim.names.2$year])
    scaling.factor.2 = comparison.2[,2]/comparison.2[,1]
    comparison.2 = cbind(comparison.2,scaling.factor.2)
    dimnames(comparison.2) = list(year = dim.names.2$year,
                                  age = c("15+, male/female combined","15+","scaling factor"))
    comparison.2.scaled = cbind(apply(NEW.15.AND.OVER.BY.SEX,c(1:2),sum),NEW.15.AND.OVER[dim.names.2$year])
    round(apply(NEW.15.AND.OVER.BY.SEX,c(1:2),sum))==round(NEW.15.AND.OVER[dim.names.2$year])
    
    
    # Checking 15-49 and 50 and over scaling
    comparison.3 = cbind(rowSums(DATA.MANAGER[[data.type]]$age[,c("15-49","50 and over")]),
                         NEW.15.AND.OVER)
    
    scaling.factor.3 = comparison.3[,2]/comparison.3[,1]
    comparison.3 = cbind(comparison.3,scaling.factor.3)
    dimnames(comparison.3) = list(year = dim.names.1$year,
                                  age = c("15-49, 50 and over","15+","scaling factor"))
    comparison.3.scaled=cbind(rowSums(cbind(NEW.15.TO.49,NEW.50.AND.OVER)),NEW.15.AND.OVER)
    round(rowSums(cbind(NEW.15.TO.49,NEW.50.AND.OVER)))==round(NEW.15.AND.OVER)
    
    NEW.PREVALENCE.ARRAY/DATA.MANAGER[[data.type]]$age
    range(NEW.PREVALENCE.ARRAY/DATA.MANAGER[[data.type]]$age)
    
    
    table(new.hiv.mortality.lowers$age<new.hiv.mortality$age)
    table(new.hiv.mortality.uppers$age>new.hiv.mortality$age)
    
    table(new.prevalence.lowers$age<new.prevalence$age)
    table(new.prevalence.uppers$age>new.prevalence$age)
    
    table(new.prevalence.lowers$age.sex<new.prevalence$age.sex)
    table(new.prevalence.uppers$age.sex>new.prevalence$age.sex)
    
    table(new.incidence.lowers$age<new.incidence$age)
    table(new.incidence.uppers$age>new.incidence$age)
    
    table(new.incidence.lowers$age.sex<new.incidence$age.sex)
    table(new.incidence.uppers$age.sex>new.incidence$age.sex)
    
}

