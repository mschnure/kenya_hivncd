
# KDHS 2014, Tables 13.9.1 and 13.9.2 - high-risk sex = multiple partners 
# KDHS 2008, Tables  13.7.1 and 13.7.2 - high-risk sex = non-marital/non-cohabiting partner
# KDHS 2003, Table 12.12 - high-risk sex = non-marital/non-cohabiting partner
get.transmission.multipliers.DHS  = function(sex,
                                             year){
    if(sex=="female"){
        if(year==2014){
            data = data.frame(
                ages = c("15-19","20-24","25-29","30-39","40-49"),
                prop.high.risk.sex=c(.01,.02,.013,.016,.009),
                condom.use.among.high.risk.sex = c(.261,.434,.431,.480,.480)) 
        } else if(year==2008){
            data = data.frame(
                ages = c("15-19","20-24","25-29","30-39","40-49"),
                prop.high.risk.sex=c(.154,.181,.125,.092,.087),
                condom.use.among.high.risk.sex = c(.41,.382,.318,.312,.264)) 
        } else if(year==2003){
            data = data.frame(
                ages = c("15-19","20-24","25-29","30-39","40-49"),
                prop.high.risk.sex=c(.467,.214,.139,.108,.109),
                condom.use.among.high.risk.sex = c(.234,.276,.258,.231,.149)) 
        }
    } else if(sex=="male"){
        if(year==2014){
            data = data.frame(
                ages = c("15-19","20-24","25-29","30-39","40-49"),
                prop.high.risk.sex=c(.037,.167,.173,.146,.119),
                condom.use.among.high.risk.sex = c(.641,.702,.501,.282,.222))
        } else if(year==2008){
            data = data.frame(
                ages = c("15-19","20-24","25-29","30-39","40-49"),
                prop.high.risk.sex=c(.243,.493,.31,.167,.072),
                condom.use.among.high.risk.sex = c(.547,.701,.654,.548,.474)) 
        } else if(year==2003){
            data = data.frame(
                ages = c("15-19","20-24","25-29","30-39","40-49"),
                prop.high.risk.sex=c(.971,.772,.353,.186,.096),
                condom.use.among.high.risk.sex = c(.413,.507,.518,.386,.5)) 
        }
    }
    
    data$prop.at.risk = data$prop.high.risk.sex*(1-data$condom.use.among.high.risk.sex)
    data$multiplier = data$prop.at.risk/(data$prop.at.risk[data$ages=="30-39"])
    
    rv = array(data$multiplier,
               dim = length(data$ages),
               dimnames = list(data$ages))
    
    rv
}

get.transmission.multipliers.older.cohort = function(sex){
    if(sex=="female"){
        data = data.frame(
            ages = c("40-49","50-59","60-69","70-79"),
            prop.high.risk.sex=c(.015,.015,.003,.003), 
            # data says 50s and 70s have 0% multiple partners; that won't work, so instead, using 40s value for 50s and 60s value for 70s
            condom.use.among.high.risk.sex = c(.165,.102,0,0)) 
    } else if(sex=="male"){
        data = data.frame(
            ages = c("40-49","50-59","60-69","70-79"),
            prop.high.risk.sex=c(.195,.197,.123,.083),
            condom.use.among.high.risk.sex = c(.2,.06,.056,.018)) 
    }
    
    data$prop.at.risk = data$prop.high.risk.sex*(1-data$condom.use.among.high.risk.sex)
    data$multiplier = data$prop.at.risk/(data$prop.at.risk[data$ages=="40-49"])
    
    rv = array(data$multiplier,
               dim = length(data$ages),
               dimnames = list(data$ages))
    
    rv
    
}

get.all.multipliers = function(sex,
                               year){
    multipliers = c(rep(0,length(get.age.brackets.in.range(lower = 0, upper = 15))),
                    rep(get.transmission.multipliers.DHS(sex=sex, year=year)["15-19"],
                        length(get.age.brackets.in.range(lower = 15, upper = 20))),
                    rep(get.transmission.multipliers.DHS(sex=sex, year=year)["20-24"],
                        length(get.age.brackets.in.range(lower = 20, upper = 25))),
                    rep(get.transmission.multipliers.DHS(sex=sex, year=year)["25-29"],
                        length(get.age.brackets.in.range(lower = 25, upper = 30))),
                    rep(get.transmission.multipliers.DHS(sex=sex, year=year)["30-39"],
                        length(get.age.brackets.in.range(lower = 30, upper = 40))),
                    rep(get.transmission.multipliers.DHS(sex=sex, year=year)["40-49"],
                        length(get.age.brackets.in.range(lower = 40, upper = 50))),
                    
                    # multiply 40-49 from DHS by 50-59 multiplier from older cohort
                    rep((get.transmission.multipliers.DHS(sex=sex, year=year)["40-49"]*
                             get.transmission.multipliers.older.cohort(sex=sex)["50-59"]),
                        length(get.age.brackets.in.range(lower = 50, upper = 60))), 
                    
                    # multiply 40-49 from DHS by 60-69 multiplier from older cohort
                    rep((get.transmission.multipliers.DHS(sex=sex, year=year)["40-49"]*
                             get.transmission.multipliers.older.cohort(sex=sex)["60-69"]),
                        length(get.age.brackets.in.range(lower = 60, upper = 70))), 
                    
                    # multiply 40-49 from DHS by 70-79 multiplier from older cohort
                    rep((get.transmission.multipliers.DHS(sex=sex, year=year)["40-49"]*
                             get.transmission.multipliers.older.cohort(sex=sex)["70-79"]),
                        length(get.age.brackets.in.range(lower = 70, upper = Inf)))
                    )
    names(multipliers) = get.age.brackets.in.range(lower=0,upper=Inf)
    multipliers
}

FEMALE.AGE.MULTIPLIERS.2003 = get.all.multipliers(sex="female",year=2003)
MALE.AGE.MULTIPLIERS.2003 = get.all.multipliers(sex="male",year=2003)

FEMALE.AGE.MULTIPLIERS.2008 = get.all.multipliers(sex="female",year=2008)
MALE.AGE.MULTIPLIERS.2008 = get.all.multipliers(sex="male",year=2008)

FEMALE.AGE.MULTIPLIERS.2014 = get.all.multipliers(sex="female",year=2014)
MALE.AGE.MULTIPLIERS.2014 = get.all.multipliers(sex="male",year=2014)

# REMOVE THIS - JUST FOR TESTING
# stop("MELISSA need to fix age sex multipliers")
# FEMALE.AGE.MULTIPLIERS.2003 = rep(1,17)
# MALE.AGE.MULTIPLIERS.2003 = FEMALE.AGE.MULTIPLIERS.2008 = MALE.AGE.MULTIPLIERS.2008 = FEMALE.AGE.MULTIPLIERS.2014 = MALE.AGE.MULTIPLIERS.2014 = FEMALE.AGE.MULTIPLIERS.2003

# plotting/testing
if(1==2){
    
    combined = round(cbind(FEMALE.AGE.MULTIPLIERS.2003,MALE.AGE.MULTIPLIERS.2003,
                           FEMALE.AGE.MULTIPLIERS.2008,MALE.AGE.MULTIPLIERS.2008,
                           FEMALE.AGE.MULTIPLIERS.2014,MALE.AGE.MULTIPLIERS.2014),2)
    
    colnames(combined) = c("F97","M97","F08","M08","F15","M15") # labeled with years they're actually applied in 
    combined
    
    age.transmission.multiplier.names = c("15.to.19",
                                          "20.to.29",
                                          "40.to.49",
                                          "50.and.over.0",
                                          "50.and.over.1",
                                          "50.and.over.2",
                                          "50.and.over.3"
                                          )
    
    
    cbind(age.transmission.multiplier.names)
    
    ages = get.age.brackets.in.range(lower=0,upper=Inf)
    ages[2] = "05-09"
    
    qplot(ages,FEMALE.AGE.MULTIPLIERS.2003,ylim=c(0,5))
    qplot(ages,FEMALE.AGE.MULTIPLIERS.2008,ylim=c(0,5))
    qplot(ages,FEMALE.AGE.MULTIPLIERS.2014,ylim=c(0,5))
    
    qplot(ages,MALE.AGE.MULTIPLIERS.2003,ylim=c(0,5))
    qplot(ages,MALE.AGE.MULTIPLIERS.2008,ylim=c(0,5))
    qplot(ages,MALE.AGE.MULTIPLIERS.2014,ylim=c(0,5))    
}



# sex multiplier, not using this
if(1==2){
    # KAIS 2012
    female.multiple.partners = .017 + .005 # Figure 9.4c, 2+ partners
    female.condom.use = .086 # Figure 9.5a
    female.at.risk = female.multiple.partners*(1-female.condom.use)
    
    male.multiple.partners = .099 + .043 # Figure 9.4c, 2+ partners
    male.condom.use = .199  # Figure 9.5a
    male.at.risk = male.multiple.partners*(1-male.condom.use)
    
    female.to.male.multiplier = male.at.risk/female.at.risk # this multiplier for risk of male acquisition, so male.at.risk is the numerator, right?
    # value = 5.656555 --> seems way too high; USING 1 INSTEAD 
}    

