
# KDHS 2014, Tables 13.9.1 and 13.9.2
get.female.transmission.multipliers  = function(){

    data = data.frame(
        ages = c("15-19","20-24","25-29","30-39","40-49"),
        prop.multiple.partners=c(.01,.02,.013,.016,.009),
        condom.use.among.multiple.partners = c(.261,.434,.431,.480,.480) # no actual value for 40-49 so using 30-39
    )
    
    data$prop.at.risk = data$prop.multiple.partners*(1-data$condom.use.among.multiple.partners)
    data$multiplier = data$prop.at.risk/(data$prop.at.risk[data$ages=="30-39"])

    rv = array(data$multiplier,
               dim = length(data$ages),
               dimnames = list(data$ages))
    
    rv
}

get.male.transmission.multipliers  = function(){
    
    data = data.frame(
        ages = c("15-19","20-24","25-29","30-39","40-49","50-54"),
        prop.multiple.partners=c(.037,.167,.173,.146,.119,.114),
        condom.use.among.multiple.partners = c(.641,.702,.501,.282,.222,.145) 
    )
    
    data$prop.at.risk = data$prop.multiple.partners*(1-data$condom.use.among.multiple.partners)
    data$multiplier = data$prop.at.risk/(data$prop.at.risk[data$ages=="30-39"])
    
    rv = array(data$multiplier,
               dim = length(data$ages),
               dimnames = list(data$ages))
    
    rv
}



FEMALE.AGE.MULTIPLIERS =c(rep(0,length(get.age.brackets.in.range(lower = 0, upper = 15))),
                          rep(get.female.transmission.multipliers()[1],
                              length(get.age.brackets.in.range(lower = 15, upper = 20))),
                          rep(get.female.transmission.multipliers()[2],
                              length(get.age.brackets.in.range(lower = 20, upper = 25))),
                          rep(get.female.transmission.multipliers()[3],
                              length(get.age.brackets.in.range(lower = 25, upper = 30))),
                          rep(get.female.transmission.multipliers()[4],
                              length(get.age.brackets.in.range(lower = 30, upper = 40))),
                          rep(get.female.transmission.multipliers()[5],
                              length(get.age.brackets.in.range(lower = 40, upper = Inf))))

MALE.AGE.MULTIPLIERS =c(rep(0,length(get.age.brackets.in.range(lower = 0, upper = 15))),
                        rep(get.male.transmission.multipliers()[1],
                            length(get.age.brackets.in.range(lower = 15, upper = 20))),
                        rep(get.male.transmission.multipliers()[2],
                            length(get.age.brackets.in.range(lower = 20, upper = 25))),
                        rep(get.male.transmission.multipliers()[3],
                            length(get.age.brackets.in.range(lower = 25, upper = 30))),
                        rep(get.male.transmission.multipliers()[4],
                            length(get.age.brackets.in.range(lower = 30, upper = 40))),
                        rep(get.male.transmission.multipliers()[5],
                            length(get.age.brackets.in.range(lower = 40, upper = 50))),
                        rep(get.male.transmission.multipliers()[6],
                            length(get.age.brackets.in.range(lower = 50, upper = Inf))))
    
# KAIS 2012
female.multiple.partners = .017 + .005 # Figure 9.4c, 2+ partners
female.condom.use = .086 # Figure 9.5a
female.at.risk = female.multiple.partners*(1-female.condom.use)

male.multiple.partners = .099 + .043 # Figure 9.4c, 2+ partners
male.condom.use = .199  # Figure 9.5a
male.at.risk = male.multiple.partners*(1-male.condom.use)

female.to.male.multiplier = male.at.risk/female.at.risk # this multiplier for risk of male acquisition, so male.at.risk is the numerator, right?
# value = 5.656555
