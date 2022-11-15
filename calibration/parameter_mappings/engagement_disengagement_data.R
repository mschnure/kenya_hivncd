
##-------------------------------------------##
##-- Disengagement - Lee et al 2018 AMPATH --##
##-------------------------------------------##

disengagement.p = 0.13
disengagement.rate = -log(1-disengagement.p)

# Relative risks of disengaging, both relative to CD4 below 350, off ART 
RRR.CD4.below.350.on.ART = .16
RRR.CD4.above.350.on.ART = .12

# Assume most in cohort were unsuppressed since only 12.8% taking ART at enrollment? 
suppressed.vs.unsuppressed.disengagement = RRR.CD4.above.350.on.ART/RRR.CD4.below.350.on.ART

disengagement.p.suppressed = disengagement.p*suppressed.vs.unsuppressed.disengagement
disengagement.rate.suppressed = -log(1-disengagement.p.suppressed)

disengagement.rate
disengagement.rate.suppressed


##------------------------------------------------##
##-- Engagement - estimated from AIDS info data --##
##------------------------------------------------##

# To get engagement proportion, take (# start art)/(# off art)
# But first, start with annual difference (will help solve for # start art below)

on.art = read.csv("data/raw_data/engagement_ART/Treatment cascade_People living with HIV receiving ART (#)_Population_ All ages.csv")
on.art = as.numeric(gsub(" ","",on.art[nrow(on.art),-1]))

years = c(2010:2021)
dim.names = list(year = years)

on.art = array(on.art,
               dim = sapply(dim.names, length),
               dimnames = dim.names)

# annual difference = (year2 on art) - (year1 on art) 
for(i in 1:length(years)-1){
    annual.difference[i] = on.art[i+1] - on.art[i]
    names(annual.difference)[i] = paste0(names(on.art[i+1]),"-",names(on.art[i]))
}


# Solving for start.art,
# annual.difference = start.art - stop.art
# annual.difference + stop.art = start.art
stop.art = on.art*disengagement.p
start.art = annual.difference + stop.art[-length(stop.art)]


# Solving for off.art
# on.art = total.plhiv*percent.on.art
# total.plhiv = on.art/percent.on.art
# (on.art + off.art) = (on.art/percent.on.art)
# off.art = (on.art/percent.on.art) - on.art
percent.on.art = read.csv("data/raw_data/engagement_ART/Treatment cascade_People living with HIV receiving ART (%)_Population_ All ages.csv")
percent.on.art = as.numeric(gsub(">","",percent.on.art[nrow(percent.on.art),c(2+3*c(0:11))]))

percent.on.art = array(percent.on.art/100,
               dim = sapply(dim.names, length),
               dimnames = dim.names)

off.art = (on.art/percent.on.art) - on.art

# Engaged prop = start.art/off.art
engagement.p = start.art/off.art[-length(off.art)]
engagement.rate = -log(1-engagement.p)





# old scratch code
if(1==2){
    sample.diagnosed.pop = 10000 # NEED THIS
    engaged.among.diagnosed.prop = 0.893
    engaged.within.3.months.among.engaged = .794
    
    engaged.within.3.months.among.diagnosed = engaged.among.diagnosed.prop*engaged.within.3.months.among.engaged
    engaged.after.3.months.among.diagnosed = 1-engaged.within.3.months.among.diagnosed
    
    engaged = engaged.among.diagnosed.prop*sample.diagnosed.pop
    unengaged.not.new.diagnoses = (1-engaged.among.diagnosed.prop)*sample.diagnosed.pop # assume all unengaged are disengaged
    
    new.diagnoses = 1000 # NEED THIS - have to have some sense of what % of the unengaged pool is newly diagnosed 
    new.diagnoses.not.yet.linked = new.diagnoses/4 # assuming three months to link
    
    new.diagnoses.prop.of.all.unengaged = new.diagnoses.not.yet.linked/(new.diagnoses.not.yet.linked + unengaged.not.new.diagnoses)
    
    new.diagnoses.engagement.prop = .794 # 79.4% enrolled within 3 months - BUT THIS IS AMONG THOSE ENROLLED, NOT DIAGNOSED
    new.diagnoses.engagement.rate = -log(1-new.diagnoses.engagement.prop)/(1/4) # -log(1-p)/t; time is 1/4 of a year
    
    # Lee et al, average of disengaged 2 and disengaged 3? 
    unengaged.engagement.prop = (.02+.05)/2
    unengaged.engagement.rate = -log(1-unengaged.engagement.prop)
    
    weighted.engagement.rate = (new.diagnoses.engagement.rate*new.diagnoses.prop.of.all.unengaged) + 
        (unengaged.engagement.rate*(1-new.diagnoses.prop.of.all.unengaged))
    
    weighted.engagement.prop = 1-exp(-(weighted.engagement.rate))
    
}
