
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





# disengagement - Lee et al
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

