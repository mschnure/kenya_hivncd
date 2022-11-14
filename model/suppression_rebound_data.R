
# Suppression - Njuguna et al
# prop suppressed at 18 months
suppressed.p = 3227/(3227+741)
suppressed.r = -log(1-suppressed.p)/(18/12)

suppressed.p.12.months = 1-exp(-suppressed.r)

# Viral rebound - Maina et al 
rate.of.viral.rebound.per.100.person.months = mean(c(3.9,0.7,0.89))

rate.of.viral.rebound.one.year = (rate.of.viral.rebound.per.100.person.months/100)*12

viral.rebound.p.one.year = 1-exp(-(rate.of.viral.rebound.one.year))
viral.rebound.p.one.year
