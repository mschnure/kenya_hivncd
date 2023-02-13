
# incidence and prevalence have the same points
incidence.total.points = c(1990:2021)
incidence.age.points = rep(1990:2021,7)
incidence.age.sex.points = rep(c(2005,2010,2015,2017),2)
incidence.all.points = c(incidence.total.points,incidence.age.points,incidence.age.sex.points)
incidence.pre.2010 = length(incidence.all.points[incidence.all.points<2010])
incidence.2010.and.later = length(incidence.all.points[incidence.all.points>=2010])

# awareness and engagement have the same points
awareness.total.points = 2015:2021
awareness.age.sex.points = rep(c(2015:2021),2)
awareness.all.points = c(awareness.total.points,awareness.age.sex.points)
awareness.pre.2010 = length(awareness.all.points[awareness.all.points<2010])
awareness.2010.and.later = length(awareness.all.points[awareness.all.points>=2010])

# suppression has its own # of points
suppression.total.points = 2019:2020
suppression.age.sex.points = rep(c(2019:2020),2)
suppression.all.points = c(suppression.total.points,suppression.age.sex.points)
suppression.pre.2010 = length(suppression.all.points[suppression.all.points<2010])
suppression.2010.and.later = length(suppression.all.points[suppression.all.points>=2010])

incidence.weight = (incidence.pre.2010*(1/4))+(incidence.2010.and.later)
awareness.weight = (awareness.pre.2010*(1/4))+(awareness.2010.and.later)
suppression.weight = (suppression.pre.2010*(1/4))+(suppression.2010.and.later)

awareness.ratio = incidence.weight/awareness.weight # 6.785714
suppression.ratio = incidence.weight/suppression.weight # 23.75
