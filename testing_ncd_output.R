states = apply(hiv.output.for.ncd$population,c(1,2),sum)[as.character(2009:2025),]
annual.engagement = c(0,apply(hiv.output.for.ncd$engagement,c(1),sum)[as.character(2010:2025)])
combined = cbind(states,annual.engagement)

difference = NULL
for(i in 1:(nrow(states)-1)){
    
    difference[i] = states[(i),3] - annual.engagement[i+1]
}
difference = c(0,difference)
combined = cbind(states,annual.engagement,difference)
combined



annual.incidence = c(0,apply(hiv.output.for.ncd$incidence,c(1),sum)[as.character(2010:2025)])
target.incidence = NULL
for(i in 1:(nrow(states)-1)){
    
    target.incidence[i] = annual.incidence[i+1]/states[(i),1]
}
