add.sim.ids = function(simset,
                       id.prefix=""){
    
    simset@simulations = lapply(1:simset@n.sim,function(i){
        
        sim = simset@simulations[[i]]
        sim$id = paste0(id.prefix,i)
        
        sim
    })
    
    simset
}

add.sim.intervention.id = function(simset,
                                   intervention.id){
    
    simset@simulations = lapply(1:simset@n.sim,function(i){
        
        sim = simset@simulations[[i]]
        sim$intervention.id = intervention.id
        
        sim
    })
    
    simset
    
}

#e.g. baseline
simset.test = add.sim.ids(simset.5)
simset.test = add.sim.intervention.id(simset.test,
                                      intervention.id = "no.int")

# intervention simset
# simset = run.intervention...
# simset.test = add.sim.intervention.id(simset.test,
#                                       intervention.id = "XXX")

