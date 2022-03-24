MODEL.AGE.CUTOFFS = c(0,10,15,20,25,50,Inf)

parse.age.brackets = function(age.cutoffs) {
        
        
        n.ages = length(age.cutoffs)-1
        
        
        rv = list(cutoffs = age.cutoffs,
                  lowers = age.cutoffs[-length(age.cutoffs)],
                  uppers = age.cutoffs[-1])
        
        rv$spans = rv$uppers-rv$lowers
        
        rv$labels = paste0(rv$lowers,"-", (rv$uppers-1))
        rv$labels[is.infinite(rv$uppers)] = paste0(rv$lowers[is.infinite(rv$uppers)], " and over")
        
        rv
        
}

MODEL.TO.SURVEILLANCE.AGE.MAPPING = list(
        "0-14" = c("0-9","10-14"),
        "10-19" = c("10-14", "15-19")
        # fill in the rest of this later
)


map.ages = function(to.map,
                    mapping = MODEL.TO.SURVEILLANCE.AGE.MAPPING,
                    map.to.options = unique(unlist(mapping))){
        
        unmappable = setdiff(to.map, union(names(mapping), map.to.options))
        
        if(length(unmappable)>0)
                stop(paste0("We cannot map these age brackets: ", paste0(unmappable,collapse = ", ")))
        
        
        rv = lapply(to.map, function(x){
                if(any(x==map.to.options))
                        x
                else
                        mapping[[x]]
        })
        
        rv
}




