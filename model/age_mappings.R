################################################################################################
################
#Description: Code to combine model age brackets and map them to surveillance age brackets
################
################################################################################################

# Model age cutoffs are created so that they can combine into the surveillance age groups
# UNAIDS surveillance age groups: '0-14','10-19','15-24','15-49','15+','50 and over'
# Population data age groups: '0-4', '5-9','10-14','15-19','20-24','25-29','30-34',
                            # '35-39','40-44','45-49','50-54','55-59','60-64','65-69',
                            # '70-74','75-79','80-84','85-89','90-94','95-99','100+','Total'
# Deaths age groups has same as population, but 95+ instead of 95-99 and 100+

MODEL.AGE.CUTOFFS = c(0,10,15,20,25,30,40,50,60,70,80,Inf)

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


# Combine model age groups to create surveillance age groups 
# Could either hard code another mapping for the population data, or create a generic - see below 
MODEL.TO.SURVEILLANCE.AGE.MAPPING = list(
        "0-14" = c("0-9","10-14"),
        "10-19" = c("10-14", "15-19"),
        "15-24" = c("15-19", "20-24"),
        "15-49" = c("15-19", "20-24","25-29","30-39","40-49"),
        "15+" = c("15-19", "20-24","25-29","30-39","40-49","50-59","60-69","70-79","80 and over"),
        "50 and over" = "50-59","60-69","70-79","80 and over"
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

map.ages.by.cutoffs = function(target.lower, # surveillance lowers
                               target.upper, # surveillance uppers
                               from.lowers, # model lowers
                               from.uppers){ # model uppers
        
        # returns a set of indices which is a subset of 1:length(from.lowers)
        
        #e.g., model age bracket is 0-10; from.lowers are 0,5,10, etc.
        # so indices would be the first two from.lowers
        # or, if it's 10-20, the lowers would be 3:4
        

    
}


