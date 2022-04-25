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


## Combine model age groups to create surveillance age groups ##

## SPECIFIC VERSION ##
MODEL.TO.SURVEILLANCE.AGE.MAPPING = list(
        "0-14" = c("0-9","10-14"),
        "10-19" = c("10-14", "15-19"),
        "15-24" = c("15-19", "20-24"),
        "15-49" = c("15-19", "20-24","25-29","30-39","40-49"),
        "15+" = c("15-19", "20-24","25-29","30-39","40-49","50-59","60-69","70-79","80 and over"),
        "50 and over" = c("50-59","60-69","70-79","80 and over")
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


## GENERIC VERSION ##
GENERIC.AGE.MAPPING = map.all.ages(data.type = "incidence") # testing this out with incidence - makes the same list as above 

# Loops through below function (map.ages.by.cutoffs) for ALL surveillance brackets
# Returns a list where each item of the list is a surveillance age bracket and the model age brackets that go into it
map.all.ages = function(data.manager = DATA.MANAGER,
                        data.type,
                        model.age.cutoffs = MODEL.AGE.CUTOFFS){
        
        parsed.model.ages = parse.age.brackets(model.age.cutoffs)
        
        surveillance.lowers = data.manager[[data.type]]$AGE.LOWERS
        surveillance.uppers = data.manager[[data.type]]$AGE.UPPERS
        surveillance.ages = data.manager[[data.type]]$AGES

        rv = list()
        
        for (i in 1:length(surveillance.lowers)){
                x = map.ages.by.cutoffs(target.lower = surveillance.lowers[i],
                                         target.upper = surveillance.uppers[i],
                                         from.lowers = parsed.model.ages$lowers,
                                         from.uppers = parsed.model.ages$uppers)
                
                rv[[i]] = x$labels
                
        }
        
        names(rv) = surveillance.ages
        
        rv
}

# for population age brackets, have to reverse - combine surveillance into model age brackets (see below)
POPULATION.AGE.MAPPING = map.population.ages(data.type = "population") 

#REVERSE OF ABOVE FUNCTION, because population age brackets are FINER than model age brackets
# Loops through below function (map.ages.by.cutoffs) for all MODEL brackets
# Returns a list where each item of the list is a MODEL age bracket and the POPULATION surveillance age brackets that go into it
map.population.ages = function(data.manager = DATA.MANAGER,
                               data.type,
                               model.age.cutoffs = MODEL.AGE.CUTOFFS){
        
        parsed.model.ages = parse.age.brackets(model.age.cutoffs)
        
        surveillance.lowers = data.manager[[data.type]]$AGE.LOWERS
        surveillance.uppers = data.manager[[data.type]]$AGE.UPPERS
        surveillance.ages = data.manager[[data.type]]$AGES
        
        rv = list()
        
        for (i in 1:length(parsed.ages$lowers)){
                x = map.ages.by.cutoffs(target.lower = parsed.model.ages$lowers[i],
                                        target.upper = parsed.model.ages$uppers[i],
                                        from.lowers = surveillance.lowers,
                                        from.uppers = surveillance.uppers)
                
                rv[[i]] = x$labels
                
        }
        
        names(rv) = parsed.model.ages$labels
        
        rv
}        

## Given ONE surveillance age bracket, returns the model age brackets to include
## E.g., given surveillance age bracket of 15-24, would return 15-19, 20-24
## this is reversed for population data because it is finer than model brackets
## --> e.g., given MODEL bracket of 0-9, returns pop brackets 0-4, 5-9
map.ages.by.cutoffs = function(target.lower, # surveillance lower
                               target.upper, # surveillance upper
                               from.lowers, # model lowers
                               from.uppers){ # model uppers
        
        rv = list()
        rv$lowers = from.lowers[from.lowers>=target.lower & from.lowers<target.upper]
        rv$uppers = from.uppers[from.uppers>target.lower & from.uppers<=target.upper]
        
        rv$spans = rv$uppers-rv$lowers
        
        rv$labels = paste0(rv$lowers,"-", (rv$uppers-1))
        rv$labels[is.infinite(rv$uppers)] = paste0(rv$lowers[is.infinite(rv$uppers)], " and over")
        
        rv

    # (Notes from before writing this: 
        # returns a set of indices which is a subset of 1:length(from.lowers)
        
        # e.g., model age bracket is 0-10; from.lowers are 0,5,10, etc.
        # so indices would be the first two from.lowers
        # or, if it's 10-20, the lowers would be 3:4)
}


