################################################################################################
# Description: Code to map finer age brackets to wider ones; either by aggregating model age 
# groups to create surveillance age groups or vice versa
################################################################################################

# Functions 
#     1. parse.age.brackets 
#     2. get.age.brackets.in.range 
#     3. map.ages 
#     4. map.population.ages 
#     5. map.all.ages
#     6. map.ages.by.cutoffs


# UNAIDS surveillance age groups: '0-14','10-19','15-24','15-49','15+','50 and over'
# Population data age groups: '0-4', '5-9','10-14','15-19','20-24','25-29','30-34',
# '35-39','40-44','45-49','50-54','55-59','60-64','65-69',
# '70-74','75-79','80-84','85-89','90-94','95-99','100+','Total'
# Deaths age groups has same as population, but 95+ instead of 95-99 and 100+

MODEL.AGE.CUTOFFS = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,Inf)

# Using age cutoffs, creates age bracket labels
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

# Given a lower and upper value, returns all age brackets within range
get.age.brackets.in.range = function(age.cutoffs = MODEL.AGE.CUTOFFS,
                                     lower,
                                     upper){
    
    # e.g., if I pass 50 as lower and Inf as upper, it returns all the age brackets within 
    model.age.brackets = parse.age.brackets(age.cutoffs = age.cutoffs)
    
    mask = model.age.brackets$lowers>=lower & model.age.brackets$uppers<=upper
    
    rv = model.age.brackets$labels[mask]
    
    rv
    
}


## SPECIFIC VERSION ##
# Hard coded; created a generic version below but not really using it
MODEL.TO.SURVEILLANCE.AGE.MAPPING = list(
    "0-14" = c("0-4","5-9","10-14"),
    "10-19" = c("10-14","15-19"),
    "15-24" = c("15-19","20-24"),
    "15-49" = c("15-19","20-24","25-29","30-34","35-39","40-44","45-49"),
    "15+" = c("15-19", "20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59",
              "60-64","65-69","70-74","75-79","80 and over"),
    "50 and over" = c("50-54","55-59","60-64","65-69","70-74","75-79","80 and over")
)

POPULATION.AGE.MAPPING.HARD.CODE = list(
    "0-4"= c("0-4"),
    "5-9" = c("5-9"),
    "10-14" = c("10-14"),
    "15-19" = c("15-19"),
    "20-24" = c("20-24"),
    "25-29" = c("25-29"),
    "30-34" = c("30-34"),
    "35-39" = c("35-39"),
    "40-44" = c("40-44"),
    "45-49" = c("45-49"),
    "50-54" = c("50-54"),
    "55-59" = c("55-59"),
    "60-64" = c("60-64"),
    "65-69" = c("65-69"),
    "70-74" = c("70-74"),
    "75-79" = c("75-79"),
    "80 and over" = c("80-84","85-89","90-94","95-99","100 and over")
)

# Creates list of necessary age brackets based on mapping list; called in extract_data functions 
# For now, uses hard-coded/specific mappings; created generic functions below but these are not really being used fully 
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


# 1. Used when surveillance age brackets are finer than model age brackets (only for population data) 
# 2. Calls maps.ages.by.cutoffs once for each model age bracket
# 3. Currently only being used to create initial population and calculate death rates 
map.population.ages = function(data.manager,
                               data.type,
                               model.age.cutoffs = MODEL.AGE.CUTOFFS){
    
    parsed.model.ages = parse.age.brackets(model.age.cutoffs)
    
    surveillance.lowers = data.manager[[data.type]]$AGE.LOWERS
    surveillance.uppers = data.manager[[data.type]]$AGE.UPPERS
    surveillance.ages = data.manager[[data.type]]$AGES
    
    rv = list()
    
    for (i in 1:length(parsed.model.ages$lowers)){
        x = map.ages.by.cutoffs(target.lower = parsed.model.ages$lowers[i],
                                target.upper = parsed.model.ages$uppers[i],
                                from.lowers = surveillance.lowers,
                                from.uppers = surveillance.uppers)
        
        rv[[i]] = x$labels
        
    }
    
    names(rv) = parsed.model.ages$labels
    
    rv
}        



# 1. Used when model age brackets are finer than surveillance age brackets (everything except for population data) 
# 2. Calls map.ages.by.cutoffs once for each surveillance age bracket 
# 3. Currently not being used 
map.all.ages = function(data.manager,
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



# 1. Called once for each wider age bracket in map.population.ages and map.all.ages
# 2. Given each wider age bracket, returns the finer age brackets to include 
# (e.g., given age bracket of 15-24, would return 15-19, 20-24) 
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
}


# # GENERIC VERSION ##
# GENERIC.AGE.MAPPING = map.all.ages(data.manager = DATA.MANAGER,
#                                    data.type = "incidence") # testing this out with incidence - makes the same list as above
# 
# # for population age brackets, have to reverse - combine surveillance into model age brackets (see below)
# POPULATION.AGE.MAPPING = map.population.ages(data.manager = DATA.MANAGER,
#                                              data.type = "population")


