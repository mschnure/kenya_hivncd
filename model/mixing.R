get.sex.mixing.proportions = function(sex.to,
                                      age.to,
                                      sexes,
                                      sampled.parameters){
        
        # returns a vector of length(sexes) that sums to 1, which is the proportion of the to stratum's partners who are in each sex
        
        rv = rep(1, length(sexes))
        names(rv) = sexes
        
        rv[sex.to] = 0 # hard coding in only pair with other sex
        
        rv
        
}

get.age.mixing.proportions = function(sex.to,
                                      age.to,
                                      sex.from,
                                      ages,
                                      sampled.parameters){
        
        # returns a vector of length(ages) that sums to 1, which is the proportion of the to stratum's partners who are in each age 
        
        rv = rep(1/(length(ages)-2), length(ages)) # all age combos equally likely 
        rv[1:2] = 0 # first two age brackets don't pair 
        names(rv) = ages
        
        rv
        
}