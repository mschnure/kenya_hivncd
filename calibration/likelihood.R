########################################################
# Description: Functions to create the likelihood object
########################################################

# Functions 
#     1. create.likelihood 
#     2. get.likelihood.elements.by.data.type
#     3. get.likelihood.elements.by.data.type.and.dimension 
#     4. compute.likelihood
#     5. create.likelihood.for.data.type



# incidence, prevalence, engagement, suppression, population, hiv mortality, awareness

# this will break for population right now because there is no denominator 
# could leave population as only observation error, no model error; or specify some model error (sqrt(n))

library("mvtnorm")



# this is the goal to be able to run 
if(1==2){
    lik = create.likelihood()
    lik(sim)
    lik.components = attr(lik,"components")
    sapply(lik.components,function(sub.lik){sub.lik(sim)})
}

# Calls individual "create.likelihood.for.data.type" functions for each data type
# Each data type function assembles the likelihood elements once (because it is time consuming - e.g., matrix M), 
# and then returns a function that computes the likelihood 
# SO, the output that create.likelihood() generates is a FUNCTION that can then be run on a sim to return the likelihood 
create.likelihood = function(data.manager=DATA.MANAGER,
                             years = 2010:2020,
                             total.weight = 1, # can make all of the other weights = total.weight * multiplier 
                             #incidence
                             incidence.years=years,
                             incidence.weight=1, 
                             incidence.obs.correlation=0.5,
                             #prevalence
                             prevalence.years=years,
                             prevalence.weight=1,
                             prevalence.obs.correlation=0.5,
                             #awareness
                             awareness.years=years,
                             awareness.weight=1,
                             awareness.obs.correlation=0.5,
                             #engagement
                             engagement.years=years,
                             engagement.weight=1,
                             engagement.obs.correlation=0.5,
                             #suppression
                             suppression.years=years,
                             suppression.weight=1,
                             suppression.obs.correlation=0.5,
                             #population
                             population.years=years,
                             population.weight=1,
                             population.obs.correlation=0.5,
                             #hiv.mortality
                             hiv.mortality.years=years,
                             hiv.mortality.weight=1,
                             hiv.mortality.obs.correlation=0.5
                             ){ 
    
    incidence.lik = create.likelihood.for.data.type(data.type = "incidence",
                                                    data.manager=data.manager,
                                                    years=incidence.years,
                                                    denominator.data.type="population", # later maybe make this out of the negative pop
                                                    obs.is.proportion=F,
                                                    weight=incidence.weight,
                                                    obs.correlation=incidence.obs.correlation)
    
    # Melissa update these to match incidence 
    prevalence.lik = create.likelihood.for.data.type(data.type = "prevalence",
                                                     data.manager=data.manager,
                                                     years=prevalence.years,
                                                     denominator.data.type="population", 
                                                     obs.is.proportion=F,
                                                     weight=prevalence.weight,
                                                     obs.correlation=prevalence.obs.correlation)
    
    awareness.lik = create.likelihood.for.data.type(data.type = "awareness",
                                                    data.manager=data.manager,
                                                    years=awareness.years,
                                                    denominator.data.type="prevalence",
                                                    obs.is.proportion=T, # awareness data is reported as a proportion 
                                                    weight=awareness.weight,
                                                    obs.correlation=awareness.obs.correlation)
    
    engagement.lik = create.likelihood.for.data.type(data.type = "engagement",
                                                     data.manager=data.manager,
                                                     years=engagement.years,
                                                     denominator.data.type="awareness", # engagement denominator = awareness
                                                     obs.is.proportion=T, # engagement data is reported as a proportion 
                                                     weight=engagement.weight,
                                                     obs.correlation=engagement.obs.correlation)
    
    suppression.lik = create.likelihood.for.data.type(data.type = "suppression",
                                                      data.manager=data.manager,
                                                      years=engagement.years,
                                                      denominator.data.type="awareness", # suppression denominator = awareness
                                                      obs.is.proportion=T, # suppression data is reported as a proportion 
                                                      weight=suppression.weight,
                                                      obs.correlation=suppression.obs.correlation)
    
    population.lik = create.likelihood.for.data.type(data.type = "population",
                                                     data.manager=data.manager,
                                                     years=population.years,
                                                     denominator.data.type=NULL, 
                                                     obs.is.proportion=F,
                                                     weight=population.weight,
                                                     obs.correlation=population.obs.correlation,
                                                     calculate.sds.from.ci=F,
                                                     use.total=F,
                                                     use.sex=F,
                                                     use.age=F,
                                                     use.age.sex=T) 
    
    hiv.mortality.lik = create.likelihood.for.data.type(data.type = "hiv.mortality",
                                                         data.manager=data.manager,
                                                         years=engagement.years,
                                                         denominator.data.type=NULL, # technically hiv mortality reported as a number
                                                         obs.is.proportion=F, 
                                                         weight=hiv.mortality.weight,
                                                         obs.correlation=hiv.mortality.obs.correlation)
    
    components = list(incidence=incidence.lik,
                      prevalence=prevalence.lik,
                      awareness=awareness.lik,
                      engagement=engagement.lik,
                      suppression=suppression.lik,
                      population=population.lik,
                      hiv.mortality=hiv.mortality.lik)
    
    rv = function(sim){ 
        
        sum(sapply(components, function(likelihood){likelihood(sim)})) # adding up each likelihood component, run on sim
    }
    
    attr(rv,"components") = components # attaching the components as an attribute to the function 
    
    # returns a function that can then be run on a sim to return the likelihood 
    rv
}

# Using a call to get.likelihood.elements.by.data.type, assembles the likelihood elements once per data type (slow), 
# then returns a FUNCTION that calls compute.likelihood, computes the likelihood on those elements (faster)
create.likelihood.for.data.type = function(data.type,
                                           data.manager,
                                           years,
                                           denominator.data.type,
                                           obs.is.proportion,
                                           weight,
                                           obs.correlation,
                                           calculate.sds.from.ci=T,
                                           use.total=T,
                                           use.sex=T,
                                           use.age=T,
                                           use.age.sex=T){
    
    # slower; so only have to call this function once and then creates the below function
    likelihood.elements = get.likelihood.elements.by.data.type(data.type=data.type,
                                                               years=years,
                                                               data.manager=data.manager,
                                                               obs.correlation=obs.correlation,
                                                               calculate.sds.from.ci=calculate.sds.from.ci,
                                                               use.total=use.total,
                                                               use.sex=use.sex,
                                                               use.age=use.age,
                                                               use.age.sex=use.age.sex
                                                               )
    
    function(sim){ # this gets called every single time we invoke likelihood on a simulation; which is why we pre-compute above it
        
        compute.likelihood(sim=sim,
                           numerator.data.type=data.type,
                           denominator.data.type=denominator.data.type,
                           M=likelihood.elements$M, 
                           obs.is.proportion=obs.is.proportion,
                           years=years,
                           obs=likelihood.elements$obs, 
                           obs.cov.mat=likelihood.elements$obs.cov.mat,
                           weight=weight)
        
    }
    
}



## Calls get.likelihood.elements.by.data.type.and.dimension once for each combo of dimensions (year, year/age, etc.)
## Will be called above (in create.likelihood.for.data.type) once per data type 
get.likelihood.elements.by.data.type = function(data.type,
                                                years,
                                                data.manager,
                                                obs.correlation,
                                                calculate.sds.from.ci=T,
                                                use.total=T,
                                                use.sex=T,
                                                use.age=T,
                                                use.age.sex=T){
    if(use.total)
        dim.1 = get.likelihood.elements.by.data.type.and.dimension(data.type = data.type,
                                                                   years = years,
                                                                   data.manager = data.manager,
                                                                   calculate.sds.from.ci=calculate.sds.from.ci,
                                                                   keep.dimensions = "year")
    else
        dim.1 = NULL
    
    if(use.age)
        dim.2 = get.likelihood.elements.by.data.type.and.dimension(data.type = data.type,
                                                                   years = years,
                                                                   data.manager = data.manager,
                                                                   calculate.sds.from.ci=calculate.sds.from.ci,
                                                                   keep.dimensions = c("year","age"))
    else 
        dim.2 = NULL
    
    if(use.sex)
        dim.3 = get.likelihood.elements.by.data.type.and.dimension(data.type = data.type,
                                                                   years = years,
                                                                   data.manager = data.manager,
                                                                   calculate.sds.from.ci=calculate.sds.from.ci,
                                                                   keep.dimensions = c("year","sex"))
    else
        dim.3 = NULL
    
    if(use.age.sex)
        dim.4 = get.likelihood.elements.by.data.type.and.dimension(data.type = data.type,
                                                                   years = years,
                                                                   data.manager = data.manager,
                                                                   calculate.sds.from.ci=calculate.sds.from.ci,
                                                                   keep.dimensions = c("year","age","sex"))
    else
        dim.4 = NULL

    # rename rv's to dim.1 
    rv = list()
    rv$M = rbind(dim.1$M, dim.2$M, dim.3$M, dim.4$M)
    rv$obs = c(dim.1$obs, dim.2$obs, dim.3$obs, dim.4$obs) 
    rv$obs.sds = c(dim.1$obs.sds, dim.2$obs.sds, dim.3$obs.sds,dim.4$obs.sds)
    rv$obs.year = c(dim.1$obs.year,dim.2$obs.year, dim.3$obs.year,dim.4$obs.year)
    rv$obs.dimensions = c(dim.1$obs.dimensions,dim.2$obs.dimensions, dim.3$obs.dimensions,dim.4$obs.dimensions)
    
    obs.corr.mat = sapply(1:length(rv$obs),function(i){
        sapply(1:length(rv$obs),function(j){
            
            # if the same observation (same year and same strata type) --> 1
            if(i==j) 
                1
            # if the dimensions (e.g., year/sex) & strata type (e.g., male) of the observations are the same, but they are different years
            else if(rv$obs.dimensions[i]==rv$obs.dimensions[j]) 
                obs.correlation 
            # if the observations are from different strata --> 0 
            else 
                0
        })
    }) 
    
    # SDs * correlation matrix
    rv$obs.cov.mat = (rv$obs.sds %*% t(rv$obs.sds)) * obs.corr.mat

    rv
}

# Lowest-level function 
# Assembles observed data, maps each observation to the stratifications present in the model, 
# Returns a list with: 
        # (1) M matrix mapping observations to model stratifications 
        # (2) vector of observations 
        # (3) vector of observation years
        # (4) dimensions of each observation (e.g., age, age/sex)
        # (5) vector of observation standard deviations 
# This will be called once for each dimension (year, year/age, year/sex, year/age/sex)
get.likelihood.elements.by.data.type.and.dimension = function(data.type,
                                                              years,
                                                              data.manager,
                                                              keep.dimensions,
                                                              calculate.sds.from.ci=T){
    
    #@ MELISSA PULL IN UPPERS/LOWER
    obs.data = get.surveillance.data(data.manager = data.manager,
                                     data.type = data.type,
                                     years = years,
                                     keep.dimensions = keep.dimensions)
    obs.data.long = melt(obs.data)
    
    # For data with upper/lower values (everything except pop), use these to calculate the SDs; only keep observations with all three values 
    if(calculate.sds.from.ci){
        obs.data.uppers = as.numeric(get.surveillance.data(data.manager = data.manager,
                                                           data.type = paste0(data.type,".lowers"),
                                                           years = years,
                                                           keep.dimensions = keep.dimensions))
        obs.data.lowers = as.numeric(get.surveillance.data(data.manager = data.manager,  # placeholder 
                                                           data.type = paste0(data.type,".uppers"),
                                                           years = years,
                                                           keep.dimensions = keep.dimensions))
        
        remove.mask = is.na(obs.data.long$value) | is.na(obs.data.uppers) | is.na(obs.data.lowers)
        obs.data.long = obs.data.long[!remove.mask,] # drop missing observations
        obs.data.uppers = obs.data.uppers[!remove.mask] 
        obs.data.lowers = obs.data.lowers[!remove.mask] 
    }

    else{
        obs.data.long = obs.data.long[!is.na(obs.data.long$value),]
    }

    
    if(nrow(obs.data.long)!=0){ ## if this dimension doesn't exist (e.g., no year/sex for incidence); returns a null list later
        
        dim.names = list(year = years,
                         age = parameters$AGES,
                         sex = parameters$SEXES)
        
        ## Fill in M matrix that maps observed data to stratifications present in model 
        ## Each row of M is a single observation, with 1's for each year/model age group/sex represented 
        M = NULL
        for(i in 1:nrow(obs.data.long)){
            years.to.pull = as.character(obs.data.long[i,"year"])
            ages.to.pull = obs.data.long[i,"age"]
            
            if(any(names(obs.data.long)!="age")){ # if no age strata, assign all ages
                ages.to.pull = "All ages"}
            
            ages.to.pull = MODEL.TO.SURVEILLANCE.AGE.MAPPING[[as.character(ages.to.pull)]] 
            
            if(any(names(obs.data.long)!="sex")){ # if no sex strata, assign both sexes
                sexes.to.pull = c("male","female")
            } else
                sexes.to.pull = as.character(obs.data.long[i,"sex"])
            
            M.row.x = array(0,
                            sapply(dim.names,length),
                            dimnames = dim.names)
            M.row.x[years.to.pull,ages.to.pull,sexes.to.pull] = 1
            M.row.x = as.numeric(M.row.x)
            
            M = rbind(M,M.row.x)
        }
        
        ## For non-population data, use SDS from confidence intervals
        if(calculate.sds.from.ci)
            obs.sds = as.numeric(obs.data.uppers-obs.data.lowers)/2/qnorm(.975)
        else
            obs.sds = sqrt(obs.data.long$value) # for population data, use square root of observed data
        
        if(is.null(obs.data.long$age) & is.null(obs.data.long$sex))
            obs.dimensions = rep("all",nrow(obs.data.long))
        else
            obs.dimensions = paste0(obs.data.long$age,"_",obs.data.long$sex)    
        
        
        rv = list(M=M,
                  obs=obs.data.long$value,
                  obs.year=obs.data.long$year,
                  obs.dimensions=obs.dimensions,
                  obs.sds=obs.sds)
    
    } else # return a null list if this dimension doesn't exist (e.g., year/sex for incidence)
        
        rv = list(M=NULL,
                  obs=NULL,
                  obs.year=NULL,
                  obs.dimensions=NULL,
                  obs.sds=NULL)
    
    rv 
    
}

# Function to actually compute the likelihood 
# Returns density of multivariate normal distribution with mean mu (model simulated value) and covariance matrix that includes 
# both model error and observation error 
compute.likelihood = function(sim,
                              numerator.data.type,
                              denominator.data.type,
                              M, # matrix that maps model strata to observed strata; pre-computed
                              obs.is.proportion,
                              years,
                              obs, # pre-computed vector of observations 
                              obs.cov.mat, # pre-computed variance-covariance matrix
                              weight){
    
    
    ## Model component## 
    y.star = as.numeric(extract.data(sim = sim,
                          data.type = numerator.data.type,
                          years=years,
                          keep.dimensions = c("year","sex","age")))
    
    if(!is.null(denominator.data.type)) # denominator will be null for population data type
        n.star = as.numeric(extract.data(sim = sim,
                                         data.type = denominator.data.type,
                                         years=years,
                                         keep.dimensions = c("year","sex","age")))
    # mean vector
    mu = M %*% y.star # mean vector with mappings incorporated, matrix multiplication
    
    if(!is.null(denominator.data.type)){
        tau = (y.star*(1-(y.star/n.star))) # covariance vector of np(1-p) 
        cov.mat = M %*% (tau * t(M)) # with mappings incorporated; dim are years x years
    }

    else
        cov.mat = 0 # for population data type only; no model error 
    # will probably have to inflate later using weights 
    
    
    if(obs.is.proportion){
        denominator = as.numeric(M %*% n.star)
        mu = mu/denominator
        D.inverse = diag(1/denominator) # matrix of inverse of diagonal matrix of denominators, can't divide matrices
        cov.mat = D.inverse %*% cov.mat %*% D.inverse
    }
    
    cov.mat = cov.mat + obs.cov.mat # add in observation error part 
    cov.mat = cov.mat*(1/weight) # higher weight --> smaller variance; (potentially downweight population likelihood)
    
    # compute and return the density of the multivariate normal
    rv = dmvnorm(x = obs,mean = mu,sigma = cov.mat,log = T)

    
}
