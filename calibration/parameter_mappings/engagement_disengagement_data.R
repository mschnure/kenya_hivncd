
##-------------------------------------------##
##-- Disengagement - Lee et al 2018 AMPATH --##
##-------------------------------------------##

disengagement.p = 0.13
disengagement.rate = -log(1-disengagement.p)

# Relative risks of disengaging, both relative to CD4 below 350, off ART 
RRR.CD4.below.350.on.ART = .16
RRR.CD4.above.350.on.ART = .12

# Assume most in cohort were unsuppressed since only 12.8% taking ART at enrollment? 
suppressed.vs.unsuppressed.disengagement = RRR.CD4.above.350.on.ART/RRR.CD4.below.350.on.ART

disengagement.p.suppressed = disengagement.p*suppressed.vs.unsuppressed.disengagement
disengagement.rate.suppressed = -log(1-disengagement.p.suppressed)

disengagement.rate
disengagement.rate.suppressed


##------------------------------------------------##
##-- Engagement - estimated from AIDS info data --##
##------------------------------------------------##

# To get engagement proportion, take (# start art)/(# off art)
# But first, start with annual difference (will help solve for # start art below)

on.art = read.csv("data/raw_data/engagement_ART/Treatment cascade_People living with HIV receiving ART (#)_Population_ All ages.csv")
on.art = as.numeric(gsub(" ","",on.art[nrow(on.art),-1]))

years = c(2010:2021)
dim.names = list(year = years)

on.art = array(on.art,
               dim = sapply(dim.names, length),
               dimnames = dim.names)

annual.difference=0
# annual difference = (year2 on art) - (year1 on art) 
for(i in 1:length(years)-1){
    annual.difference[i] = on.art[i+1] - on.art[i]
    names(annual.difference)[i] = paste0(names(on.art[i+1]),"-",names(on.art[i]))
}


# Solving for start.art,
# annual.difference = start.art - stop.art
# annual.difference + stop.art = start.art
stop.art = on.art*disengagement.p
start.art = annual.difference + stop.art[-length(stop.art)]
names(start.art) = as.character(2011:2021)


# Solving for off.art
# on.art = total.plhiv*percent.on.art
# total.plhiv = on.art/percent.on.art
# (on.art + off.art) = (on.art/percent.on.art)
# off.art = (on.art/percent.on.art) - on.art
percent.on.art = read.csv("data/raw_data/engagement_ART/Treatment cascade_People living with HIV receiving ART (%)_Population_ All ages.csv")
percent.on.art = as.numeric(gsub(">","",percent.on.art[nrow(percent.on.art),c(2+3*c(0:11))]))

percent.on.art = array(percent.on.art/100,
                       dim = sapply(dim.names, length),
                       dimnames = dim.names)

off.art = (on.art/percent.on.art) - on.art



# ENGAGEMENT RATE = start.art/off.art
engagement.rate = start.art/off.art[-length(off.art)] 
engagement.anchor.year = 2000
engagement.years = as.numeric(names(engagement.rate))-engagement.anchor.year

df = data.frame(y=engagement.rate,
                x=engagement.years)
df$x2 = pmax(0,df$x-15)
df$x4 = pmax(0,df$x-15) 
df$x4[df$x>17]=0 # makes a 1 in 2016 and 2 in 2017, all the rest are zero 

fit1 = suppressWarnings(glm(y~x+x2, family=binomial,data=df)) # logistic with spline at 2017 (family=binomial, meaning link is logit)
fit2 = suppressWarnings(glm(y~x+x2+x4, family=binomial,data=df)) # three slopes: pre-2016, 2016-2017, post 2017 - but I don't know if this is true actually

fit.to.use = "fit2"

max.proportion = 0.95

if(fit.to.use=="fit1"){
    fit=fit1
    ENGAGEMENT.MODEL = list(intercept=fit$coefficients[1],
                            pre.universal.slope=fit$coefficients[2],
                            post.universal.slope=fit$coefficients[3],
                            anchor.year=engagement.anchor.year,
                            max.proportion=max.proportion)
}

if(fit.to.use=="fit2"){
    fit=fit2
    ENGAGEMENT.MODEL = list(intercept=fit$coefficients[1],
                            pre.universal.slope=fit$coefficients[2],
                            intermediate.slope.2016.2017=fit$coefficients[4],
                            post.universal.slope=fit$coefficients[3],
                            anchor.year=engagement.anchor.year,
                            max.proportion=max.proportion)
}

if(1==2){
    # For parameters table 
    exp(fit$coefficients[1] + fit$coefficients[2]*15)
    odds = exp(fit$coefficients[1] + fit$coefficients[2]*15)
    lower.odds = odds/4
    upper.odds = odds*4
    p = odds/(1+odds)
    # For table 
    # Probability of engaging, 2015
    c(p,(odds/(1+odds))/4,(odds/(1+odds))*4)
    # pre-2015 slope 
    pre.2015.OR = exp(fit$coefficients[2])
    c(pre.2015.OR, pre.2015.OR/4, pre.2015.OR*4)
    # 2016-2017 slope 
    universal.ART.OR = exp(fit$coefficients[2] +  fit$coefficients[3] + fit$coefficients[4])
    c(universal.ART.OR,universal.ART.OR/4,universal.ART.OR*4)
    # post-2017 slope 
    post.2017.OR = exp(fit$coefficients[2] +  fit$coefficients[3])
    c(post.2017.OR,post.2017.OR/4,post.2017.OR*4)
}



# Checking what functional form to use for smoothing
if(1==2){
    engagement.years = as.numeric(names(engagement.rate))
    qplot(names(engagement.rate),engagement.rate) + ylim(0,NA)
    
    df = data.frame(y=c(engagement.rate,rep(NA,30)),
                    x=c(engagement.years,2021:2050))
    
    # spline options: 
    df$x2 = pmax(0,df$x-2016) # years only at/after 2016 (pmax makes sure it isn't negative)
    df$x3 = pmax(0,df$x-2017) # same but for 2017
    df$x4 = pmax(0,df$x-2015) 
    df$x4[df$x>2017]=0 # makes a 1 in 2016 and 2 in 2017, all the rest are zero 
    df$x5 = as.numeric(df$x==2016 | df$x==2017) # rather than a separate slope from 2016-2017 as in previous, just a one-time bump
    
    fit1 = lm(y ~ x, data = df) # linear, no spline
    fit2 = lm(y~x+x2, data=df) # linear, spline 
    fit3 = glm(y~x+x2, family=binomial,data=df) # logit, spline - wouldn't have runaway exponential growth **
    fit4 = glm(y~x+x2, family=poisson,data=df) # log, spline - makes sense for a rate, but could have runaway growth
    fit5 = glm(y~x, family=binomial,data=df) # logit link without spline - limits runaway growth more than log link w/o spline, but not as good as spline
    fit6 = glm(y~x, family=poisson,data=df) # log link without spline - runaway growth
    fit7 = glm(y~x+x3, family=poisson,data=df) # with 2018 spline point, will be negative slope
    fit8 = glm(y~x+x4, family=binomial,data=df) # with separate slope in 2016/2017
    fit9 = glm(y~x+x2+x4, family=binomial,data=df) # one slope for early, another slope for 2016/2017, third slope for after
    fit10 = glm(y~x+x5, family=binomial,data=df) # bump in 2016/2017 
    
    fit=fit9
    
    predictions = predict(fit,newdata=df,type="response") # predict off of model (same as multiplying coefficients)
    qplot(c(df$x,df$x),c(df$y,predictions),color=rep(c("true","fitted"),each=length(predictions)),geom="line") + ylim(0,1)
    
    
    
}


