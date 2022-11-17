

df = data.frame(y=c(engagement.rate,rep(NA,10)),
                x=c(engagement.years+1,2021:2030))

fit1 = lm(y ~ x, data = df)


df$x2 = pmax(0,df$x-2016) # make sure it isn't negative 
df$x3 = pmax(0,df$x-2017) # make sure it isn't negative, 2017 spline point
df$x4 = pmax(0,df$x-2015) # makes a 1 in 2016 and 2 in 2017
df$x4[df$x>2017]=0
df$x5 = as.numeric(df$x==2016 | df$x==2017)

fit2 = lm(y~x+x2, data=df) # linear spline 

fit3 = glm(y~x+x2, family=binomial,data=df)
fit4 = glm(y~x+x2, family=poisson,data=df) # log - makes sense for a rate **
fit5 = glm(y~x, family=poisson,data=df) # without spline
fit6 = glm(y~x, family=binomial,data=df)
fit7 = glm(y~x+x3, family=poisson,data=df) # with 2017 spline point, will be negative slope
fit8 = glm(y~x+x4, family=binomial,data=df) # with just a bump in 2016/2017
fit9 = glm(y~x+x2+x4, family=binomial,data=df) # one slope for early, another slope for 2016/2017, third slope for after
fit10 = glm(y~x+x5, family=binomial,data=df)

fit=fit10

predictions = predict(fit,newdata=df,type="response") # predict off of model (same as multiplying coefficients)

qplot(c(df$x,df$x),c(df$y,predictions),color=rep(c("true","fitted"),each=length(predictions)),geom="line") + ylim(0,1)
