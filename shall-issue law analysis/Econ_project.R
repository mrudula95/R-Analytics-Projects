
library(foreign)
library(plm)

guns<-read.csv("shall-issue law analysis/guns.csv")
summary(guns)

head(guns)
library(caret)
library(dplyr)
 
# created dummy varibles for shall variable having laws in the state from initial years, 
# which doesn't have any law and laws which are introduced in between

guns$law_from_start<-0
guns$law_first_half<-0
guns$law_second_half<-0
guns$no_law<-0

## the arrays have state-ids depending on when the law was introduced there 
guns[which(guns$stateid %in% c(18,33,50,53)),14]<-1
guns[which(guns$stateid %in% c(12,23,38,46,49)),15]<-1
guns[which(guns$stateid %in% c(2,4,5,13,16,21,22,28,30,32,37,40,41,42,45,47,48,51,54,56)),16]<-1
guns[which(guns$stateid %in% c(1,6,8,9,10,11,15,17,19,20,24,25,26,27,29,31,34,35,36,39,44,55)),17]<-1


#guns[which(!(guns$stateid %in% c(1,6,8,9,10,11,15,17,18,19,20,24,25,26,27,29,31,33,34,35,36,39,44,50,53,55))),15]<-1

## we plot histograms to check crime trends
hist(guns$vio)
guns$vio<-(log(guns$vio))

hist(guns$mur)
guns$mur<-(log(guns$mur))

hist(guns$rob)
guns$rob<-(log(guns$rob))

coplot(shall ~ year|stateid, type="l", data=guns) # Lines
coplot(shall ~ year|stateid, type="b", data=guns)

# partitioning the data as per presence of shall-issue law
d1<-guns[which(guns$law_in_between==1),]
d2<-guns[which(guns$no_law==1),]

##Pooled ols model for both datasets
model1<-plm(mur ~ incarc_rate + pb1064 + pw1064 + pm1029 + avginc + density  ,index=c("stateid","year"),model="pooling",data=d1)
model2<-plm(mur ~ incarc_rate + pb1064 + pw1064 + pm1029 + avginc + density ,index=c("stateid","year"),model="pooling",data=d2)
summary(model1)
summary(model2)

## fixed effect within entity
model3<-plm(mur ~ incarc_rate + pb1064 + pw1064 + pm1029 + avginc + density  ,index=c("stateid","year"),model="within",data=d1)
model4<-plm(mur ~ incarc_rate + pb1064 + pw1064 + pm1029 + avginc + density ,index=c("stateid","year"),model="within",data=d2)
summary(model3)
summary(model4)


d1<-plm(mur ~ incarc_rate + pb1064 + pw1064 + pm1029 + avginc + I(avginc*avginc) + density + shall  ,index=c("stateid","year"), model="within", data=guns)
summary(d1)

d2<-plm(mur ~ incarc_rate + pb1064 + pw1064 + pm1029 + avginc + I(avginc*avginc) + density + shall  ,index=c("stateid","year"), model="pooling", data=guns)
summary(d2)

d3<-plm(mur ~ incarc_rate +log(pb1064) + I(pb1064*pb1064) + I(pb1064*pb1064*pb1064) + pw1064 + pm1029 + avginc + I(avginc*avginc) + density + shall  ,index=c("stateid","year"), model="pooling", data=guns)
summary(d3)

plot(guns$pb1064,d1$residuals)
plot(guns$pw1064,d1$residuals)

plot(guns$pb1064,d2$residuals)
plot(guns$pw1064,d2$residuals)

# Qestion : How does the varience of Violance in a state having laws in between campare to the vaience of violence in a
# state with no laws differ. If there is a difference then there is a heteroskedasticity and this can be tested using 
# Goldfeld Quandt test

# F stat =
#  var d2/var d1 = 8665.2 / 4308.7
#  = 2.011094

# F Critical
# qf(.95, df1=568, df2=499) 
# 1.154002

# As F stat is greater than Fcritical we reject the null hypotheses that means there is a heteroskedasticity 
# (Refer page no 308 from Book)


context3<- plm.data(guns,index=c("stateid","year"))
names(context3)

plotmeans(mur ~ stateid, main="Heterogeineity across States", data=context3)
plotmeans(vio ~ stateid, main="Heterogeineity across States", data=context3)
plotmeans(rob ~ stateid, main="Heterogeineity across States", data=context3)

plotmeans(mur ~ year, main="Heterogeineity across States", data=context3)
plotmeans(vio ~ year, main="Heterogeineity across States", data=context3)
plotmeans(rob ~ year, main="Heterogeineity across States", data=context3)


context3$avgcrime<-(context3$vio+context3$mur+context3$rob)/3
hist(context3$avgcrime)
hist(log(context3$avgcrime))

random_model <- plm(mur ~ incarc_rate + I(incarc_rate*law_from_start) + I(incarc_rate*law_in_between) + pb1064 + pw1064 + pm1029 + avginc + density  ,model="random",data=context3)
within_model <- plm(mur ~ incarc_rate + I(incarc_rate*law_from_start) + I(incarc_rate*law_in_between) + pb1064 + pw1064 + pm1029 + avginc + density + law_from_start + law_in_between ,model="within",data=context3)
FE<-plm(mur ~ incarc_rate + I(incarc_rate*law_from_start) + I(incarc_rate*law_in_between) + pb1064 + pw1064 + pm1029 + avginc + density  ,model="within",data=context3)
pooling_model <- plm(mur ~ incarc_rate + I(incarc_rate*law_from_start) + I(incarc_rate*law_in_between) + pb1064 + pw1064 + pm1029 + avginc + density  ,model="pooling",data=context3)
pb1064sq<-context3$pb1064*context3$pb1064
pooling_model <- plm(mur ~ incarc_rate + I(incarc_rate*law_from_start) + I(incarc_rate*law_in_between) + pb1064sq + pw1064 + pm1029 + avginc + density  ,model="pooling",data=context3)

summary(FE)
summary(pooling_model)
summary(within_model)
summary(random_model)


plot(xi, yi, 
     xlab="GDP", 
     ylab="edu exp", 
     type = "p")
abline(model8)
plot(model8$residuals)


#write.csv(guns,"D:/Applied Econometrics/Data_sets/guns.csv")

library(corrplot)
M <- cor(as.matrix(context3[,3:16]))
corrplot(M, method = "number")

coplot(shall ~ year|stateid, type="l", data=context3) # Lines
coplot(shall ~ year|stateid, type="b", data=context3)
library(ggplot2)
library(car)
scatterplot(shall~year|stateid, boxplots=FALSE, smooth=TRUE, reg.line=FALSE, data=context3)


library(gplots)

plotmeans(mur ~ stateid, main="Heterogeineity across States", data=context3)
# plotmeansdraw a 95%
#confidence interval
#around the means

#detach("package:gplots")

plotmeans(mur ~ year, main="Heterogeineity across Years", data=context3)

fixef(within_model)
mean(fixef(within_model))

summary(pooling_model)
summary((within_model))


library(car)
scatterplot(shall~year|stateid, boxplots=FALSE, smooth=TRUE, reg.line=FALSE, data=head(context3,46))

coplot(shall ~ year|stateid, type="l", data=context3) # Lines
coplot(shall ~ year|stateid, type="b", data=context3)


#####  Hausman Test #####

# It basically tests whether the unique errors (ui) are correlated with the regressors,
# the null hypothesis is they are not.


phtest(within_model, random_model)

# Hausman Test
# 
# data:  shall ~ vio + mur + rob + incarc_rate + pb1064 + context3$pw1064 +  ...
# chisq = 171.07, df = 10, p-value < 2.2e-16
# alternative hypothesis: one model is inconsistent

summary(fixed.time)

BIC(within_model)
BIC(within_model)

pFtest(fixed.time, within_model)
 
# F test for individual effects
# 
# data:  shall ~ vio + +factor(year) + mur + rob + incarc_rate + pb1064 +  ...
# F = 4.1453, df1 = 22, df2 = 1090, p-value = 6.461e-10
# alternative hypothesis: significant effects

##### Checking Serial Correlation ####

pbgtest(within_model)

# Breusch-Godfrey/Wooldridge test for serial correlation in panel models
# 
# data:  shall ~ vio + mur + rob + incarc_rate+pb1064+context3$pw1064+pm1029 + pop + avginc + density + law_from_start + law_in_between +     no_law
# chisq = 700.05, df = 23, p-value < 2.2e-16
# alternative hypothesis: serial correlation in idiosyncratic errors


### Check for Heteroskedasticity ###
library(lmtest)
bptest(fixed.time, data = context3, studentize=F)

# Breusch-Pagan test
# 
# data:  fixed.time
# BP = 379.12, df = 34, p-value < 2.2e-16

# There is a Heteroskedasticty in the model
# we can remove that using HAC estimators

coeftest(fixed.time, vcovHC)




.