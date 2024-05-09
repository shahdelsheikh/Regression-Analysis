setwd("C:/Users/shahd/Desktop/Regression Project")
mult<-read.csv("EDIT.csv")
sample<-read.csv("Sample.csv")

#descriptive statistics

summary(sample$HDI)
summary(sample$Life.exp)
summary(sample$EYOS)
summary(sample$MYOS)
summary(sample$GNI)
summary(sample$GNI.rank)
summary(sample$HDI.rank.2020)
summary(sample$HDI.rank.2021)
var(sample$HDI)
var(sample$Life.exp)
var(sample$EYOS)
var(sample$MYOS)
var(sample$GNI)
var(sample$GNI.rank)
var(sample$HDI.rank.2020)
var(sample$HDI.rank.2021)

#scatterplots

plot(sample$Life.exp~sample$EYOS, ylab= "Life Expentancy at Birth (Years)", xlab="Expected Years of Schooling (Years)")
plot(sample$Life.exp~sample$HDI, xlab="HDI index (Value)",ylab= "Life Expentancy at Birth (Years)" )
plot(sample$Life.exp~sample$MYOS, xlab="Mean Years of Schooling (Years)", ylab= "Life Expentancy at Birth (Years)")
plot(sample$Life.exp~sample$GNI, xlab="Gross National Income per Capita (Dollars)",ylab= "Life Expentancy at Birth (Years)")
plot(sample$Life.exp~sample$GNI.rank, xlab= "GNI per Capita Rank Minus HDI Rank",ylab= "Life Expentancy at Birth (Years)")
plot(sample$Life.exp~sample$HDI.rank.2020, xlab="HDI Rank 2020",ylab= "Life Expentancy at Birth (Years)")
plot(sample$Life.exp~sample$HDI.rank.2021, xlab="HDI Rank 2021",ylab= "Life Expentancy at Birth (Years)")

#multicollinearity

library(GGally)
ggpairs(mult)
#so check significance of HDI value and MYOS to choose which one to remove
cor.test(sample$Life.exp,sample$HDI)
cor.test(sample$Life.exp, sample$MYOS)
#the smaller cor will be removed so we removed MYOS
cor(sample$Life.exp, sample$HDI)
cor(sample$Life.exp,sample$MYOS)

#########remove MYOS and HDI ranks

Model<-lm(Life.exp~ HDI+EYOS+GNI+GNI.rank, data=sample)
Model
summary(Model)


############ forward selection

#null model for full model
noc<-read.csv("noc.csv")
null.model<-lm(Life.exp~1, data=noc)
summary(null.model)
forward.model1<-step(null.model,direction = "forward",scope=formula(Model))
summary(forward.model1)


########backward elimanation

#for full model
backward.model1<-step(Model,direction = "backward")
summary(backward.model1)

######### stepwise
null.model<-lm(Life.exp~1, data=noc)
stepwisemodel<-step(null.model,direction = "both", scope=formula(Model))
summary(stepwisemodel)

#checking model assumptions for this full model (random part)

library(olsrr)
library(jtools)
library(moments)
library(lmtest)
my_confidence<-0.95
ols_plot_cooksd_chart(Model)
plot(Model,pch=18, col="red", which=c(4))
plot(Model,pch=18, col="red")

hist(residuals(Model), col="steelblue")
plot(fitted(Model), residuals(Model))
abline(h = 0, lty = 2)

resid(Model)
X<-resid(Model)
qqnorm(X)
qqline(X)

####### log trans and it didn't work

logModel<-lm(formula=log(Life.exp)~ HDI+EYOS+GNI+GNI.rank, data=sample)
logModel
resid(logModel)
K<-resid(logModel)
plot(fitted(logModel), residuals(logModel))
abline(h = 0, lty = 2)
qqnorm(K)
qqline(K)
summary(logModel)


###### try the sqrt model (it didn't work)
sqrtModel<-lm(formula=sqrt(Life.exp)~ HDI+EYOS+GNI+GNI.rank, data=sample)
sqrtModel
resid(sqrtModel)
U<-resid(sqrtModel)
plot(fitted(sqrtModel), residuals(sqrtModel))
abline(h = 0, lty = 2)
qqnorm(U)
qqline(U)
hist(residuals(sqrtModel), col="steelblue")
summary(sqrtModel)



###### normality assumption 
#histogram is somehow symmetric
#try to remove the outlier
# after removing outliers , there was generated 3 more outliers
#do box plots
install.packages("tidyverse")
library(tidyverse)


######## model without outliers
ModelwRemovedOutliers<-lm(Life.exp~ HDI+EYOS+GNI+GNI.rank, data=noc)
ModelwRemovedOutliers
summary(ModelwRemovedOutliers)
anova(ModelwRemovedOutliers)
resid(ModelwRemovedOutliers)
Q<-resid(ModelwRemovedOutliers)
plot(fitted(ModelwRemovedOutliers), residuals(ModelwRemovedOutliers))
abline(h = 0, lty = 2)
qqnorm(Q)
qqline(Q)
hist(residuals(ModelwRemovedOutliers), col="steelblue")
