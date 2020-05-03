rm(list=ls())
library(tidyverse)
library(ggplot2)
library(forcats)
library(dplyr)
library(glmnet)
library(fastDummies)
require(randomForest)
library(caret)
setwd('/Users/wangxiuqi/Desktop/da/capstone')
data<-read.csv('vgsales-12-4-2019-short.csv')
summary(data)


data=data[,-c(1,2,12,13,14,15)]
data=data[complete.cases(data$Global_Sales),]
data=data[,-8]
#year
data=data[complete.cases(data$Year),]
summary(data$Year)
data$Year=as.factor(data$Year)
unique(data$Year)
#score
hist(data$Critic_Score)
sum(is.na(data$Critic_Score))#48281
is.na(data$Critic_Score)
data$Critic_Score[is.na(data$Critic_Score)] <- 'n'
data$Critic_Score[data$Critic_Score!='n'] <- 'y'
#data$Critic_Score[is.na(data$Critic_Score)] <- median(data$Critic_Score, na.rm=TRUE)

hist(data$User_Score)
sum(is.na(data$User_Score))
data$User_Score[is.na(data$User_Score)] <- 'n'
data$User_Score[data$User_Score!='n'] <- 'y'

#genre
sum(is.na(data$Genre))
plot(data$Genre)
data=data[data$Genre!='Education',]
data=data[data$Genre!='Sandbox',]
data=data[data$Genre!='Board Game',]
plot(data$Genre)
data$Genre=as.factor(as.character(data$Genre))
summary(data$Genre)

plot(data$ESRB_Rating)
data$ESRB_Rating<- as.character(data$ESRB_Rating)
data$ESRB_Rating[data$ESRB_Rating == ''] <- 'NA'
data$ESRB_Rating<- as.factor(data$ESRB_Rating)
summary(data$ESRB_Rating)
#E everyone
#E10 everyone 10+
#T teen
#ao adults only
#rp rating pending
#mature 17+
#ka kids to adults
#ec early childhood



plot(data$Platform)
nrow(count(data,Platform))
c=count(data,Platform)
c=c[c$n<50,]
data$Platform=fct_collapse(data$Platform, 
             Others = as.character(c$Platform))
plot(data$Platform)
data$Platform=as.factor(as.character(data$Platform))
summary(data$Platform)

plot(data$Publisher)
nrow(count(data,Publisher))
c=count(data,Publisher)
c=c[c$n<100,]
name=as.character(c$Publisher)
data$Publisher=fct_collapse(data$Publisher,Others=name)
data$Publisher=as.factor(as.character(data$Publisher))
summary(data$Publisher)


plot(data$Developer)
unique(data$Developer)
c=count(data,Developer)
c=c[c$n<100,]
name=as.character(c$Developer)
data$Developer=fct_collapse(data$Developer,Others=name)
data$Developer=as.factor(as.character(data$Developer))
summary(data$Developer)

#ship=data[complete.cases(data$Total_Shipped),]
#ship=ship[,-9]
#sale=data[complete.cases(data$Global_Sales),]
#sale=sale[,-8]

boxplot(data$Global_Sales)
fivenum(data$Global_Sales)

#model
dum = dummy_cols(data,remove_first_dummy = TRUE,remove_selected_columns=TRUE)
help("dummy_cols")
testid=sample(nrow(data),trunc(nrow(data)*0.3))
test=dum[testid,]
train=dum[-testid,]

#linear regression
m1=lm(Global_Sales~.,data=train)
summary(m1)
mean((train$Global_Sales - m1$fitted.values)^2)
prd=predict(m1,test[,-1])
mean((test$Global_Sales - prd)^2)
plot(m1)

#lasso 
m=data.matrix(train[,-8])
lambda=10^seq(2, -3, by = -.1)
lasso=cv.glmnet(as.matrix(train[,-1]), train$Global_Sales, 
                alpha = 1, lambda = lambda, standardize = TRUE, nfolds = 5)
plot(lasso)
lambda_best <- lasso$lambda.min 
lambda_best
m2=glmnet(as.matrix(train[,-1]), train$Global_Sales, 
              alpha = 1, lambda = lambda_best, standardize = TRUE)
summary(m2)
coef(m2,, s = "lambda.1se")
as.matrix(coef(m2, m2$lambda.min))
mean((train$Global_Sales - predict(m2,as.matrix(train[,-1])))^2)
prd=predict(m2,as.matrix(test[,-1]))
mean((test$Global_Sales - prd)^2)

#random forest
m3=randomForest(x=train[,-1],y=train$Global_Sales)
m3
random.forest=m3
plot(random.forest)

oob.err=double(20)
test.err=double(20)
oob2=double(5)
test2=double(5)
help("randomForest")
for(mtry in 21:25) 
{
  rf=randomForest(x=train[,-1],y=train$Global_Sales,mtry=mtry,ntree=300) 
  oob2[mtry-20] = rf$mse[300] 
  
  pred<-predict(rf,test[,-1]) 
  test2[mtry-20]= mean((test$Global_Sales - pred)^2)
  
  cat(mtry," ") 
  
}
test.err=c(test.err,test2)
oob.err=c(oob.err,oob2)
matplot(1:mtry , cbind(oob.err,test.err), pch=19 , col=c("red","black"),type="b",ylab="Mean Squared Error",xlab="Number of Predictors Considered at each Split")
legend("topright",legend=c("Train Error","Test Error"),pch=19, col=c("red","black"))

rf=randomForest(x=train[,-1],y=train$Global_Sales,mtry=16,ntree=300) 
mean((train$Global_Sales - predict(rf,as.matrix(train[,-1])))^2)
prd=predict(rf,as.matrix(test[,-1]))
mean((test$Global_Sales - prd)^2)
imp = varImp(rf)
imp <- cbind(ColName = rownames(imp), imp)
rownames(imp) <- 1:nrow(imp)
imp=imp[order(imp$Overall,decreasing = TRUE),]
imp=imp[1:15,]
barplot(imp$Overall,
        main = "Feature Importance",
        xlab = "importance",
        ylab = "feature",
        names.arg = imp$ColName)
        #horiz = TRUE)

#naive
mean=mean(data$Global_Sales)
trainm=rep(mean,nrow(train))
prd=rep(mean,nrow(test))
mean((train$Global_Sales - mean)^2)
mean((test$Global_Sales - prd)^2)
