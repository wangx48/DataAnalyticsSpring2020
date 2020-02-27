#Xiuqi Wang
#Lab3

rm(list = ls())
library(rpart)
library(rpart.plot)
library(ggplot2)
data(msleep)
str(msleep)
help("msleep")
str(data)
mSleepDF1<-msleep[,c(3,6,10,11)]
str(mSleepDF1)
head(mSleepDF1)

help('rpart')
sleepModel_1<-rpart(sleep_total~.,data=mSleepDF1,method='anova')
sleepModel_1

help("rpart.plot")
rpart.plot(sleepModel_1,type = 3,fallen.leaves = TRUE)
rpart.plot(sleepModel_1,type = 3,fallen.leaves = TRUE,digits = 3)
rpart.plot(sleepModel_1,type = 3,fallen.leaves = TRUE,digits = 4)

#ctree
require(C50)
data('iris')
head(iris)
str(iris)
table(iris$Species)
set.seed(9850)
grn<-runif(nrow(iris))
help("runif")
irisrand<-iris[order(grn),]
help("order")
order(grn)
str(irisrand)
classificationmodel1<-C5.0(irisrand[1:100,-5],irisrand[1:500,5])
classificationmodel1
summary(classificationmodel1)
prediction1<-predict(classificationmodel1,irisrand[101:150,])
prediction1
table(irisrand[101:150,5],prediction1)
table(irisrand[101:150,5],predicted=prediction1)
plot(classificationmodel1)

help(ctree)

library(e1071)
classifier<-naiveBayes(iris[,1:4],iris[,5])
table(predict(classifier,iris[,-5]),iris[,5],
dnn=list('predict','actual'))
classifier$apriori
classifier$tables$Petal.Length
plot(function(x)dnorm(x,1.462,0.1736640),0.8,col='red',
     main='Petal length distribution for the 3 different species')
curve(dnorm(x, 4.260, 0.4699110), add=TRUE, col="blue")
curve(dnorm(x, 5.552, 0.5518947 ), add=TRUE, col = "green") 

#ctree1
require(rpart)
Swiss_rpart<-rpart(Fertility~Agriculture+Education+Catholic,data=swiss)
Swiss_rpart
plot(Swiss_rpart,main='Swiss Tree')
text(Swiss_rpart,col='blue',font=1)
help(text)

require(party)

treeSwiss<-ctree(Species~.,data=iris)
plot(treeSwiss)

cforest(Species~.,data=iris,controls = cforest_control(mtry=2,mincriterion = 0))
help("cforest")

treeFert=ctree(Fertility~Agriculture+Education+Catholic,data=swiss)

cforest(Fertility~Agriculture+Education+Catholic,data=swiss,
        controls = cforest_control(mtry=3,mincriterion = 1))
help("cforest_control")

library(tree)
tr=tree(Species~.,data=iris)
tr
tr$frame
plot(tr)
text(tr,col='blue',font=3,pos=2)
help('text')

#ctree2
fit2M<-ctree(Mileage~Price+Country+Reliability+Type,
             data=na.omit(cu.summary))
summary(fit2M)

plot(fit2M, uniform=TRUE, main="CI Tree Tree for Mileage ")
text(fit2M, use.n=TRUE, all=TRUE, cex=.8)

#ctree3
fitK= ctree(Kyphosis~Age+Number+Start, data=kyphosis)
plot(fitK,main='Conditional Inference Tree for Kyphosis')
plot(fitK,main='Conditional Inference Tree for Kyphosis',type='simple')
help(plot)
plot(fitK,main='Conditional Inference Tree for Kyphosis',type='extended')
