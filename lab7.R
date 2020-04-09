rm(list=ls())

#svm12
library(kernlab)
library(ISLR)
names(Khan)
data(Khan)
svp=ksvm(Khan$xtrain,Khan$ytrain,type='C-svc',kernel='vanilladot',C=100,scaled=c())

svp
attributes(svp)
alpha(svp)
alphaindex(svp)
b(svp)
#plot(svp,dat=train)

#svm13
cv.folds=function(n,folds=3){
  split(sample(n),rep(1:folds,length=length(y)))
}

data("iris")

library(rfUtilities)
cv.ksvm=function(x,y,folds=3){
  error=0
  folds_i <- sample(rep(1:folds, length.out = dim(x)[1]))
  for (i in folds){
    testid=which(folds_i==i)
    trainx=x[-testid,]
    trainy=y[-testid]
    testx=x[testid,]
    testy=y[testid]
    model= ksvm(trainx,trainy,type="C-svc",C=1,scaled=c())
    error=error+sum(predict(model,testx)!=testy)/length(testy)
  }
  return (error/folds)
}
x=as.matrix(iris[,-5])
y= iris[,5]

score=cv.ksvm(x,y,5)
score

svp= ksvm(Species~., data=iris,type="C-svc",C=1,scaled=c(),cross=5)
print(cross(svp))

#svm_rpart1
library(e1071)
library(rpart)
data(Glass,package='mlbench')
index=1:nrow(Glass)
testindex=sample(index,trunc(length(index)/3))
testset=Glass[testindex,]
trainset=Glass[-testindex,]
svm.model=svm(Type~.,data=trainset,cost=100,gamma=1)
svm.pred=predict(svm.model,testset[,-10])
rpart.model=rpart(Type~.,data=trainset)
rpart.pred=predict(rpart.model,testset[,-10],type='class')
table(pred=svm.pred,true=testset[,10])


#rpart&ctree&titanic   lab4
