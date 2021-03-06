
rm(list=ls())
library(e1071)
set.seed(1)

x=matrix(rnorm(20*2),ncol=2)
y=c(rep(-1,10),rep(1,10))
x[y==1,]=x[y==1,] + 1
x
y
plot(x,col=(3-y))

dat = data.frame(x=x,y=as.factor(y))
svmfit=svm(y~.,data=dat,kernel='linear',cost=10,scale=FALSE)
plot(svmfit,dat)
svmfit$index
summary(svmfit)

svmfit=svm(y~.,data=dat,kernel='linear',cost=0.1,scale=FALSE)
plot(svmfit,dat)
svmfit$index

tune.out=tune(svm,y~.,data=dat,kernel='linear',ranges=list(cost=c(0.001,0.01,0.1,1,5,10,100)))
summary(tune.out)
bestmod=tune.out$best.model
summary(bestmod)

xtest=matrix(rnorm(20*2), ncol=2)
ytest=sample(c(-1,1), 20, rep=TRUE)
xtest[ytest==1,]=xtest[ytest==1,] + 1
testdat=data.frame(x=xtest,y=as.factor(ytest))
ypred=predict(bestmod,testdat)
table(predict=ypred,truth=testdat$y)

svmfit=svm(y~.,data=dat,kernel='linear',cost=0.01,scale=FALSE)
ypred=predict(svmfit,testdat)
table(ypred,testdat$y)
x[y==1,]=x[y==1,] + 0.5
plot(x,col=(y+5)/2,pch=19)

dat = data.frame(x=x,y=as.factor(y))
svmfit=svm(y~.,data=dat,kernel='linear',cost=1e5)
summary(svmfit)
plot(svmfit,dat)

svmfit=svm(y~.,data=dat,kernel='linear',cost=1)
summary(svmfit)
plot(svmfit,dat)



library(e1071)
library(ISLR)
names(Khan)
dim(Khan$xtrain)
dim(Khan$xtest)

length(Khan$ytrain)
length(Khan$ytest)
table(Khan$ytrain)
table(Khan$ytest)

dat=data.frame(x=Khan$xtrain,y=as.factor(Khan$ytrain))
out=svm(y~.,data=dat,kernel='linear',cost=10)
summary(out)

dat.te=data.frame(x=Khan$xtest , y = as.factor(Khan$ytest ))
pred.te=predict(out, newdata=dat.te)
table(pred.te, dat.te$y)

#svm1
n=150
p=2
sigma=1
meanpos=0
meanneg=3
npos=round(n/2)
nneg=n-npos
xpos=matrix(rnorm(npos*p,mean=meanpos,sd=sigma),npos,p)
xneg=matrix(rnorm(nneg*p,mean=meanpos,sd=sigma),npos,p)
x=rbind(xpos,xneg)
y=matrix(c(rep(1,npos),rep(-1,nneg)))

plot(x,col=ifelse(y>0,1,2))
legend('topleft',c('Positive','Negative'),col=seq(1),pch=1,text.col = seq(2))
help(legend)

ntrain=round(n*0.8)
tindex=sample(n,ntrain)
xtrain=x[tindex,]
xtest=x[-tindex,]
ytrain=y[tindex,]
ytest=y[-tindex,]
istrain=rep(0,n)
istrain[tindex]=1

plot(x,col=ifelse(y>0,1,2),pch=ifelse(istrain==1,1,2))
legend('topleft',c('Positive Train','Positive Test','Negative Train','Negative Test'),col=c(1,1,2,2),pch=c(1,2,1,2),text.col=c(1,1,2,2))

#svm2
library(e1071)
library(rpart)
data(Ozone,package = 'mlbench')

index=1:nrow(Ozone)
testindex=sample(index,trunc(length(index)/3))
help(trunc)
testset=na.omit(Ozone[testindex,-3])
trainset=na.omit(Ozone[-testindex,-3])
svm.model=svm(V4~.,data=trainset,cost=1000,gamma=0.0001)
svm.pred=predict(svm.model,testset[,-3])
crossprod(svm.pred-testset[,3])/length(testindex)
help("crossprod")

#svm3
data(iris)
attach(iris)

model=svm(Species~.,data=iris)
x=subset(iris,select=-Species)
y=Species
model=svm(x,y)

print(model)
summary(model)

pred=predict(model,x)
pred=fitted(model)

table(pred,y)

pred=predict(model,x,decision.values = TRUE)
attr(pred,'decision.values')[1:4,]

plot(cmdscale(dist(iris[,-5])),col=as.integer(iris[,5]),
     pch=c('o','+')[1:150 %in% model$index+1])
x=seq(0.1,5,by=0.05)
y=log(x)+rnorm(x,sd=0.2)

m=svm(x,y)
new=predict(m,x)

plot(x,y)
points(x,log(x),col=2)
points(x,new,col=4)

x=data.frame(a=rnorm(1000),b=rnorm(1000))
attach(x)

m=svm(x,gamma=0.1)

m=svm(~.,data=x,gamma=0.1)

newdata=data.frame(a=c(0,4),b=c(0,4))
predict(m,newdata)

plot(x,col=1:1000 %in% index+1,xlim=c(-5,5),ylim=c(-5,5))
points(newdata,pch='+',col=2,cex=5)

i2=iris
levels(i2$Species)[3]='versicolor'
summary(i2$Species)
wts=100/table(i2$Species)
wts
m=svm(Species~.,data=i2,class.weights=wts)

#svm4
data(promotergene,package = 'kernlab')
ind=sample(1:dim(promotergene)[1],20)
genetrain=promotergene[-ind,]
genetest=promotergene[ind,]
library(kernlab)
gene=ksvm(Class~.,data=genetrain,kernel='rbfdot',
          kpar=list(sigma=0.015),C=70,cross=4,prob.model=TRUE)
help(ksvm)

genetype=predict(gene,genetest,type='probabilities')



#svm5
m1 <- matrix( c( 0,0,0,1,1,2,1,2,3,2,3,3, 0, 1,2,3,0, 1, 2, 3,1,2,3,2,3,3,0, 0,    0,    1, 1, 2, 4, 4,4,4,    0, 
  1, 2, 3, 1,1,1,1,1,1,-1,-1,-1,-1,-1,-1, 1 ,1,1,1,1,1,-1,-1), ncol = 3 ) 
y=m1[,3]
x=m1[,1:2]
df=data.frame(x,y)

par(mfcol=c(4,2))
for(cost in c(1e-3,1e-2,1e-1,1e0,1e+1,1e+2,1e+3)){
  model.svm=svm(y~.,data=df,type='C-classification',
                kernel='linear',cost=cost,scale=FALSE)


print(model.svm$SV)
plot(x=0,ylim=c(0,5),xlim=c(0,3),main=paste('cost:',cost,'#SV:',nrow(model.svm$SV)))
points(m1[m1[,3]>0,1], m1[m1[,3]>0,2], pch=3, col="green") 
points(m1[m1[,3]<0,1], m1[m1[,3]<0,2], pch=4, col="blue") 
points(model.svm$SV[,1],model.svm$SV[,2], pch=18 , col = "red") 
}

#svm6
data(spam)
index=sample(1:dim(spam)[1])
spamtrain=spam[index[1:floor(dim(spam)[1]/2)],]
spamtest=spam[spamtest <- spam[index[((ceiling(dim(spam)[1]/2)) + 1):dim(spam)[1]], ],]

filter=ksvm(type~.,data=spamtrain,kernel='rbfdot',kpar=list(sigma=0.05),C=5,cross=3)
filter

mailtype=predict(filter,spamtest[,-58])
table(mailtype,spamtest[,58])

#svm7
data(iris)
rbf=rbfdot(sigma = 0.1)
rbf

irismodel=ksvm(Species~.,data=iris,type='C-bsvc',
               kernel=rbf,C=10,prob.model=TRUE)
fitted(irismodel)

predict(irismodel,iris[,-5],type='probabilities')

#svm8
x=rbind(matrix(rnorm(120),,2),matrix(rnorm(120,mean=3),,2))
y=matrix(c(rep(1,60),rep(-1,60)))
svp=ksvm(x,y,type='C-svc')
par(mfcol=c(1,1))
plot(svp,data=x)

k=as.kernelMatrix(crossprod(t(x)))
svp2=ksvm(k,y,type='C-svc')
svp2
help(t)

#svm9
xtest=rbind(matrix(rnorm(20),,2),matrix(rnorm(20,mean=3),,2))
Ktest= as.kernelMatrix(crossprod(t(xtest),t(x[SVindex(svp2), ])))
predict(svp2, Ktest)

k = function(x,y) {(sum(x*y) +1)*exp(-0.001*sum((x-y)^2))}
class(k)= "kernel"

data(promotergene)
gene= ksvm(Class~.,data=promotergene[c(1:20, 80:100),],kernel=k,
             C=5,cross=5)
gene

data(reuters)
is(reuters)
tsv= ksvm(reuters,rlabels,kernel="stringdot",
            kpar=list(length=5),cross=3,C=10)
tsv

x = seq(-20,20,0.1)
y =sin(x)/x + rnorm(401,sd=0.03)

regm = ksvm(x,y,epsilon=0.01,kpar=list(sigma=16),cross=3)
plot(x,y,type="l")
lines(x,predict(regm,x),col="red")

#svm10
k=function(x,y){(sum(x*y)+1)*exp(-0.001*sum((x-y)^2))}
class(k)='kernel'

data("promotergene")
gene=ksvm(Class~.,data=promotergene[c(1:20,80:100),],kernel=k,C=5,cross=5)
gene

#svm11
data("reuters")
is(reuters)
tsv=ksvm(reuters,rlabels,kernel='stringdot',kpar=list(length=5),cross=3,C=10)
tsv

#v15i09
library("kernlab")
data("iris")
irismodel = ksvm(Species ~ ., data = iris,type='C-bsvc',kernel='rbfdot',kpar=list(sigma=0.1),C=10,prob.model=TRUE)
irismodel                 

predict(irismodel,iris[c(3,10,56,68,107,120),-5],type='probabilities')                 
predict(irismodel,iris[c(3,10,56,68,107,120),-5],type='decision')                 

k=function(x,y){
  (sum(x*y)+1)*exp(0.001*sum((x-y)^2))
}
class(k)='kernel'

data("promotergene")
gene=ksvm(Class~.,data=promotergene[c(1:20,80:100),],kernel=k,C=5,cross=5)
gene

x=rbind(matrix(rnorm(120), , 2), matrix(rnorm(120,mean = 3), , 2))
y=matrix(c(rep(1, 60), rep(-1, 60)))
svp=ksvm(x, y, type = "C-svc", kernel = "rbfdot",kpar = list(sigma = 2))
plot(svp)

library("e1071")
model= svm(Species ~ ., data = iris,method = "C-classification", kernel = "radial",
        cost = 10, gamma=0.1)
summary(model)

plot(model, iris, Petal.Width ~Petal.Length, slice = list(Sepal.Width = 3,Sepal.Length = 4))
pred=predict(model,head(iris),decision.values=TRUE)
attr(pred,'decision.values')

library(e1071)
summary(spam[,58])
spam[,58]=='nonspam'
tobj= tune.svm(type ~ ., data = spamtrain[1:300,], gamma = 10^(-6:-3), cost = 10^(1:2))
summary(tobj)

plot(tobj,transform.x=log10,xlab=expression(log[10](gamma)),ylab='C')

bestGamma= tobj$best.parameters[[1]]
bestC= tobj$best.parameters[[2]]
model= svm(type ~ ., data = spamtrain, cost = bestC, gamma = bestGamma, cross = 10)
summary(model)

pred=predict(model,spamtest)
acc=table(pred,spamtest$type)
acc
classAgreement(acc)

library('klaR')
data('B3')
help("svmlight")
Bmod=svmlight(PHASEN ~ ., data = B3, svm.options = "-c 10 -t 2 -g 0.1 -v 0")
#error
predict(Bmod, B3[c(4, 9, 30, 60, 80, 120),-1])

library(svmpath)
data("svmpath")
attach(balanced.overlap)
svmpm= svmpath(balanced.overlap$x, balanced.overlap$y, kernel.function = radial.kernel,param.kernel = 0.1)
predict(svmpm, x, lambda = 0.1)
predict(svmpm, lambda = 0.2, type = "alpha")
