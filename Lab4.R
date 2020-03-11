#Xiuqi Wang
#Lab4

rm(list=ls())
set.seed(12345)
help(par)
par(mar=rep(0.2,4))
data_Matrix<-matrix(rnorm(400),nrow = 40)
image(1:10,1:40,t(data_Matrix)[,nrow(data_Matrix):1])
t(data_Matrix)[,40:1]
help("heatmap")
help(rep)
heatmap(data_Matrix)
help('rbinom')
set.seed(678910)
for (i in 1:40){
  coin_Flip<-rbinom(1,size=1,prob=0.5)
  if(coin_Flip){
    data_Matrix[i,]<-data_Matrix[i,]+rep(c(0,3),each=5)
  }
}
par(mar=rep(0.2,4))
image(1:10,1:40,t(data_Matrix)[,nrow(data_Matrix):1])

heatmap(data_Matrix)

hh<-hclust(dist(data_Matrix))
data_Matrix_Ordered<-data_Matrix[hh$order,]
par(mfrow=c(1,3))
image(t(data_Matrix_Ordered)[,nrow(data_Matrix_Ordered):1])
plot(rowMeans(data_Matrix_Ordered),40:1, ,xlab = 'The Row Mean',ylab='Row',pch=19)
plot(colMeans(data_Matrix_Ordered), ,xlab = 'Column',ylab='Column Mean',pch=19)

#Lab_bronx1
library(gdata)
bronx1=read.xls(file.choose(),pattern = 'BOROUGH',stringsAsFactor=FALSE,sheet = 1)
#strawberryperl
help("read.xls")
bronx1<-bronx1[which(bronx1$GROSS.SQUARE.FEET!='0'&bronx1$LAND.SQUARE.FEET!='0'&bronx1$SALE.PRICE!='$0'),]

View(bronx1)
attach(bronx1)
SALE.PRICE<-sub('\\$','',SALE.PRICE)
SALE.PRICE<-as.numeric(gsub(',','',SALE.PRICE))
GROSS.SQUARE.FEET<-as.numeric(gsub(',','',GROSS.SQUARE.FEET))
LAND.SQUARE.FEET<-as.numeric(gsub(',','',LAND.SQUARE.FEET))
plot(log(GROSS.SQUARE.FEET),log(SALE.PRICE))
m1=lm(log(SALE.PRICE)~log(GROSS.SQUARE.FEET))
summary(m1)
abline(m1,col='red',lwd=2)
plot(resid(m1))

#model2

m2<-lm(log(SALE.PRICE)~log(GROSS.SQUARE.FEET)+log(LAND.SQUARE.FEET)+factor(bronx1$NEIGHBORHOOD))
summary(m2)
plot(resid(m2))

m2a=lm(log(SALE.PRICE)~0+log(GROSS.SQUARE.FEET)+log(LAND.SQUARE.FEET)+factor(bronx1$NEIGHBORHOOD))
#no intercept
summary(m2a)
plot(resid(m2a))
#noconstant model usually have large r2

#model3
m3<-lm(log(SALE.PRICE)~0+log(GROSS.SQUARE.FEET)+log(LAND.SQUARE.FEET)+factor(bronx1$NEIGHBORHOOD)+factor(bronx1$BUILDING.CLASS.CATEGORY))
summary(m3)
plot(resid(m3))

#model4
m4<-lm(log(SALE.PRICE)~0+log(GROSS.SQUARE.FEET)+log(LAND.SQUARE.FEET)+factor(bronx1$NEIGHBORHOOD)*factor(bronx1$BUILDING.CLASS.CATEGORY))
summary(m4)
plot(resid(m4))
par(mfrow=c(1,1))

#bronx2
bronx1$SALE.PRICE<-sub('\\$','',bronx1$SALE.PRICE)
bronx1$SALE.PRICE<-as.numeric(gsub(',','',bronx1$SALE.PRICE))
bronx1$GROSS.SQUARE.FEET<-as.numeric(gsub(',','',bronx1$GROSS.SQUARE.FEET))
bronx1$LAND.SQUARE.FEET<-as.numeric(gsub(',','',bronx1$LAND.SQUARE.FEET))
bronx1$SALE.DATE<-as.Date(gsub('[^]:digit:]]','',bronx1$SALE.DATE))
# ^ head
bronx1$YEAR.BUILT<-as.numeric(gsub('[^]:digit:]]','',bronx1$YEAR.BUILT))
bronx1$ZIP.CODE=as.character(gsub('[^]:digit]]','',bronx1$ZIP.CODE))
help(gsub)

minprice<-10000
bronx1<-bronx1[which(bronx1$SALE.PRICE>=minprice),]
nval<-dim(bronx1)[1]

bronx1$ADDRESS<-gsub("[,][[:print:]]*","",gsub("[ ]+","",trim(bronx1$ADDRESS))) 
bronxadd<-unique(data.frame(bronx1$ADDRESS, bronx1$ZIP.CODE,stringsAsFactors=FALSE)) 
#Printable characters: [:alnum:], [:punct:] and space.
# * The preceding item will be matched zero or more times.
help(trim)
names(bronxadd) = c('ADDRESS','ZIP.CODE')
bronxadd=bronxadd[order(bronxadd$ADDRESS),]
duplicates = duplicated(bronx1$ADDRESS)
for (i in 2345){
  if (duplicates[i]==FALSE)
    dupadd = bronxadd[bronxadd$duplicates,1]
}
#?
nsample=450
devtools::install_github("dkahle/ggmap")
library(ggmap)
addsample<-bronxadd[sample.int(dim(bronxadd),size=nsample),]
addrlist = paste(addsample$ADDRESS,'NY',addsample$ZIP.CODE,'US',sep = ' ')
register_google(key='mykey')
#for security
querylist = geocode(addrlist)
help("geocode")

matched = (querylist$lat!=0 && querylist$lon !=0)
addsample = cbind(addsample,querylist$lat,querylist$lon)
names(addsample)<-c('address','zipcode','latitude','longtitude')
adduse<-merge(bronx1,addsample)
adduse<-adduse[!is.na(adduse$latitude),]
mapcoord = adduse[,c(2,3,24,25)]

table(mapcoord$NEIGHBORHOOD)
mapcoord$NEIGHBORHOOD<-as.factor(mapcoord$NEIGHBORHOOD)
map = get_map(location = 'Bronx',zoom=12)
ggmap(map)+geom_point(aes(x = mapcoord$longtitude,y=mapcoord$latitude,
                          size = 1, color = mapcoord$NEIGHBORHOOD),data = mapcoord)+theme(legend.position = 'none')
mapmeans = cbind(adduse,as.numeric(mapcoord$NEIGHBORHOOD))
colnames(mapmeans)[26] = 'neighborhood'
keeps = c("ZIP.CODE","NEIGHBORHOOD","TOTAL.UNITS","LAND.SQUARE.FEET","GROSS.SQUARE.FEET","SALE.PRICE","latitude","longtitude") 
mapmeans = mapmeans[keeps]
mapmeans$NEIGHBORHOOD = as.numeric(mapcoord$NEIGHBORHOOD)

for (i in 1:8){
  mapmeans[,i] = as.numeric(mapmeans[,i])
}

#classification
library(class)
mapcoord$class = as.numeric(mapcoord$NEIGHBORHOOD)
nclass = dim(mapcoord)[1]
split = 0.8
trainid = sample.int(nclass,floor(split*nclass))
testid = (1:nclass)[-trainid]

kmax = 10
knnpred = matrix(NA,ncol = kmax, nrow = length(testid))
knntesterr = rep(NA,times = kmax)
for (i in 1:kmax){
  knnpred[,i]<-knn(mapcoord[trainid,3:4],mapcoord[testid,3:4],cl=mapcoord[trainid,2],k=i,use.all = FALSE)
  knntesterr[i] = sum(knnpred[,i]!=mapcoord[testid,2])/length(testid)
}
#too many ties in knn
knntesterr

#clustering
help(kmeans)
mapobj = kmeans(na.omit(mapmeans),5,iter.max = 10,nstart = 5,algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"))
fitted(mapobj,method=c('centers','classes'))
mapobj$centers

library(cluster)
clusplot(na.omit(mapmeans),mapobj$cluster,color = TRUE,shade = TRUE,labels = 2, lines = 0)

library(fpc)
plotcluster(na.omit(mapmeans),mapobj$cluster)

mapmeans1<-mapmeans[,-c(1,3,4)]
mapobjnew<-kmeans(mapmeans1,5, iter.max=10, nstart=5, algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"))
fitted(mapobjnew,method=c("centers","classes"))
clusplot(mapmeans1, mapobjnew$cluster, color=TRUE, shade=TRUE, labels=2, lines=0) 
plotcluster(mapmeans1, mapobjnew$cluster)
ggmap(map) + geom_point(aes(x = mapcoord$Longitude, y = mapcoord$Latitude, size =1, color=mapobjnew$cluster), data = mapcoord)

#ctree2
pairs(~Fertility+Education+Catholic,data = swiss,subset = Education<20,main = 'Swiss data,Education<20')
require(party)
swiss_ctree = ctree(Fertility~Agriculture+Education+Catholic,data=swiss)
plot(swiss_ctree)

#kknn1
library(kknn)
data("iris")
m = dim(iris)[1]
val = sample(1:m,size = round(m/3),replace = FALSE,prob=rep(1/m,m))
iris.learn = iris[-val,]
iris.valid = iris[val,]
iris.kknn = kknn(Species~.,iris.learn,iris.valid,distance=1,kernel='triangular')
summary(iris.kknn)
fit = fitted(iris.kknn)
table(iris.valid$Species,fit)
pcol = as.character(as.numeric(iris.valid$Species))
pairs(iris.valid[1:4],pch=pcol,col=c('green3','red')[(iris.valid$Species !=fit)+1])
# fit green / non fit red
help("pch")

#kknn2
require(kknn)
data("ionosphere")
ionosphere.learn = ionosphere[1:200,]
ionosphere.valid = ionosphere[-c(1:200),]
fit.kknn = kknn(class~.,ionosphere.learn,ionosphere.valid)
table(ionosphere.valid$class,fit.kknn$fit)
fit.train1 = train.kknn(class~.,ionosphere.learn,kmax = 15,
                        kernel = c("triangular", "rectangular", "epanechnikov", "optimal"), distance = 1)
table(predict(fit.train1,ionosphere.valid),ionosphere.valid$class)
fit.train2 = train.kknn(class~.,ionosphere.learn,kmax = 15,
                        kernel = c("triangular", "rectangular", "epanechnikov", "optimal"), distance = 2)
table(predict(fit.train2,ionosphere.valid),ionosphere.valid$class)
help("train.kknn")


#kknn3
data("swiss")
pairs(~Fertility+Education+Catholic,data=swiss,subset=Education<20,main='Swiss data, Education<20')

#kmeans1
data(swiss)
sclass = kmeans(swiss[2:6],3)
table(sclass$cluster,swiss[,1])

#nyt
nyt1 = read.csv('C:\\Users\\wxq\\Desktop\\da\\hw3\\dds_ch2_nyt\\nyt1.csv')
nyt1 = nyt1[which(nyt1$Impressions>0 & nyt1$Clicks>0 & nyt1$Age>0),]
nnyt1 = dim(nyt1)[1]
sampling.rate = 0.9
num.test.set.labels = nnyt1*(1.-sampling.rate)
training = sample(1:nnyt1,sampling.rate*nnyt1,replace=FALSE)
train = subset(nyt1[training,],select=c(Age,Impressions))
testing = setdiff(1:nnyt1,training)
test = subset(nyt1[testing,],select = c(Age,Impressions))
cg = nyt1$Gender[training]
true.labels = nyt1$Gender[testing]
library(class)
classif = knn(train,test,cg,k=5)
classif
attributes(.Last.value)

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

#lab2_abalone
aba = read.csv('C:\\Users\\wxq\\Desktop\\da\\hw2\\abalone.csv')
aba$Sex = is.numeric(aba$Sex)
naba = dim(aba)[1]
sampling.rate = 0.9
num.test.set.labels = naba*(1.-sampling.rate)
training = sample(1:naba,sampling.rate*naba,replace = FALSE)
train = subset(aba[training,],select=c('Sex','Length','Diameter','Height','Whole.weight','Shucked.weight','Viscera.weight','Shell.weight'))
testing = setdiff(1:naba,training)
test = subset(aba[testing,],select=c('Sex','Length','Diameter','Height','Whole.weight','Shucked.weight','Viscera.weight','Shell.weight'))
crings = aba$Rings[training]
true.labels = aba$Rings[testing]
classif=knn(train,test,crings,k=5)
classif
attributes(.Last.value)

# try bayes
aba$rings2 = aba$Rings>10
library(e1071)
m = naiveBayes(rings2~Sex+Length+Diameter+Height+Whole.weight+Shucked.weight+Viscera.weight+Shell.weight,data=aba)
table(predict(m,aba[,1:8]),aba[,10])

#lab2_kknn1
library(kknn)
spam = read.csv('C:\\Users\\wxq\\Desktop\\da\\w7\\spambase.csv',header = FALSE)
n = dim(spam)[1]
spam=spam[2:n,]
n=n-1
colnames(spam)[58]='spam'
sampling.rate = 0.9
num.test.set.labels = n*(1.-sampling.rate)
training = sample(1:n,sampling.rate*n,replace = FALSE)
train = subset(spam[training,])
testing = setdiff(1:n,training)
test = subset(spam[testing,])

spam.kknn = kknn(spam~.,train,test,distance=1,kernel='triangular')
summary(spam.kknn)
table(spam.kknn$fit,test[,'spam'])
help(kknn)
spam.kknn = kknn(spam~.,train,test,distance=1,kernel='rectangular')

#lab2_nbayes1
require(mlbench)
data("HouseVotes84")
model = naiveBayes(Class~.,data=HouseVotes84)
predict(model,HouseVotes84[1:10,-1])
predict(model,HouseVotes84[1:10,-1],type='raw')
pred=predict(model,HouseVotes84[,-1])
table(pred,HouseVotes84$Class)

data("Titanic")
m = naiveBayes(Survived~.,data=Titanic)
m
predict(m,as.data.frame(Titanic)[,1:3])

data(iris)
m=naiveBayes(Species~.,data=iris)
m=naiveBayes(iris[,-5],iris[,5])
m
table(predict(m,iris[,-5]),iris[,5])

#lab2_nbayes2
data("Titanic")
md = naiveBayes(Survived~.,data=Titanic)
md

#lab2_nbayes3
library(klaR)
model=NaiveBayes(Class~.,data=HouseVotes84)
predict(model,HouseVotes84[1:10,-1])
pred = predict(model,HouseVotes84[,-1])
table(pred$class,HouseVotes84$Class)

#lab2_nbayes4
library(ElemStatLearn)
library(klaR)
library(caret)
data(spam)
sub=sample(nrow(spam),floor(nrow(spam)*0.9))
help(floor)
train=spam[sub,]
test=spam[-sub,]

xTrain=train[,-58]
yTrain=train$spam

xTest=test[,-58]
yTest=test$spam

model=train(xTrain,yTrain,'nb',trControl=trainControl(method='cv',number=10))
help("trainControl")
prop.table(table(predict(model$finalModel,xTest)$class,yTest))

train.ind=sample(1:nrow(spam),ceiling(nrow(spam)*2/3),replace=FALSE)
nb.res=NaiveBayes(spam~.,data=spam[train.ind,])

nb.pred=predict(nb.res,spam[-train.ind,])
head(predict(nb.res))
#using train data
table(predict(nb.res,xTest)$class,yTest)

#lab2_swiss
data("swiss")
swiss$class = as.factor(cut(swiss$Infant.Mortality,breaks = 3))
sclass = kmeans(swiss[,1:6],3)
help("kmeans")
table(sclass$cluster,swiss[,7])

library(e1071)
help("naiveBayes")
m=naiveBayes(swiss[,-7],swiss[,7])
table(predict(m,swiss[,-7]),swiss[,7])
#code on the website cannot run and change the code a little

#lab3_kknn1
require(kknn)
data("iris")
m=dim(iris)[1]
val = sample(1:m,size=round(m/3),replace=FALSE,prob=rep(1/m,m))
iris.learn=iris[-val,]
iris.valid=iris[val,]
iris.kknn=train.kknn(Species~.,iris.learn,distance=1,kernel=c("triangular", "epanechnikov", "biweight", "triweight", "cos", "inv", "gaussian", "rank", "optimal"))
summary(iris.kknn)
table(predict(iris.kknn,iris.valid),iris.valid$Species)

head(iris.kknn$W)#?
head(iris.kknn$D)#?
head(iris.kknn$c)
head(iris.kknn$fitted.values)

#lab3_randomforest1
require(randomForest)
data(kyphosis,package='rpart')
fitKF=randomForest(Kyphosis~Age+Number+Start,data=kyphosis)
print(fitKF)
importance(fitKF)

fitSwiss = randomForest(Fertility~Agriculture+Education+Catholic,data=swiss)
print(fitSwiss)
plot(fitSwiss)
getTree(fitSwiss,1,labelVar = TRUE)
help("randomForest")

help(rfcv)

data("imports85")
m = randomForest(price~.,data=na.omit(imports85))
plot(m)
getTree(m,1,labelVar = TRUE)

#lab3_rpart1
require(rpart)
Swiss_rpart <- rpart(Fertility ~ Agriculture + Education + Catholic, data = swiss)
plot(Swiss_rpart)
text(Swiss_rpart) 
plot(Swiss_rpart,main='Tree plot')
text(Swiss_rpart,col='red',font=2)

#lab3_rpart2
fitM <- rpart(Mileage~Price + Country + Reliability + Type, method="anova", data=cu.summary)
printcp(fitM) 
plotcp(fitM)
summary(fitM)
par(mfrow=c(1,2)) 
rsq.rpart(fitM)

plot(fitM, uniform=TRUE, main="Regression Tree for Mileage ")
text(fitM, use.n=TRUE, all=TRUE, cex=.8)

par(mfrow=c(1,2))
pfitM<- prune(fitM, cp=0.01160389)
plot(pfitM, uniform=TRUE, main="Pruned Regression Tree for Mileage")
text(pfitM, use.n=TRUE, all=TRUE, cex=.8)
pfitM<- prune(fitM, cp=0.02544094)
plot(pfitM, uniform=TRUE, main="Pruned Regression Tree for Mileage")
text(pfitM, use.n=TRUE, all=TRUE, cex=.8)

post(pfitM, file = 'tree.ps', title = "Pruned Regression Tree")
help(post)
par(mfrow=c(1,1))
#lab3_rpart3
library(e1071)
library(rpart)
data(Glass, package="mlbench")
index = 1:nrow(Glass)
testindex = sample(index, trunc(length(index)/3))
testset = Glass[testindex,]
trainset = Glass[-testindex,]
rpart.model = rpart(Type ~ ., data = trainset)
rpart.pred = predict(rpart.model, testset[,-10], type = "class")
printcp(rpart.model)
help("printcp")
#omplexity parameter
plotcp(rpart.model)
rsq.rpart(rpart.model)
print(rpart.model)

plot(rpart.model,compress=TRUE)
text(rpart.model, use.n=TRUE)
plot(rpart.pred)

#lab3_rpart4
fitK = rpart(Kyphosis ~ Age + Number + Start, method="class", data=kyphosis)
printcp(fitK) 
plotcp(fitK) 
summary(fitK) 

plot(fitK, uniform=TRUE, main="Classification Tree for Kyphosis")
text(fitK, use.n=TRUE, all=TRUE, cex=.8)

post(fitK, file = 'fitK.ps', title = "Classification Tree for Kyphosis")

pfitK<- prune(fitK, cp=   fitK$cptable[which.min(fitK$cptable[,"xerror"]),"CP"])
plot(pfitK, uniform=TRUE, main="Pruned Classification Tree for Kyphosis")
text(pfitK, use.n=TRUE, all=TRUE, cex=.8)
post(pfitK, file = 'PFITK.ps', title = 'Pruned Classification Tree for Kyphosis')


#TREES FOR TITANIC
data("Titanic")
m1=rpart(Survived~.,data=Titanic)
plot(m1)
text(m1)

library(party)
m2=ctree(Survived~.,data=Titanic)
m2

m3=randomForest(Survived~.,data=Titanic)
plot(m3)            
m3

c1 = hclust(dist(Titanic),method='complete')
c1
help("hclust")