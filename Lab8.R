rm(list = ls())
data=read.csv('//Users//wangxiuqi//Desktop//da//w11//dataset_EFA.csv')

data

library(psych)

corMat=cor(data)
solution=fa(r=corMat,nfactors = 2,rotate='oblimin',fm='pa')
solution

fit=princomp(data,cor=TRUE)
summary(fit)
loadings(fit)
plot(fit,type='lines')
fit$scores
biplot(fit)

fit=principal(data,nfactors = 5,rotate = 'varimax')
help("principal")
help('fa')
fit

fit=factanal(data,3,rotation='varimax')
print(fit,digits = 2,cutoff=.3,sort=TRUE)

load=fit$loadings[,1:2]
load
plot(load,type='n')
help(plot)#n no ploting
text(load,labels=names(data),cex=0.7)

fit=factor.pa(data,nfactors = 3)
fit

library(nFactors)
ev=eigen(cor(data))
ap=parallel(var=ncol(data),rep = 100,cent=.05)
ns=nScree(x=ev$values,aparallel = ap$eigen$qevpea)
plotnScree(ns)

library(FactoMineR)
result=PCA(data)

#library(sem)
#data.cov=cov(data)
#model.data=specify.model()

#latent factor vs reduce dimension

#lab2_fa1
v1=c(rep(1,10),3,3,3,3,3,4,5,6)
v2=c(1,2,1,1,1,1,2,1,2,1,3,4,3,3,3,4,6,5)
v3=c(3,3,3,3,3,1,1,1,1,1,1,1,1,1,1,5,4,6)
v4=c(3,3,4,3,3,1,1,2,1,1,1,1,2,1,1,5,6,4)
v5=c(1,1,1,1,1,3,3,3,3,3,1,1,1,1,1,6,4,5)
v6=c(1,1,1,2,1,3,3,3,4,3,1,1,1,2,1,6,5,4)
m1=cbind(v1,v2,v3,v4,v5,v6)
cor(m1)
factanal(m1,factors = 3)
factanal(m1,factors = 3,rotation='promax')
prcomp(m1)
help("factanal")#on cor
help("prcomp")

factanal(~v1+v2+v3+v4+v5+v6,factors = 3,scores = 'Bartlett')$scores


#lab2_fa2
install.packages('Hmisc')
library(Hmisc)
AthleticsData=spss.get('//Users//wangxiuqi//Desktop//da//w11//AthleticsData.sav')
attach(AthleticsData)

names(AthleticsData)

cor(AthleticsData)
prcomp(AthleticsData)

fit.2=factanal(AthleticsData,factors = 2,rotation='varimax')
print(fit.2)
fit.3=factanal(AthleticsData,factors = 3,rotation = 'varimax')
print(fit.3)
print(fit.3,digits=2,cutoff=.2,sort=TRUE)

install.packages('GPArotation')
library(GPArotation)
library(psych)
fit=principal(AthleticsData,nfactors = 3,rotate='arimax')
fit

fa.promax = function(fo,factors=1,digits=4,sort=FALSE,m=3,...)
{
  sort.loadings = function(ld)
  {
    f = dim(ld)[2]
    loadmax=abs(ld[,1:f])==apply(abs(ld[,1:f]),1,max)
    FL = as.list(colnames(ld)[1:f])
    for (i in 1:f)
    {
      FL[[i]]=ld[loadmax[,i]==TRUE,]
      if (length(dim(FL[[i]])) > 0)
      {
        FL[[i]]=FL[[i]][order(abs(FL[[i]][,i]),decreasing=TRUE),]
      }
      if (i == 1)
      {
        erg=FL[[1]]
      }
      else
      {
        erg=rbind(erg,FL[[i]])
        if (i == 2)
        {
          if (length(dim(FL[[1]])) == 0)
          {
            rownames(erg)[1] = rownames(ld)[which(loadmax[,1]==TRUE)]
          }
        }
        if (i > 1)
        {
          if (length(dim(FL[[i]])) == 0)
          {
            rownames(erg)[dim(erg)[1]] = rownames(ld)[which(loadmax[,i]==TRUE)]
          }
        }
      }
    }
    erg
  }
  
  res = factanal(fo,factors=factors,rotation="none",...)
  if (factors > 1)
  {
    vm = varimax(loadings(res))
    ssvm = diag(t(vm$loadings[]) %*% vm$loadings[])
    ssvm = rbind(ssvm,ssvm/dim(vm$loadings[])[1])
    pm = promax(loadings(vm),m=m) # m=3 is default in many programs
    sspm = diag(t(pm$loadings[]) %*% pm$loadings[])
    sspm = rbind(sspm,sspm/dim(pm$loadings[])[1])
    A = vm$rotmat %*% pm$rotmat
    phi = solve(t(A) %*% A)
    pmst = pm$loadings %*% phi
    unld = res$loadings[]
    vmld = vm$loadings[]
    pmld = pm$loadings[]
    SeqF = order(colSums(vmld**2),decreasing=TRUE)
    ssvm = ssvm[,SeqF]
    ssvm = rbind(ssvm,cumsum(ssvm[2,]))
    rownames(ssvm)=c('SS loadings','Proportion Var','Cumulative Var')
    sspm = sspm[,SeqF]
    sspm = rbind(sspm,cumsum(sspm[2,]))
    rownames(sspm)=c('SS loadings','Proportion Var','Cumulative Var')
    unld = unld[,SeqF]
    vmld = vmld[,SeqF]
    pmld = pmld[,SeqF]
    pmst = pmst[,SeqF]
    phi = phi[,SeqF]
    phi = phi[SeqF,]
    colnames(unld) = paste(rep('Factor',factors),1:factors,sep='')
    colnames(ssvm) = colnames(unld)
    colnames(sspm) = colnames(unld)
    colnames(vmld) = colnames(unld)
    colnames(pmld) = colnames(unld)
    colnames(pmst) = colnames(unld)
    colnames(phi)=colnames(unld)
    rownames(phi)=colnames(unld)
    if (length(res$scores) > 0)
    {
      FS = sqrt(res$n.obs/(res$n.obs-1))*scale(res$scores)
      vm.fs = FS %*% vm$rotmat
      pm.fs = sqrt(res$n.obs/(res$n.obs-1))*scale(FS %*% pm$rotmat)[,]
      FS = FS[,SeqF]
      vm.fs = vm.fs[,SeqF]
      pm.fs = pm.fs[,SeqF]
      colnames(FS) = colnames(unld)
      colnames(vm.fs)=colnames(unld)
      colnames(pm.fs)=colnames(unld)
    }
    if (sort==TRUE)
    {
      uniqueness = cbind(sort(res$uniqueness))
      vmld = sort.loadings(vmld)
      Dummy = NULL
      for (i in 1:nrow(unld))
      {
        Dummy = rbind(Dummy,unld[which(rownames(unld)==rownames(vmld)[i]),])
      }
      rownames(Dummy)=rownames(vmld)
      unld = Dummy
      pmld = sort.loadings(pmld)
      pmst = sort.loadings(pmst)
    }
    else
    {
      uniqueness = cbind(res$uniqueness)
    }
    colnames(uniqueness) = "residual variance"
    if (length(res$scores) > 0)
    {
      erg = list(uniqueness=round(uniqueness,digits),
                 unrotated.loadings=round(unld,digits),
                 unrotated.factorscores=round(FS[,],digits),
                 varimax.SS = round(ssvm,digits),
                 varimax.loadings=round(vmld,digits),
                 varimax.factorscores = round(vm.fs,digits),
                 promax.SS = round(sspm,digits),
                 promax.loadings=round(pmld,digits),
                 promax.structure=round(pmst,digits),
                 corr.factors=round(phi,digits),
                 promax.factorscores = round(pm.fs,digits),
                 n=res$n.obs,chi=round(res$STATISTIC,3),df=res$dof,p=res$PVAL)
    }
    else
    {
      erg = list(uniqueness=round(uniqueness,digits),
                 unrotated.loadings=round(unld,digits),
                 varimax.SS = round(ssvm,digits),
                 varimax.loadings=round(vmld,digits),
                 promax.SS = round(sspm,digits),
                 promax.loadings=round(pmld,digits),
                 promax.structure=round(pmst,digits),
                 corr.factors=round(phi,digits),
                 n=res$n.obs,chi=round(res$STATISTIC,3),df=res$dof,p=res$PVAL)
    }
  }
  else
  {
    ss = diag(t(res$loadings[]) %*% res$loadings[])
    ss = rbind(ss,ss/dim(res$loadings[])[1])
    ss = rbind(ss,cumsum(ss[2,]))
    rownames(ss)=c('SS loadings','Proportion Var','Cumulative Var')
    if (sort==TRUE)
    {
      uniqueness = cbind(sort(res$uniqueness))
      vmld=cbind(sign(res$loadings[order(abs(res$loadings),decreasing=TRUE)])*
                   sort(abs(res$loadings[1:dim(res$loadings)[1],]),dec=TRUE))
      colnames(vmld)="Factor1"
    }
    else
    {
      uniqueness = cbind(res$uniqueness)
      vmld=res$loadings[]
    }
    colnames(uniqueness) = "residual variance"
    if (length(res$scores) > 0)
    {
      FS = cbind(sqrt(res$n.obs/(res$n.obs-1))*scale(res$scores))
      colnames(FS)='Factor1'
      erg = list(uniqueness=round(uniqueness,digits),
                 SS = round(ss,digits),
                 loadings=round(vmld,digits),
                 factorscores = round(FS,digits),
                 n=res$n.obs,chi=round(res$STATISTIC,3),df=res$dof,p=res$PVAL)
    }
    else
    {
      erg = list(uniqueness=round(uniqueness,digits),
                 SS = round(ss,digits),
                 loadings=round(vmld,digits),
                 n=res$n.obs,chi=round(res$STATISTIC,3),df=res$dof,p=res$PVAL)
    }
  }
  erg
}

fa.promax(~general+picture+blocks+maze+reading+vocab,covmat=ability.cov)

fa.promax(~general+picture+blocks+maze+reading+vocab,factors=2,sort=TRUE,covmat=ability.cov)

fa.promax(~general+picture+blocks+maze+reading+vocab,factors=2,sort=TRUE,m=2.5,covmat=ability.cov)

fit.3.promax=update(fit.3,rotation='promax')
help(update)
colnames(fit.3.promax$loadings)=c('Endurance','Strength','Hand-Eye')
print(loadings(fit.3.promax),digits = 2,cutoff = .2,sort=TRUE)
AssignFactorNames=function(fit.object,names){
  colnames(fit.object$promax.loadings)=names
  colnames(fit.object$varimax.loadings)=names
  rownames(fit.object$corr.factors)=names
  colnames(fit.object$corr.factors)=names
}
fit.3.Enzmann <- fa.promax(AthleticsData,factors=3, digits=2, sort=TRUE) 
factor.names=c('Endurance','Strength','Hand-Eye')
AssignFactorNames(fit.3.Enzmann,factor.names)
fit.3.Enzmann

#lab2_fa4
library(psych)
epi=read.csv('//Users//wangxiuqi//Desktop//da//w11//epi.csv')
epi=epi[,-1]
epi.dictionary=read.csv('//Users//wangxiuqi//Desktop//da//w11//epi.dictionary.csv')
rownames(epi.dictionary)=epi.dictionary$X
epi.dictionary=epi.dictionary[-1]
epi.keys=make.keys(epi,list(E = c(1, 3, -5, 8, 10, 13, -15, 17, -20, 22, 25, 27,-29, -32, -34, -37, 39, -41, 44, 46, 49, -51, 53, 56),
                            N=c(2, 4, 7, 9, 11, 14, 16, 19, 21, 23, 26, 28, 31, 33, 35, 38, 40,43, 45, 47, 50, 52, 55, 57),
                            L = c(6, -12, -18, 24, -30, 36, -42, -48, -54),
                            I =c(1, 3, -5, 8, 10, 13, 22, 39, -41), 
                            S = c(-11, -15, 17, -20, 25, 27, -29, -32, -37, 44, 46, -51, 53)))
epi.keys
scores=scoreItems(epi.keys,epi)
scores
help("make.keys")
help("fa.lookup")
scores=scoreItems(epi.keys,epi)
N=epi[abs(epi.keys[,'N'])>0]
E=epi[abs(epi.keys[,'E'])>0]
lookupFromKeys(epi.keys[,1:3],epi.dictionary)

#lab2_fa5
set.seed(1.234)
N=200
P=6
Q=2

Lambda=matrix(c(0.7,-0.4, 0.8,0, -0.2,0.9, -0.3,0.4, 0.3,0.7, -0.8,0.1),nrow=P, ncol=Q, byrow=TRUE)

library(mvtnorm)
FF=rmvnorm(N,mean=c(5,15),sigma = diag(Q))
E=rmvnorm(N, rep(0, P), diag(P))
X=FF %*% t(Lambda)+E
Xdf=data.frame(X) 

Xdi=lapply(Xdf, function(x) cut(x, breaks=c(-Inf, median(x), Inf), ordered=TRUE))
Xdidf=do.call("data.frame", Xdi)
XdiNum=data.matrix(Xdidf) 

library(polycor)
pc=hetcor(Xdidf,ML=TRUE)

faPC=fa(r=pc$correlations, nfactors=2, n.obs=N, rotate='varimax')
faPC$loadings

faPCdirect=fa.poly(XdiNum, nfactors=2, rotate="varimax")   
faPCdirect$fa$loadings 
factor.plot(faPCdirect$fa, cut=0.5)
fa.diagram(faPCdirect)

fa.parallel.poly(XdiNum) 
vss(pc$correlations, n.obs=N, rotate='varimax') 

library(random.polychor.pa)
random.polychor.pa(data.matrix=XdiNum, nrep=5, q.eigen=0.99)

#boston
library(MASS)
attach(Boston)
summary(Boston)
library(corrplot)
corr_matrix<-cor(Boston)
corrplot(corr_matrix, type="upper")

summary(crim)
require(ggplot2)
require(plotly)
plot_ly(data = Boston, x = ~lstat, y = ~crim)
plot_ly(data = Boston, x = ~tax, y = ~crim)
plot_ly(data=Boston, x = ~crim, type = "histogram")
quantile(Boston$crim, .90)
hcrim=subset(Boston, crim>quantile(Boston$crim, .90))
sum(crim>quantile(Boston$crim, .90))
summary(hcrim)

plot_ly(data=Boston, y = ~lstat, name = "Boston", type="box")  %>%
  add_boxplot(data=hcrim, y= ~lstat, name = "Area with 90th percentile crime rate", type="box")

plot_ly(data=Boston, y = ~medv, name = "Boston", type="box")  %>%
  add_boxplot(data=hcrim, y= ~medv, name = "Area with 90th percentile lstat", type="box")

summary(Boston$lstat)
plot_ly(data = Boston, x = ~lstat, y = ~medv)
plot_ly(data = Boston, x = ~lstat, y = ~rm)
plot_ly(data = Boston, x = ~lstat, y = ~age)
plot_ly(data = Boston, x = ~lstat, type = "histogram")
hlstat<-subset(Boston, lstat>quantile(Boston$lstat, .90))
sum(lstat>quantile(Boston$lstat, .90))
summary(hlstat, Boston)
plot_ly(data=Boston, y = ~rm, name = "Boston", type="box")  %>%
  add_boxplot(data=hlstat, y= ~rm, name = "Area with 90th percentile lstat", type="box")
plot_ly(data=Boston, y = ~crim, name = "Boston", type="box")  %>%
  add_boxplot(data=hlstat, y= ~crim, name = "Area with 90th percentile lstat", type="box")
plot_ly(data=Boston, y = ~age, name = "Boston", type="box")  %>%
  add_boxplot(data=hlstat, y= ~age, name = "Area with 90th percentile lstat", type="box")
plot_ly(data=Boston, y = ~medv, name = "Boston", type="box")  %>%
  add_boxplot(data=hlstat, y= ~medv, name = "Area with 90th percentile lstat", type="box")

any(is.na(Boston))

data(Boston)
smp_size=floor(0.75*nrow(Boston))
set.seed(12)
train_ind=sample(seq_len(nrow(Boston)), size=smp_size)
train=Boston[train_ind, ]
test=Boston[-train_ind, ]
lm.fit=lm(medv~lstat,data=train)
summary(lm.fit)

require(Metrics)
evaluate<-predict(lm.fit, test) 
rmse(evaluate,test[,14 ])

dat <- data.frame(lstat = (1:35),medv = predict(lm.fit, data.frame(lstat = (1:35))))
plot_ly() %>% 
  add_trace(x=~lstat, y=~medv, type="scatter", mode="lines", data = dat, name = "Predicted Value") %>%
  add_trace(x=~lstat, y=~medv, type="scatter", data = test, name = "Actual Value")

plot(lm.fit)

lm.fit=lm(medv~lstat+I(lstat^2),data=train)
dat=data.frame(lstat = (1:40),
                  medv = predict(lm.fit, data.frame(lstat = (1:40))))
plot_ly() %>% 
  add_trace(x=~lstat, y=~medv, type="scatter", mode="lines", data = dat, name = "Predicted Value") %>%
  add_trace(x=~lstat, y=~medv, type="scatter", data = test, name = "Actual Value")
summary(lm.fit)

lm.fit=lm(medv~crim+zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+black+lstat+I(lstat^2),data=train)
summary(lm.fit)

evaluate<-predict(lm.fit, test) 
rmse(evaluate,test[,14 ])
