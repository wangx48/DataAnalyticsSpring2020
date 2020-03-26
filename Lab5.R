#Xiuqi Wang
#lab5

wine_data = read.table('http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data',sep=',')
head(wine_data)
nrow(wine_data)
dim(wine_data)
colnames(wine_data)=c("Cvs", "Alcohol", 
                      "Malic_Acid", "Ash", "Alkalinity_of_Ash", 
                      "Magnesium", "Total_Phenols", "Flavanoids", "NonFlavanoid_Phenols",
                      "Proanthocyanins", "Color_Intensity", "Hue", "OD280/OD315_of_Diluted_Wine", 
                      "Proline")
head(wine_data)
help('heatmap')
heatmap(cor(wine_data),Rowv = NA, Colv = NA)
help('factor')
cultivar_classes = factor(wine_data$Cvs)
cultivar_classes
help('prcomp')
wine_data_PCA = prcomp(scale(wine_data[,-1]))
summary(wine_data_PCA)

library(gcookbook)
library(ggplot2)
ggplot(BOD,aes(x=Time,y=demand))+geom_line()
BOD
BOD1 = BOD
BOD1$Time = factor(BOD1$Time)
ggplot(BOD1,aes(x=Time,y=demand,group=1))+geom_line()
ggplot(BOD1,aes(x=Time,y=demand,group=1))+geom_line()+ylim(0,max(BOD$demand))
ggplot(BOD1,aes(x=Time,y=demand,group=1))+geom_line()+expand_limits(y=0)

ggplot(BOD1,aes(x=Time,y=demand,group=1))+geom_line()+geom_point()
ggplot(worldpop,aes(x=Year,y=Population))+geom_line()+geom_point()
ggplot(worldpop,aes(x=Year,y=Population))+geom_line()+geom_point()+scale_y_log10()
help(aes)
