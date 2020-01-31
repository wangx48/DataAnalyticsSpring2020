multivariate<-read.csv('C:\\Users\\wxq\\Desktop\\da\\w3\\multivariate.csv')
attach(multivariate)
mm<-lm(Homeowners~Immigrant)
mm

summary(mm)$coef
summary(mm)
plot(Homeowners~Immigrant)
help(abline)
abline(mm)
abline(mm,col=2,lwd=3)
attributes(mm)
mm$coefficients

plot(mtcars$wt,mtcars$mpg)
library(ggplot2)
qplot(mtcars$wt,mtcars$mpg)
qplot(wt,mpg,data=mtcars)
ggplot(mtcars,aes(x=wt,y=mpg))+geom_point()
plot(pressure$temperature,pressure$pressure,type='l')
points(pressure$temperature,pressure$pressure)

lines(pressure$temperature,pressure$pressure/2,col='red')
lines(pressure$temperature,pressure$pressure/2,col='blue')
qplot(pressure$temperature,pressure$pressure,geom='line')
qplot(temperature,pressure,data=pressure,geom = 'line')
ggplot(pressure,aes(x=temperature,y=pressure))+geom_line()+geom_point()


barplot(BOD$demand,names.arg = BOD$Time)
help("barplot")
table(mtcars$cyl)
barplot(table(mtcars$cyl))
qplot(mtcars$cyl)
qplot(factor(mtcars$cyl))

qplot(factor(cyl),data=mtcars)
ggplot(mtcars,aes(x=factor(cyl)))+geom_bar()

hist(mtcars$mpg)
hist(mtcars$mpg,breaks = 10)
hist(mtcars$mpg,breaks = 5)
hist(mtcars$mpg,breaks = 12)
qplot(mpg,data=mtcars,binwidth=4)
ggplot(mtcars,aes(x=mpg))+geom_histogram(binwidth = 4)
ggplot(mtcars,aes(x=mpg))+geom_histogram(binwidth = 4)
ggplot(mtcars,aes(x=mpg))+geom_histogram(binwidth = 5)

plot(ToothGrowth$supp,ToothGrowth$len)
boxplot(len~supp,data=ToothGrowth)
boxplot(len~supp+dose,data=ToothGrowth)
qplot(ToothGrowth$supp,ToothGrowth$len,geom='boxplot')
qplot(supp,len,data = ToothGrowth, geom='boxplot')
ggplot(ToothGrowth,aes(x=supp,y=len))+geom_boxplot()
qplot(interaction(ToothGrowth$supp,ToothGrowth$dose),ToothGrowth$len,geom='boxplot')
qplot(interaction(supp,dose),len,data=ToothGrowth,geom = 'boxplot')
ggplot(ToothGrowth,aes(x=interaction(supp,dose),y=len))+geom_boxplot()

library(dplyr)
library(nycflights13)

head(flights)
summary(flights)

#filter
filter(flights,year ==2013,month ==10,day==1)
head(filter(flights,year ==2013,month ==10,day==1))
head(flights[flights$year==2013&flights$month==10,])

#slice
slice(flights,1:15)
help('slice')
#only slice row numbers

#arrange
arrange(flights,year,month,day,dep_time)
arrange(flights,year,desc(month),day,dep_time)

#select
select(flights,carrier)
help('select')
#select only by column
select(flights,year,carrier)
rename(flights, airline.carrier = carrier)
help('rename')

#distinct
distinct(select(flights,carrier))

#mutate
mutate(flights, MyNewColumn = arr_delay - dep_delay)
transmute(flights,MyNewColumn = arr_delay-dep_delay)
#show only new column

#summarize
summarise(flights,avg = mean(air_time,na.rm = T))
summarise(flights,sum = sum(air_time,na.rm = T))

#sample_n
sample_n(flights,20)

#sample_frac
sample_frac(flights,0.3)

df_mtcars<-mtcars
head(df_mtcars)
filter(df_mtcars,mpg>20)
sample_n(filter(df_mtcars,mpg>20),5)
arrange(sample_n(filter(df_mtcars,mpg>20),5),desc(mpg))
results<-arrange(sample_n(filter(df_mtcars,mpg>20),5),desc(mpg))


#exercise1
EPI_dataset = read.csv('C:\\Users\\wxq\\Desktop\\da\\w1\\2010EPI_data.csv',skip=1)
attach(EPI_dataset)
plot(ecdf(EPI),do.points=F,verticals=T)
help('qqnorm')
x<-seq(30,95,1)
qqplot(qt(ppoints(250),df=5),x,xlab='Q-Q plot for t dsn')
qqline(x)
help('ppoints')
ppoints(250)
help('qt')

plot(ecdf(EPI),do.points=F,verticals = T)
plot(ecdf(EPI),do.points=T,verticals = T)
par(pty='s')
qqnorm(EPI)
qqline(EPI)

x<-seq(30,95,1)
x2<-seq(30,95,2)
qqplot(qt(ppoints(250),df=5),x,xlab='Q-Q plot')
qqline(x)

plot(ecdf(EPI),do.points=F,verticals=T)
help('qqnorm')
x<-seq(30,95,1)
qqplot(qt(ppoints(250),df=5),x,xlab='Q-Q plot for t dsn')
qqline(x)
help('ppoints')
ppoints(250)
help('qt')

plot(ecdf(DALY),do.points=F,verticals = T)
plot(ecdf(DALY),do.points=T,verticals = T)
par(pty='s')
qqnorm(DALY)
qqline(DALY)

plot(ecdf(WATER_H),do.points=F,verticals = T)
plot(ecdf(WATER_H),do.points=T,verticals = T)
par(pty='s')
qqnorm(WATER_H)
qqline(WATER_H)

boxplot(EPI,DALY)
boxplot(EPI,AIR_H)
boxplot(EPI,WATER_H)
boxplot(EPI,ECOSYSTEM)
boxplot(EPI,BIODIVERSITY)
boxplot(EPI,ENVHEALTH)
boxplot(EPI,AIR_E)
boxplot(EPI,WATER_E)
