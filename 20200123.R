#Week2
#dataframe 

days<-c('Mon','Tue','Wed','Thur','Fri','Sat','Sun')
temp<-c(28,30.5,32,31.2,29.3,27.9,26.4)
snowed<-c('T','T','F','F','T','T','F')
help('data.frame')
RPI_Weather_Week<-data.frame(days,temp,snowed)
RPI_Weather_Week
head(RPI_Weather_Week)
str(RPI_Weather_Week)
summary(RPI_Weather_Week)

RPI_Weather_Week[1,]
RPI_Weather_Week[,1]
RPI_Weather_Week[,'snowed']
RPI_Weather_Week[1:5,c('days','temp')]
RPI_Weather_Week$temp
subset(RPI_Weather_Week,subset = snowed==TRUE)

sorted.snowed<-order(RPI_Weather_Week['snowed'])
RPI_Weather_Week[sorted.snowed,]

dec.snow<-order(-RPI_Weather_Week$temp)
empty.DataFrame<-data.frame()
v1<-1:10
letters
v2<-letters[1:10]
df<-data.frame(col.name.1 = v1, col.name.2=v2)

write.csv(df,file = 'saved.df1.csv')
df2<-read.csv('saved.df1.csv')

GPW3<-read.csv('C:\\Users\\wxq\\Desktop\\da\\w2\\GPW3_GRUMP_SummaryInformation_2010.csv')
EPI_data<-read.csv('C:\\Users\\wxq\\Desktop\\da\\w1\\2010EPI_data.csv',header = TRUE,skip = 1)
help('read.csv')
data()

View(EPI_data)

EPI
tf<-is.na(EPI)
E<-EPI[!tf]
E
summary(EPI)
fivenum(EPI,na.rm = TRUE)
stem(EPI)
hist(EPI)
hist(EPI, seq(30.,95.,1.0),prob=TRUE)
lines(density(EPI, na.rm=TRUE,bw=1.))
lines(density(EPI, na.rm=TRUE,bw='SJ'))
rug(EPI)

plot(ecdf(EPI),do.points=FALSE, verticals = TRUE)
par(pty='s')
help('par')
qqnorm(EPI)
qqline(EPI)

x<-seq(30,95,1)
qqplot(qt(ppoints(250),df=5),x,xlab='Q-Q plot for t dsn')
qqline(x)
help('qqplot')

DALY
tf<-is.na(DALY)
E<-DALY[!tf]
E
summary(DALY)
fivenum(DALY,na.rm = TRUE)
stem(DALY)
hist(DALY)
hist(DALY, seq(0.,92.,1.0),prob=TRUE)
lines(density(DALY, na.rm=TRUE,bw=1.))
lines(density(DALY, na.rm=TRUE,bw='SJ'))
help('density')
rug(EPI)

plot(ecdf(DALY),do.points=FALSE, verticals = TRUE)
par(pty='s')
help('par')
qqnorm(DALY)
qqline(DALY)

WATER_H
summary(WATER_H)
fivenum(WATER_H,na.rm = TRUE)
stem(WATER_H)
hist(WATER_H)
hist(WATER_H, seq(0.,100.,1.0),prob=TRUE)
lines(density(WATER_H, na.rm=TRUE,bw=1.))
lines(density(WATER_H, na.rm=TRUE,bw='SJ'))
rug(WATER_H)

plot(ecdf(WATER_H),do.points=FALSE, verticals = TRUE)
par(pty='s')
qqnorm(WATER_H)
qqline(WATER_H)

boxplot(EPI, DALY)

qqplot(EPI, DALY)

boxplot(EPI, ENVHEALTH, ECOSYSTEM, DALY, AIR_H, WATER_H, BIODIVERSITY)

help("distributions")

EPILand<-EPI[!Landlock]
EPILand
Eland<-EPILand[!is.na(EPILand)]
Eland
hist(Eland)
hist(Eland,seq(30.,95.,1.0),prob=TRUE)
lines(density(Eland, na.rm=TRUE,bw=1.))
lines(density(Eland, na.rm=TRUE,bw='SJ'))
rug(Eland)
plot(ecdf(Eland),do.points=FALSE, verticals = TRUE)
qqnorm(Eland)
qqline(Eland)

