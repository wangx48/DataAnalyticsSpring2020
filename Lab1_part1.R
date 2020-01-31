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

EPI_data<-read.csv('C:\\Users\\wxq\\Desktop\\da\\w1\\2010EPI_data.csv',header = TRUE,skip = 1)
help('read.csv')
data()

View(EPI_data)
attach(EPI_data)
fix(EPI_data)
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
summary(Eland)
fivenum(Eland,na.rm = TRUE)
stem(Eland)
hist(Eland)
hist(Eland,seq(30.,95.,1.0),prob=TRUE)
lines(density(Eland, na.rm=TRUE,bw=1.))
lines(density(Eland, na.rm=TRUE,bw='SJ'))
rug(Eland)
plot(ecdf(Eland),do.points=FALSE, verticals = TRUE)
qqnorm(Eland)
qqline(Eland)

summary(No_surface_water)
fivenum(No_surface_water,na.rm = TRUE)
stem(No_surface_water)
hist(No_surface_water)
hist(No_surface_water,seq(0.,1.,0.1),prob=TRUE)
lines(density(No_surface_water, na.rm=TRUE,bw=1.))
rug(No_surface_water)
plot(ecdf(No_surface_water),do.points=FALSE, verticals = TRUE)
qqnorm(No_surface_water)
qqline(No_surface_water)

summary(Desert)
fivenum(Desert,na.rm = TRUE)
stem(Desert)
hist(Desert)
hist(Desert,seq(0.,1.,0.1),prob=TRUE)
lines(density(Desert, na.rm=TRUE,bw=1.))
rug(Desert)
plot(ecdf(Desert),do.points=FALSE, verticals = TRUE)
qqnorm(Desert)
qqline(Desert)

summary(High_Population_Density)
fivenum(High_Population_Density,na.rm = TRUE)
stem(High_Population_Density)
hist(High_Population_Density)
hist(High_Population_Density,seq(0.,1.,0.1),prob=TRUE)
lines(density(High_Population_Density, na.rm=TRUE,bw=1.))
rug(High_Population_Density)
plot(ecdf(High_Population_Density),do.points=FALSE, verticals = TRUE)
qqnorm(High_Population_Density)
qqline(High_Population_Density)

EPI_South_Asia<-EPI[EPI_regions=='South Asia']

GPW3<-read.csv('C:\\Users\\wxq\\Desktop\\da\\w2\\GPW3_GRUMP_SummaryInformation_2010.csv')
attach(GPW3)
summary(Continent)
fivenum(Continent,na.rm = TRUE)
stem(Continent)
hist(Continent)
hist(Continent,seq(1,6,1),prob=TRUE)
lines(density(Continent, na.rm=TRUE,bw=1.))
rug(Continent)
plot(ecdf(Continent),do.points=FALSE, verticals = TRUE)
qqnorm(Continent)
qqline(Continent)

water<-read.csv('C:\\Users\\wxq\\Desktop\\da\\w2\\water-treatment.csv')
attach(water)
fix(water)

summary(PH.E)
fivenum(PH.E,na.rm = TRUE)
stem(PH.E)
hist(PH.E)
hist(PH.E,seq(6.9,8.7,0.2),prob=TRUE)
lines(density(PH.E, na.rm=TRUE,bw=1.))
rug(PH.E)
plot(ecdf(PH.E),do.points=FALSE, verticals = TRUE)
qqnorm(PH.E)
qqline(PH.E)