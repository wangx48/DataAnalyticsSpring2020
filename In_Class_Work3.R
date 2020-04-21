rm(list=ls())

data(economics,package = 'ggplot2')
economics$index=1:nrow(economics)
economics=economics[1:80,]
loessMod10=loess(uempmed~index,data=economics,span=0.1)
loessMod25=loess(uempmed~index,data=economics,span=0.25)
loessMod50=loess(uempmed~index,data=economics,span=0.5)

smoothed10=predict(loessMod10)
smoothed25=predict(loessMod25)
smoothed50=predict(loessMod50)

plot(economics$uempmed,x=economics$date,type='l',
     main='Loess Smoothing and Prediction',
     xlab='Date',ylab='Unemployment(Median)')

lines(smoothed10,x=economics$date,col='red')
lines(smoothed25,x=economics$date,col='green')
lines(smoothed50,x=economics$date,col='blue')


data(cars,package = 'datasets')
str(cars)
plot(speed~dist,data=cars)

help(lowess)
lowess(cars$speed~cars$dist)
lines(lowess(cars$speed~cars$dist,f=2/3),col='blue')
lines(lowess(cars$speed~cars$dist,f=0.75),col='gray')
lines(lowess(cars$speed~cars$dist,f=0.8),col='red')
lines(lowess(cars$speed~cars$dist,f=0.9),col='green')
lines(lowess(cars$speed~cars$dist,f=0.1),col=5)
lines(lowess(cars$speed~cars$dist,f=0.01),col=6)

#https://rafalab.github.io/dsbook/smoothing.html
library(tidyverse)
library(dslabs)
data("polls_2008")
qplot(day,margin,data=polls_2008)

span=7
fit=with(polls_2008,
         ksmooth(day,margin,kernel = 'box',bandwidth = span))
polls_2008%>%mutate(smooth=fit$fitted)%>%ggplot(aes(day,margin))+
  geom_point(size=3,alpha=.5,color='grey')+
  geom_line(aes(day,smooth),color='red')

span=7
fit=with(polls_2008,
         ksmooth(day,margin,kernel = 'normal',bandwidth = span))
polls_2008%>%mutate(smooth=fit$y)%>%ggplot(aes(day,margin))+
  geom_point(size=3,alpha=.5,color='grey')+
  geom_line(aes(day,smooth),color='red')
#smoother

total_days=diff(range(polls_2008))
total_days
span=21/total_days
fit=loess(margin~day,degree=1,span=span,data=polls_2008)
polls_2008%>%mutate(smooth=fit$fitted)%>%ggplot(aes(day,margin))+
  geom_point(size=3,alpha=.5,color='grey')+
  geom_line(aes(day,smooth),color='red')

total_days=diff(range(polls_2008))
total_days
span=28/total_days
fit1=loess(margin~day,degree=1,span=span,data=polls_2008)
fit2=loess(margin~day,span=span,data=polls_2008)
polls_2008%>%mutate(smooth1=fit1$fitted,smooth2=fit2$fitted)%>%ggplot(aes(day,margin))+
  geom_point(size=3,alpha=.5,color='grey')+
  geom_line(aes(day,smooth1),color='red',lty=2)+
  geom_line(aes(day,smooth2),color='orange',lty=1)

polls_2008%>%ggplot(aes(day,margin))+
  geom_point()+
  geom_smooth()

polls_2008%>%ggplot(aes(day,margin))+
  geom_point()+
  geom_smooth(span = 0.15, method.args = list(degree=1),method='loess')
help("geom_smooth")

library(tidyverse)
library(purrr)
library(pdftools)
library(dslabs)
library(lubridate)
fn=system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf",package="dslabs")
dat <- map_df(str_split(pdf_text(fn), "\n"), function(s){
  s <- str_trim(s)
  header_index <- str_which(s, "2015")[1]
  tmp <- str_split(s[header_index], "\\s+", simplify = TRUE)
  month <- tmp[1]
  header <- tmp[-1]
  tail_index  <- str_which(s, "Total")
  n <- str_count(s, "\\d+")
  out <- c(1:header_index, which(n == 1), 
           which(n >= 28), tail_index:length(s))
  s[-out] %>%  str_remove_all("[^\\d\\s]") %>% str_trim() %>%
    str_split_fixed("\\s+", n = 6) %>% .[,1:5] %>% as_tibble() %>% 
    setNames(c("day", header)) %>%
    mutate(month = month, day = as.numeric(day)) %>%
    gather(year, deaths, -c(day, month)) %>%
    mutate(deaths = as.numeric(deaths))
}) %>%
  mutate(month = recode(month, 
                        "JAN" = 1, "FEB" = 2, "MAR" = 3, 
                        "APR" = 4, "MAY" = 5, "JUN" = 6, 
                        "JUL" = 7, "AGO" = 8, "SEP" = 9, 
                        "OCT" = 10, "NOV" = 11, "DEC" = 12)) %>%
  mutate(date = make_date(year, month, day)) %>%
  filter(date <= "2018-05-01")

library(broom)  
data("mnist_27")
mnist_27$train%>%
  glm(y~x_2,family='binomial',data=.)%>%tidy()

qplot(x_2,y,data=mnist_27$train)

library(MASS)
names(iris)
dim(iris)
head(iris)
set.seed(555)
train=sample(1:nrow(iris),nrow(iris)/2)
iris_train=iris[train,]
iris_test=iris[-train,]
help(lda)
fit1=lda(Species~.,data = iris_train)
predict1=predict(fit1,iris_train)
predict1_class=predict1$class

table1=table(predict1_class,iris_train$Species)
table1

sum(diag(table1))/sum(table1)
