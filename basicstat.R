###    descriptive statistic
###     raulvalerio@gmail.com

#### birth 2011

ex2 = read.csv("~/Canal Yout/Stats with R/birth2011.csv", head=FALSE, ",")



# height in cm
x <- c(151, 174, 138, 186, 128, 136, 179, 163, 152, 131, 180, 185, 156, 175, 170,163)
x
x <- sort(x)   # sort data and save it
x

sum(x)  # sum of all x's elements
length(x)  # number of x's elements

mean(x)   ## sum(x)/length(x)

sd(x)   #  sqrt( sum( (x - mean(x))^2 ) / (length(x) - 1) )

hist(x)   # histogram

## how to build boxplot
xmedian= median(x)
qx <- quantile(x)

qx[2]
qx[4]

LRI =  qx[2] - 1.5* (qx[4] - qx[2])
LRS =  qx[4] + 1.5*(qx[4] - qx[2])

boxplot(x)   # boxplot

plot(x)

plot(x, main = "Height Measures", xlab = "No.element", ylab="Height (cm)")

plot(density(x))  #  x distribution

xrandom <- rnorm(50,5,2)   #  rnorm(number of elements, mean, standard deviation   )
plot(density(xrandom))  #  xrandom distribution

# weight in kg

## ejemplo 2

library(gapminder)
library(plyr)
library(dplyr)

data(gapminder)
mydata<-gapminder

head(mydata)
attach(mydata)

mean(mydata$lifeExp)

dataAfrica=mydata[continent=="Africa",]

mydata %>% 
  group_by(country) %>% 
  summarise(averagelife=mean(lifeExp))


library(ggplot2)

mydata1<- mydata %>% 
  group_by(year,continent) %>% 
  summarise(averageGPD=mean(gdpPercap))

ggplot(mydata1,aes(x=year,y=averageGPD,color=continent)) + geom_line() + geom_point()

mydata2<- mydata %>% 
  group_by(year,continent) %>% 
  summarise(averagePop=mean(pop))

ggplot(mydata2,aes(x=year,y=averagePop,color=continent)) + geom_line() + geom_point()


mydata2<- mydata%>%
  filter(gdpPercap>10000)


#mydata[mydata$country=="Albania",]

mydata %>% 
  group_by(country)%>%
  select(country, lifeExp,gdpPercap) %>% 
  summarise(
    conteo= n(),
    averagelife=mean(lifeExp),
    averageGDP=mean(gdpPercap))


## ejemplo 3
## motor trend car road test


data(mtcars)
## 0 automatic
## 1  manual

boxplot(mtcars$mpg~ mtcars$am)
boxplot(mtcars$mpg~ mtcars$cyl)

library(ggplot2)



###
mymat<- matrix(
  c (
    30,	29,	19,	27,	23,
    27, 15, 18, 13, 21,
    21,	14,	14,	8,	20,
    31,	22,	12,	14,	13,
    25,	22,	17,	20,	16,
    9,	8,	26,	23,	20,
    20,	19,	24,	24,	12,
    26, 6,  14, 13, 22,
    29,	28,	22,	23,	27,
    23,	18,	9,	22,	16),10,5)


mydata<- as.data.frame(mymat)

