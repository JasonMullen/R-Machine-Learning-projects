#Lecture notes practice 
weight = c(60,0.75, 2)
#C means to combine, which allows you to make vectors
rnorm(15)
weight[3]
rnorm(15)
mean(weight)
sd(weight)

#Lab 1
mtcars
data(mtcars)
head(mtcars)
summary(mtcars)
dim(mtcars)
plot(wt,mpg) #Got an error
plot(mtcars$wt,mtcars$mpg)#Can potentially get an error if the window is too small
attach(mtcars)
detach(mtcars)
plot(mtcars$cyl,mtcars$mpg)
cyl = as.factor(mtcars$cyl)
plot(cyl,mpg)
pairs(~mpg+disp+hp+wt,mtcars)#multi-varibales
summary(mtcars$mpg)
summary(cyl)
pairs(mtcars)

#How to import a dataset
summary(ontime)
ontime$CARRIER = as.factor(ontime$CARRIER)
summary(ontime$CARRIER)
table(ontime$CARRIER)
#install.packages("ggplot2")
#library(ggplot2)
#ggplot(onetime,ase(x = DEP_DELAY_NEW, y = DISTANCE, color = CARRIER)) + geom_point{}


