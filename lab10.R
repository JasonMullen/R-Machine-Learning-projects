installed.packages("ISLR")
library(ISLR2)

alpha.fn = function(data,index) {
  X = data$X[index]
  Y = data$Y[index]
  return((var(Y) - cov(X,Y))/(var(X) + var(Y) - 2*cov(X,Y)))
}

alpha.fn(Portfolio, 1:100)
#Answer:0.5758321
#Question #1
set.seed(10)
alpha.fn(Portfolio, sample(100, 100, replace = TRUE))
#Answer: [1] 0.508921


#Question #2
install.packages("boot")
library(boot)
alpha.boot = boot(Portfolio,alpha.fn, R = 1000)
alpha.boot
mean(alpha.boot$t)
#Answer: 0.5828526


#Question #3
mean.boot = mean(alpha.boot)
sd.boot = sd(alpha.boot$t)
mean.boot
#Answer: The original estiamte of alpha is :0.5758321. The SE(alpha) is 0.09220045,and then the bootstrap estimate is 0.580996237


#Question #4
#Answer:It shows that the standard error is percise.


#Question #5
boot.fn = function(data,index)
  return(coef(lm(mpg~horsepower, data = data, subset = index)))
boot.fn(Auto, 1:392)
#Answer:(Intercept)  horsepower
        #39.9358610  -0.1578447


#Question #6
boot.fn(Auto,sample(392, 392,replace = TRUE))
boot.out = boot(Auto,boot.fn,1000)
boot.out
#Answer: (Intercept)  horsepower
          #40.7585298  -0.1633531
#Bootstrap Statistics :
#  original        bias    std. error
#t1* 39.9358610  0.0099757801 0.858173569
#t2* -0.1578447 -0.0001529907 0.007391526


#Question 7
summary(lm(mpg~horsepower, data = Auto))$coef
#Answer:
# Estimate  Std. Error   t value      Pr(>|t|)
#(Intercept) 39.9358610 0.717498656  55.65984 1.220362e-187
#horsepower  -0.1578447 0.006445501 -24.48914  7.031989e-81

#Question 8:
#The reason their is a differnence in the standard errors is because the y-intercept is exponentially more precise.

