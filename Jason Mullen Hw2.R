#PROBLEM #5
library(ISLR2)
data(Auto)
auto.lm = lm(mpg ~ horsepower,data = Auto)
summary(auto.lm)
#5.A
#i)There is a relationship between Mpg and horsepower
#ii)There is a moderate positive correlation since the r^2 value is 0.6059
#iii)The relationship between the two is negative
predict(auto.lm, newdata = data.frame(horsepower = 98), interval = "p")
predict(auto.lm, newdata = data.frame(horsepower = 98), interval = "c")
#iv)The predicted mpg associated with a horsepower of 98 is 24.46708
  #The prediction interval is [14.8094, 34.12476], whihc means when one automobile has a horsepower of 98,
  #we are 95% confident that the mpg is between the given preditction interval.
  #*The confidence interval is [23.97308, 24.96108],this means that for all the automobiles that we
  #*#have a horsepower of 98 and are 95% confident that the mean mpg will be  23.97308 and 24.96108
#5.B
#Plotting the predictor
attach(Auto)
plot(horsepower, mpg)
abline(auto.lm)
#5.C
par(mfrow = c(2,2))
plot(auto.lm)
#There is a possiblity this isn't a linear relationship.

#PROBLEM 6:
#6.A scatterplot Matrix
pairs(~mpg+cylinders+displacement+horsepower+weight+acceleration+year+origin,data = Auto)
#6.B correlation Matrix
round(cor(Auto[,1:7]),3)
#6.C Multiple lienar regression with mpg
auto.new = Auto[,-9]
auto.new$origin = as.factor(auto.new$origin)
auto.new$cylinders = as.factor(auto.new$cylinders)
auto.lm = lm(mpg~., data = auto.new)
summary(auto.lm)

  #6.C.i: TestH0 :β1 =β2 =···=β6 =0againstHa :atleastoneoftheβj isnotzero. p−value≈0. Thusthere is at least one predictor associated with mpg
  #6.C.ii: For testing each one predictor separately, H0 : βj = 0 it seems that only acceleration doens't have any statistically significance in relation to mpg.
  #6.C.iii: The coefficient for the year is 0.073 so for each additional year, the mpg is predicted on average to increase by 0.073 keeping all of the other variables constant.

#6.D
auto.new2 = auto.new[,-6]
auto.lm2 = lm(mpg~., data = auto.new2)
summary(auto.lm2)

par(mfrow = c(2,2))
plot(auto.lm2)
#6.DThese plots seem to show a sufficent linear fit
#6.There is a high leverage outier of with a value of about 328

#6.E
auto.int = lm(mpg~ cylinders + displacement*horsepower + horsepower * weight + year + origin, data = auto)
summary(auto.int)
#There is potentially potentially a correlation between horsepower and horsepower displacement.However, when we include the interactions, displacement becomes irrelevant.

#PROBLEM #7
#7.a
library(ISLR2)
b.boston = NA
f.boston = NA
p.boston = NA
for (i in 1:ncol(Boston)-1) {
  lm.fit = lm(Boston$crim~Boston[,i+1])
  b.boston[i] = lm.fit$coef[2]
  f.boston = summary(lm.fit)$fstatistic
  p.boston[i] = pf(f.boston[1],f.boston[2],f.boston[3],lower.tail = F)
}
cbind(colnames(Boston[,-1]),b.boston,p.boston)
#The one predictior that seems insignificant is the one that has to do with a suburb being bounded to the Charles river. All others are important
par(mfrow = c(3,4))
for(i in 1:12 ){
  plot(Boston[,i+1], Boston$crim, xlab = "", ylab = "Crim")
}
#7.b
lm.fit = lm(crim ~ ., data = Boston)
summary(lm.fit)
summary(lm(crim ~ zn + nox + dis + rad + ptratio + lstat + medv, data = Boston))
#Regarding the analysis it seems that zn, nox, dis, rad, ptratio, lstat, and medv are important predictors regarding crime
#7.c
plot(b.boston, lm.fit$coefficients[-1], xlab = "Simple Linear Regression", ylab = "Mulitple Linear Regression")
#7.d(WILL HAVE TO SKIP)
install.packages("stargazer")
library(stargazer)
lm.zn = lm(crim ~ poly(zn, 3), data = Boston)
lm.nox = lm(crim ~ poly(nox), data = Boston)
lm.dis = lm(crim ~ poly(dis, 3), data = Boston)
lm.rad = lm(crim ~ poly(rad, 3), data = Boston)
lm.ptratio = lm(crim ~ poly(ptratio, 3), data = Boston)
lm.lstat = lm(crim ~ poly(lstat, 3), data = Boston)
lm.medv = lm(crim ~ poly(medv, 3), data = Boston)
stargazer(lm.zn,lm.nox,lm.dis,lm.rad,lm.ptratio,lm.lstat,lm.medv,
          title = "Polynomial Models", font.size = "scriptsize")

#PROBLEM #8
set.seed(1)
x1 = runif(100)
x2 = 0.5 * x1+rnorm(100) /10
y = 2+2 * x1 + 0.3 * x2+rnorm(100)
#8.a answer: The linear model is: y = β0 + β1x1 + β2x2 + ε.
#8.a answer: The regression coefficients are: β0 = 2, β1 = 2 and β2 = 0.3.


#8.b
cor(x1, x2)
plot(x1, x2)

#8.c
summary(lm(y ~ x1 + x2))
#8.c answers:
  #FortestingH0 :β1 =β2 =0 against Ha :at least one βj isnot zero.We ultimately get a p-value close to zero. Thus at least one of the variables x1 or x2 is related to y.
  #βˆ0 = 2.1305, βˆ1 = 1.439,  βˆ2 = 1.0097
  #From the actual values of β0, β1, and β2. This estimate is close for β0 and somewhat to β1 but not for β2.
  #For testing H0 : β1 = 0 we reject that hypothesis with a p-value = 0.0487.
  #For testing H0 : β2 = 0 we fail to reject the null hypothesis with a p-value = 0.3754

#8.d
summary(lm(y~x1))
#βˆ0 and βˆ1 are close to the original coefficients.
#If we test H0 : β1 = 0 we would reject the null hypothesis.

#8.e
summary(lm(y~x2))
#This shows that x2 is associated with y by rejecting H0 : β2 = 0.

#8.f
#They do not contradict each other because if x1 is used in the model to predict y then we technically don't need x2. This is true because x2 was calculated based on x1. Thus the results in 8.c and 8.e don't contradict each other
summary(lm(y~x1 + x2))
summary(lm(y~x2))
#This causes changes in the β1 and β2 estimates.
par(mfrow = c(2,2))
#x1 + x2 plot
plot(lm(y~ x1 + x2))
#x1 plot
plot(lm(y ~ x1))
#x2 plot
plot(lm(y~x2))
#What I noticed is that the extra point has high leverage.


#PROBLEM 9
#9.a: Generating X and epsilon
set.seed(1)
X = rnorm(100)
e = rnorm(100)

#9.b GenerateY. Letβ0 =2,β1 =0.5,β2 =−0.75andβ3 =5.
Y = 2 + 0.5 * X - 0.75 * X^2 + 5*X^2 + e

#9.c
install.packages("leaps")
library(leaps)
new.data = data.frame(cbind(Y,X))
fit.y = regsubsets(Y ~ poly(X,10), data = new.data)
(fit.res = summary(fit.y))
fit.stat = cbind(fit.res$adjr2, fit.res$cp, fit.res$bic)
colnames(fit.stat) = c("Adjr2", "Cp", "BIC")
print(fit.stat)
#9.c
#The model with the 4th degree appears to be the best subset.
par(mfrow = c(2,2))
plot(lm(Y~ poly(X,4)))
plot(lm(Y ~ poly(X,5)))

#9.d Using stepwise selections
step(lm(Y~ poly(X, 10)), direction = "backward")
step(lm(Y~poly(X,10)), direction = "forward")

