install.packages("ISLR2")
library(ISLR2)
#Task 1
head(Boston)
#Task 2
plot(Boston$rm, Boston$medv, xlab = "rm", ylab = "medv")
res <- cor.test(Boston$rm, Boston$medv, method = "pearson")
res
#Task 3
which.max(Boston$rm)
which.min(Boston$rm)
#Task 4
lm.fit <-lm(medv ~ rm, data = Boston)
summary(lm.fit)
coef(lm.fit)
#Taske 5
confint(lm.fit)
#Task 6 
predict(lm.fit, data.frame(rm = c(5,6,7)))
predict(lm.fit, data.frame(rm = c(5,6,7)), 
      interval = "confidence")
predict(lm.fit, data.frame(rm = c(5,6,7)),
        interval = "prediction")
#Task 7
par(mfrow = c(2,2))
plot(lm.fit)
which.max(hatvalues(lm.fit))#used to find extreme values
#How to find the largest leverage
Boston[366,]

