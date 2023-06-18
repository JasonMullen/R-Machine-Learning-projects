#Question #1

#Question 3
library(ISLR)
?Carseats
High = ifelse(Carseats$Sales <= 8, "No","Yes")
High = as.factor(High)
Carseats = data.frame(Carseats,High)
rm(Carseats)
#install.packages("tree")
library(tree)
tree.carseats = tree(High~. -Sales,Carseats)
summary(tree.carseats)

#Question 5
plot(tree.carseats)
text(tree.carseats,pretty = 0)
#Question 6
set.seed(2)
train = sample(1:nrow(Carseats),200)
Carseats.test = Carseats[-train,]
High.test = High[-train]
tree.carseats = tree(High ~ . -Sales, Carseats, subset = train)
tree.pred = predict(tree.carseats, Carseats.test, type = "class")
table(tree.pred, High.test)
#Question 7
set.seed(3)
cv.carseats = cv.tree(tree.carseats, FUN = prune.misclass)
cv.carseats

plot(cv.carseats$size,cv.carseats$dev, type = "b")


#Question 8
prune.carseats = prune.misclass(tree.carseats, best = 8)
plot(prune.carseats)
text(prune.carseats, pretty = 0)
tree.pred = predict(prune.carseats, Carseats.test, type = "class")
table(tree.pred, High.test)



#Question 9
library(MASS)
set.seed(1)
train = sample(1: nrow(Boston), nrow(Boston)/2)
tree.boston = tree(medv ~., Boston, subset = train)
summary(tree.boston)
#Question 11
plot(tree.boston)
text(tree.boston, pretty = 0)
#Question 12
cv.boston = cv.tree(tree.boston)
plot(cv.boston$size, cv.boston$dev, type = "b")
tree.boston
#Question 13
prune.boston = prune.tree(tree.boston, best = 5)
plot(prune.boston)
text(prune.boston, pretty = 0)

yhat = predict(tree.boston, newdata = Boston[-train,])
boston.test = Boston[-train, "medv"]
plot(yhat, Boston.test)
abline(0,1)
mean((yhat - boston.test) ^2)






