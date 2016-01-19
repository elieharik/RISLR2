library(MASS)
set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston)/2)
tree.boston = tree(medv~., Boston, subset = train)
summary(tree.boston)

#Only using three of the vars: lstat, rm and dis
#Deviance is the sum of squared o errors for tree in context of reg tree

par(mfrow = c(1,1))
plot(tree.boston)
text(tree.boston, pretty = 0)

#Check if pruning will improve the tree
cv.boston = cv.tree(tree.boston) #Give the function your tree
plot(cv.boston$size, cv.boston$dev, type = "b")
#Most complex tree would be selected by CV

prune.boston = prune.tree(tree.boston, best = 5)
plot(prune.boston)
text(prune.boston, pretty = 0) #Take the pruned tree of size 5

#Following results of CV, use unpruned tree (most complex) for data set
yhat = predict(tree.boston, newdata = Boston[-train,])
boston.test = Boston[-train, "medv"] # medv -> median value of owner-occupied homes in $1000
plot(yhat, boston.test)
abline(0,1)
mean((yhat - boston.test)^2)
