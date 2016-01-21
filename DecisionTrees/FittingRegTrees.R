library(MASS)
library(tree)
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




#Bagging

#Bagging is a special case of a random forest with m = p 
#Bagging <=> Bootsrappingish method particularly useful for decision trees
#Generate B different bootsrapped training data sets and average all the pridctions

#Random trees > Bagged tree. Improvement because every time a split in tree is considered, 
#a random sample of m preds is chosen as split candidates from the full set of p predictors. 
#Typically take m = sqrt(p)
##Useful when there is a very strong predictor in the data set, and other predictors are meh. 
#Bagged trees will have that top predictor in the top split -> all predictors will look similiar
#Predictions from the bagged trees (avg) wll be strongly correlated -> bagging will not 
#lead to a substantialreduction in variance in this scenario. 

#Random trees DECORRELATE the trees -> also useful when have a large # of correlated predictors.

library(randomForest)
library(MASS)
set.seed(1)
#mtry = 13 -> all 13 preds should be considered for each split  of tree 
#IOW (In Other Words) -> bagging should be done
bag.boston = randomForest(medv~., data = Boston, subset = train, mtry = 13, importance = TRUE)
bag.boston


#Check how well bagging performs on the test set
yhat.bag = predict(bag.boston, newdata = Boston[-train,]) ####!!newdata and not newData!! will give diff results
plot(yhat.bag, boston.test)
abline(0,1)
mean((yhat.bag-boston.test)^2) #13.4

#random tree is the exact same, except use smaller number (not full set of predictors)
#for the mtry argument. 
set.seed(1)
rf.boston = randomForest(medv~., data = Boston, subset = train, mtry = 6, importance = TRUE)
yhat.rf = predict(rf.boston, newdata = Boston[-train,])
mean((yhat.rf-boston.test)^2) #11.22

#Use the importance function to view the imp of each variable
importance(rf.boston)
#IncMSE -> mean decrease of accuracy in predictions on the out of bag samples when 
#given variable is excluded from the model 
#IncNodePurity -> measure of otal decrease in node purity that results from splits over that var
#Averaged over all trees. 

#For reg trees -> node imp. is measured by training RSS
#For class -> deviance
#Reminder: node purity: a small value indicates that a node contains predominantly obs from a single class. 

#Plot importance measures
varImpPlot(rf.boston) #Sweet



#Boosting.
#Use the gbm() function to fit boosted reg tree to Boston data set. 
#n.trees = 5000: we want 5000 trees and limit depth of tree to 4

library(gbm)
set.seed(1)
boost.boston = gbm(medv~., data =Boston[train, ], distribution = "gaussian",
                   n.trees = 5000, interaction.depth =4)

summary(boost.boston)
#lstat and rm are most important vars. 

#Partial dependence plots -> show marginal effect of selected vars on the response
#after integrating out other vars. 

par(mfrow=c(1,2))
plot(boost.boston, i = "rm")
plot(boost.boston, i = "lstat")


#Predict
yhat.boost=predict(boost.boston,newdata=Boston[-train,], n.trees=5000)
mean((yhat.boost -boston.test)^2)
#Better than bagging

#We could boost with different lamba param
boost.boston=gbm(medv~.,data=Boston[train,],distribution= "gaussian",
                 n.trees=5000, interaction.depth=4,shrinkage =0.2, verbose =F)
yhat.boost=predict(boost.boston,newdata=Boston[-train,], n.trees=5000)
mean((yhat.boost -boston.test)^2)
