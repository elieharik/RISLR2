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
bag.boston = randomForest(medv~., data = Boston, subset = train, mtry = 13, importance = TRUE)
