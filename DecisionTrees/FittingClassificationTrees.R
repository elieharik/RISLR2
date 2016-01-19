#Use the tree library for class / reg trees
library(tree)

#Using classification
#Need to turn Sales into binary variable
library(ISLR)
attach(Carseats)
High = ifelse(Sales<=8, "No", "Yes")

#Use the data.frame() function to merge the High variable with the Carseats data
Carseats = data.frame(Carseats, High)

#Fit a classification tree in order to predict High using all vars except Sales
tree.carseats = tree(High~. -Sales, Carseats)

#List the variables that are used as internal nodes in the tree 
# the number of terminal nodes and the (training) error rate. 
summary(tree.carseats)
#Small deviance -> tree gives us a good fit to the TRAINING data

#Plot that tree
plot(tree.carseats)
#Add that node text
text(tree.carseats, pretty = 0) #Not sure what that pretty does though

#Let's look at the output corresponding to every branch of tree
tree.carseats

#Let's see how the tree performs on test data
set.seed (2)
train=sample(1:nrow(Carseats), 200)
Carseats.test=Carseats [-train ,]
High.test=High[-train]
tree.carseats=tree(High~.-Sales,Carseats,subset=train)
tree.pred=predict(tree.carseats,Carseats.test,type="class")
table(tree.pred ,High.test)
#0.715 correct classification

#Will pruning the tree lead to a lower test error rate
#cv.tree functon does CV in order to get optimal level of tree complexity
#argument FUN = prune.misclass -> want classification error rate to guide CV+pruning
#(Usually it's deviance)
set.seed(3)
cv.carseats = cv.tree(tree.carseats, FUN = prune.misclass)
names(cv.carseats)
cv.carseats
#dev gives you CV error rate
#tree with 9 terminal nodes gives you lowest error rate
par(mfrow = c(1,2))
#Plot error rate as function of size and k (alpha in R)
plot(cv.carseats$size, cv.carseats$dev, type = "b")
plot(cv.carseats$k, cv.carseats$dev, type = "b")

#Perform prune.misclass function to prune the tree to get nine node tree
prune.carseats = prune.misclass(tree.carseats, best = 9)
plot(prune.carseats)
text(prune.carseats, pretty = 0)

#How well does the prune tree perform on the test data?
tree.pred = predict(prune.carseats, Carseats.test, type = "class")
table(tree.pred, High.test)
#0.77
#Pruning  has improved accuracy of tree + improved interpretability
#increasing the value of best would reduce the classification accuracy
#Reminder: With pruning, start with a very large tree and then prune it back to obtain a subtree


