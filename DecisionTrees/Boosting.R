#Use gbm package / gbm() function to fit boosted regression trees

#Use Dist. = "gaussian" since regression problem. If classification, use "bernoulli"
#n.trees = 5000 -> 5000 trees
#int.dep = 4 limits depth of each tree

library(gbm)
set.seed (1)
boost.boston=gbm(medv~.,data=Boston[train,],distribution= "gaussian",n.trees=5000, interaction.depth=4)

#Summary:
summary(boost.boston)

#Show partial dependence plots for most important vras lstat and rm 
#Plots show marginal effect of selected vars on the response after integrating the other vars. 
par(mfrow=c(1,2)) 
plot(boost.boston ,i="rm") 
plot(boost.boston ,i="lstat")

#Use boosted model to predict medv
yhat.boost=predict(boost.boston,newdata=Boston[-train,], n.trees=5000)
mean((yhat.boost -boston.test)^2)
