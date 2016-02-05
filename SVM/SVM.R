#Download the e1071 library -> for stat learning methods functions
#Want to use the svm() function. 
#Support Vector Classifier used when kernel = "linear"

#Cost argument: if cost argument is small -> margins wide. Many support vectors will violate the margin
#Cost large: margins will be narrow.

#Generate the observations of two classes
set.seed(1)
x = matrix(rnorm(20*2), ncol = 2)
y = c(rep(-1,10), rep(1,10))
x[y==1,] = x[y==1,] +1 #change x vals according to y
plot(x, col = (3-y)) #3-y because col argument must be greater than 0


#The classes are not linearly seperable
#Let's fit the support vector classifier
#For svm to perform class (and not reg) we need to encode response as factor
#Support vector classifier fit. 
dat = data.frame(x=x, y = as.factor(y))
library(e1071)
svmfit = svm(y~., data = dat, kernel = "linear", cost = 10, scale =FALSE)
#scale = FALSE tells the svm fct not to scale each feature to have mean 0 and sd = 1
plot(svmfit, dat) #much wow. 
#Support vectors plotted as crosses, circles are remaining observations. 

#Which observations are support vectors?
svmfit$index
summary(svmfit)

#let's try using a smaller value for the cost argument. 
svmfit = svm(y~., data = dat, kernel = "linear", cost = .1, scale = FALSE)
plot(svmfit, dat)
svmfit$index

#We now have a large number of support vectors (the margin is wider)


#Can perform cross validation with tune(), built in function in e1071 library
set.seed(1)
tune.out = tune(svm, y~., data= dat, kernel = "linear", 
                ranges = list(cost = c(0.001, 0.01, 0.1, 1,5,10,100)))
summary(tune.out)
#cost = 0.1 gives us lowest cross val error rate. 

#Access the best model: 
bestmod = tune.out$best.model
summary(bestmod)

#Let's use this model and see performance on test observations: 
xtest = matrix(rnorm(20*2), ncol = 2) 
ytest = sample(c(-1,1), 20, rep = TRUE) #random sample of 1s and -1s
xtest[ytest==1,] = xtest[ytest==1,] +1
testdat = data.frame(x=xtest, y=as.factor(ytest))

#fit the model on the data using preddict
ypred = predict(bestmod, testdat)
table(predict = ypred, truth = testdat$y)


#What if have 2 classes that are linearly sepearble
#-> find sepearting hyperplne with svm function
x[y==1,] = x[y==1] + 0.5
plot(x, col = (y+5)/2, pch = 19)

#Use a very large cost so that no obs misclassified.
data = data.frame(x = x, as.factor(y))
svmfit = svm(y~., data = dat, kernel = "linear", cost = 1e5)
summary(svmfit)
plot(svmfit, dat)

#We can see that obs that are not support vectors are very close to boundary (due to small margin)
#This model might perform badly on test data

#Try another model with larger margins
svmfit2 = svm(y~., data = dat, kernel = "linear", cost = 1)
summary(svmfit2)
plot(svmfit2, dat) #1 misclassification but have 7 support vectors -> will prob perform better on test dat.




####SUPPORT VECTOR MACHINES
#Now use different parameter to kernel argument: poly or radial
set.seed(1)

#Generate data with non linear boundary 
x = matrix(rnorm(200*2), ncol = 2)
x[1:100,] = x[1:100,] +2
x[101:150,]=x[101:150,]-2
y=c(rep(1,150),rep(2,50))
dat=data.frame(x=x,y=as.factor(y))

#lot the data to make sure boundaries non linear
plot(x, col = y)

#Split the data into training and testing group
train = sample(200,100) #pick 100 random vars from 1-100 (unique)
svmfit = svm(y~., data = dat[train,], kernel = "radial", gamma = 1, cost = 1)
plot(svmfit, dat[train,])
#Dayum.

summary(svmfit)

#Some errors.. increase cost to reduce number of errors. But subject to OVERFITTING.
svmfit = svm(y~., data = dat[train,], kernel = "radial", gamma = 1, cost = 1e5)
plot(svmfit, dat[train,]) #Mother of overfits. 
summary(svmfit)


#Let's perform cross val to select the best parameters (cost and gamma)
set.seed (1)
tune.out=tune(svm, y~., data=dat[train,], kernel="radial",ranges=list(cost=c(0.1,1,10,100,1000), gamma=c(0.5,1,2,3,4)))
summary(tune.out)

#Best params: cost = 1 and gamma = 2

#See test data performance
table(true = dat[-train,"y"], pred = predict(tune.out$best.model, newdata = dat[-train,]))
#10% of test observations are misclassified by this model.





###ROC CURVES
#Write function to  plot ROC curve given vector containing numerical score for each
#obs pred + class label for every observation truth
library(ROCR)
rocplot = function(pred, truth, ...) {
  predob = prediction(pred, truth)
  perf = performance(predob, "tpr", "fpr")
  plot(perf,...)
}

#If want to obain the fitted values for SVM -> decision.values = TRUE
svmfit.opt <- svm(y~., data = dat[train,], kernel = "radial", gamma = 2, cost = 1, decision.values =T)
fitted=attributes(predict(svmfit.opt,dat[train,],decision.values=TRUE))$decision.values

par(mfrow=c(1,2))
rocplot(fitted ,dat[train ,"y"],main="Training Data")

#Increase gamma to improve accuracy
svmfit.flex=svm(y~., data=dat[train,], kernel="radial", gamma=50, cost=1, decision.values=T)
fitted=attributes(predict(svmfit.flex,dat[train,],decision.values=T))$decision.values
rocplot(fitted ,dat[train ,"y"],add=T,col="red")

#Check on test data
fitted=attributes(predict(svmfit.opt,dat[-train,],decision.values=T))$decision.values
rocplot(fitted,dat[-train,"y"],main="Test Data")
fitted=attributes(predict(svmfit.flex,dat[-train,],decision.values=T))$decision.values
rocplot(fitted,dat[-train,"y"],add=T,col="red")

#model with gamma = 2 gives us better accuracy



###What if we had multiple classes 
#One vs one approach
set.seed(1)
x = rbind(x,matrix(rnorm(50*2), ncol = 2))
x[y==0, 2] = x[y==0, 2] +2
y = c(y, rep(0,50))
dat = data.frame(x=x, y =as.factor(y))
par(mfrow = c(1,1))
plot(x,col=(y+1))


#Fit svm 
svmfit=svm(y~., data=dat, kernel="radial", cost=10, gamma=1) 
plot(svmfit , dat)



##########APPLICATION TO GENE EXP. DATA

library(ISLR)
names(Khan)
dim(Khan$xtrain)
dim(Khan$xtest)
length(Khan$ytrain)
length(Khan$ytest)


#Use linear kernel, because additional flexibility that will result from using a poly / radial kernel unncessary + large nuumber of features relative to obs. 

dat = data.frame(x = Khan$xtrain, y = as.factor(Khan$ytrain))
out = svm(y~., data = dat, kernel = "linear", cost = 10)
summary(out)

table(out$fitted , dat$y)
#No training errors. Not surprrising because large number of vars relative to observations. Easy to find hyerlanes that completely seperate classes.

#Check perf on test data
dat.te = data.frame(x=Khan$xtest, y = as.factor(Khan$ytest))
pred.te = predict(out, newdata = dat.te)
table(pred.te, dat.te$y) #Two test set erros. 
