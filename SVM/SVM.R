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
