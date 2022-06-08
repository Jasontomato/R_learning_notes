# Chapter 8 Lab: Decision Trees
# See lab in textbook
# Fitting Classification and Regression Trees

library(tree)
library(ISLR)
attach(Carseats)
names(Carseats)
# The Carseats data set does not have a categorical variable. We will create 
# a variable for high sales and add it to the dataframe
High=factor(ifelse(Sales<=8,"No","Yes"))
Carseats=data.frame(Carseats,High)
#
# The tree function can be used to build the tree. Here this is done on the full 
# data frame, but typically we will want to do this using a training set
tree.carseats=tree(High~.-Sales,Carseats)
summary(tree.carseats)
#
# To plot and all labels:
plot(tree.carseats)
text(tree.carseats,pretty=0)
tree.carseats
#
#
# The same, using a data partition follows
#
set.seed(2)
train=sample(1:nrow(Carseats), 200)
#
# The test data set is ...
Carseats.train=Carseats[train,]
Carseats.test=Carseats[-train,]
# The dependent variable ...
High.test=High[-train]
# 
# We call the tree function using the training dataset 
# Note the use of the subset option (a variation on what we have been doing)
#
tree.carseats=tree(High~.-Sales,Carseats.train)
# To predict on the test set
tree.pred=predict(tree.carseats,Carseats.test,type="class")
# The confusion matrix (Prediction is rows, actual is columns)
(CM = table(High.test,tree.pred))
(Acc = (CM[1,1]+CM[2,2])/sum(CM))
#
# We will set the seed because we will be doing cross-validation, which selects 
# partitions
set.seed(2)
#
# We will use cross-validation using the cv.tree function which will give us the 
# error rate for different tree sizes. The prune.misclass argument gives the number 
# of misclassification errors
#
cv.carseats=cv.tree(tree.carseats,FUN=prune.misclass)
names(cv.carseats)

cv.carseats
#
# In the output, $size is the tree size and $dev is the number of errors. Pick the 
# The following plots help identify best size
plot(cv.carseats$size,cv.carseats$dev,type="b")
# Here we get 5 for best tree, but note that this number will depend on the seed set
# Now prune the tree to a best size of 5
prune.carseats=prune.misclass(tree.carseats,best=5)
plot(prune.carseats)
text(prune.carseats,pretty=0)
#
# Predict using the pruned tree
tree.pred=predict(prune.carseats,Carseats.test,type="class")
CM = table(High.test,tree.pred)
(Acc = (CM[1,1]+CM[2,2])/sum(CM))
#
# You can prune to any size you want by changing the best argument ...
prune.carseats=prune.misclass(tree.carseats,best=15)
plot(prune.carseats)
text(prune.carseats,pretty=0)
tree.pred=predict(prune.carseats,Carseats.test,type="class")
CM = table(tree.pred,High.test)
(Acc = (CM[1,1]+CM[2,2])/sum(CM))

# Fitting Regression Trees
# The following to get the Boston data set
library(MASS)
#
set.seed(1)
# Partition the data set
train = sample(1:nrow(Boston), nrow(Boston)/2)
tree.boston=tree(medv~.,Boston,subset=train)
summary(tree.boston)
#
# Plot the regression tree ...
plot(tree.boston)
text(tree.boston,pretty=0)
#
# Do the cross validation and plot the deviance (RSS) against size
set.seed(1)
cv.boston=cv.tree(tree.boston)
plot(cv.boston$size,cv.boston$dev,type='b')
#
# With the seed above the best size appears to be 7
prune.boston=prune.tree(tree.boston,best=7)
plot(prune.boston)
text(prune.boston,pretty=0)
# Generate predictions on the test set
yhat=predict(tree.boston,newdata=Boston[-train,])
#
# Now that we have built our model, we can compute the test error (MSE) or RMSE
boston.test=Boston[-train,"medv"]
plot(yhat,boston.test)
abline(0,1)
# 
(MSE = mean((yhat-boston.test)^2))
(RMSE = sqrt(MSE))






