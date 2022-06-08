# 
# Fitting Classification Trees (Validation Set Approach)

library(tree)
library(ISLR)
attach(Carseats)
High=factor(ifelse(Sales<=8,"No","Yes"))
Carseats=data.frame(Carseats,High)
tree.carseats=tree(High~.-Sales,Carseats)
summary(tree.carseats)
plot(tree.carseats)
text(tree.carseats,pretty=0)
tree.carseats
#
# Partition into three data sets
set.seed(123)
inTrain <- sample(nrow(Carseats), 0.5*nrow(Carseats))
#
train <- data.frame(Carseats[inTrain,])
temp <- data.frame(Carseats[-inTrain,])
inTrain2 <- sample(nrow(temp), 0.5*nrow(temp))
validation <- data.frame(temp[inTrain2,])
test <- data.frame(temp[-inTrain2,])
rm(temp)
#
# Computing the error rate on validation data (full tree)
tree.carseats=tree(High~.-Sales,data=train)
tree.pred=predict(tree.carseats,validation,type="class")
confusion = table(tree.pred,validation$High)
confusion
Error =Size <- 1:19
Error <- rep(0,length(Size))
# (confusion[1,2]+confusion[2,1])/sum(confusion)
Error
#
# Since there are 19 terminal nodes we will find the best pruned tree as follows

# For tree with one terminal node the prediction is the modal class
table(validation$High)
# There are 41 "Yes" and 59 "High"
Error[1] = 41/100
#
for (i in 2:19) {
  prune.carseats=prune.misclass(tree.carseats,best=i)
  tree.pred=predict(prune.carseats,validation,type="class")
  confusion = table(tree.pred,validation$High)
  Error[i] = (confusion[1,2]+confusion[2,1])/sum(confusion)
 }
plot(Size,Error,type = "o",xlab="Tree Size",ylab="Error Rate",col="blue")
#
# Now plot the best pruned tree
prune.carseats=prune.misclass(tree.carseats,best=which.min(Error))
plot(prune.carseats)
text(prune.carseats,pretty=0)
tree.pred=predict(prune.carseats,test,type="class")
confusion = table(tree.pred,test$High)
confusion
Error = (confusion[1,2]+confusion[2,1])/sum(confusion)
Error

