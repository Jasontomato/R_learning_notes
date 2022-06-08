
library(tree)
library(ISLR)
house <- read.csv('UniversalBank.csv')
house_dummy<-house
house_dummy[,c(6,8,10,11,12,13,14)] <- lapply(house_dummy[,c(6,8,10,11,12,13,14)],as.factor)

set.seed(12345)
train <- sample(nrow(house_dummy),0.6*nrow(house_dummy))
dftrain <- house_dummy[train,]
dfvalidation <- house_dummy[-train,]
tree.house=tree(Personal.Loan~.,dftrain)
summary(tree.house)
plot(tree.house)
text(tree.house,pretty=0)
tree.house
# confusion matrix
tree.pred=predict(tree.house,dfvalidation,type="class")
# The confusion matrix (Prediction is rows, actual is columns)
(CM = table(dfvalidation$CreditCard,tree.pred))
(Acc = (CM[1,1]+CM[2,2])/sum(CM))
# prune the tree
set.seed(1)
cv.house=cv.tree(tree.house,FUN=prune.misclass)
names(cv.house)
cv.house

