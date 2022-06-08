#
# Read the data. 
#
# setwd('G:\\ÎÒµÄÔÆ¶ËÓ²ÅÌ\\Graduate course\\758T Data Mining\\Course Practise')
df <- read.csv("UniversalBank.csv")
df[,1] <- NULL
df$ZIP.Code <- NULL
#
# Now we need to normalize each variable
fun <- function(x){ 
  a <- mean(x) 
  b <- sd(x) 
  (x - a)/(b) 
} 
df[,1:7] <- apply(df[,1:7], 2, fun)
df[,9:12] <- apply(df[,9:12], 2, fun)
#
library("caret")
set.seed(12345)
inTrain <- createDataPartition(df$Personal.Loan, p=0.6, list=FALSE)
#
dftrain <- data.frame(df[inTrain,])
dftemp <- data.frame(df[-inTrain,])
inVal <- createDataPartition(dftemp$Personal.Loan, p=0.6, list=FALSE)
dfvalidation <- data.frame(dftemp[inVal,])
dftest <- data.frame(dftemp[-inVal,])
#
# knn() may be found in the library class
library(class)
# 
train_input <- as.matrix(dftrain[,-8])
train_output <- as.vector(dftrain[,8])
validate_input <- as.matrix(dfvalidation[,-8])
test_input <- as.matrix(dftest[,-8])
#
#
kmax <- 15
ER1 <- rep(0,kmax)
ER2 <- rep(0,kmax)
#
for (i in 1:kmax){
prediction <- knn(train_input, train_input,train_output, k=i)
prediction2 <- knn(train_input, validate_input,train_output, k=i)
prediction3 <- knn(train_input, test_input,train_output, k=i)
#
# The confusion matrix for training data is:
CM1 <- table(prediction, dftrain$Personal.Loan)
# The training error rate is:
ER1[i] <- (CM1[1,2]+CM1[2,1])/sum(CM1)
# The confusion matrix for validation data is: 
CM2 <- table(prediction2, dfvalidation$Personal.Loan)
ER2[i] <- (CM2[1,2]+CM2[2,1])/sum(CM2)
}
plot(c(1,kmax),c(0,0.1),type="n", xlab="k",ylab="Error Rate")
lines(ER1,col="red")
lines(ER2,col="blue")
legend(9, 0.1, c("Training","Validation"),lty=c(1,1), col=c("red","blue"))
z <- which.min(ER2)
cat("Minimum Validation Error k:", z)
#
# Scoring at optimal k
prediction <- knn(train_input, train_input,train_output, k=z)
prediction2 <- knn(train_input, validate_input,train_output, k=z)
prediction3 <- knn(train_input, test_input,train_output, k=z)
#
CM1 <- table(prediction, dftrain$Personal.Loan)
CM2 <- table(prediction2, dfvalidation$Personal.Loan)
CM3 <- table(prediction3, dftest$Personal.Loan)
CM1
CM2
CM3
ER3 <- (CM3[1,2]+CM3[2,1])/sum(CM3)
ER3
#
# 
# Now we compute the lift curve for k=15. 
prediction3 <- knn(train_input, test_input, train_output, k=15, prob=T)
#
predicted.probability <- attr(prediction3, "prob") # probability of success
# 
# This (unfortunately returns the proportion of votes for the winning class - P(Success))
#
predicted.probability <- ifelse(prediction3 ==1, predicted.probability, 1-predicted.probability)
#
df1 <- data.frame(prediction3, predicted.probability,dftest$Personal.Loan)
# When prediction is 1, we will use predicted.probability; else use 1-predicted.probability
df1S <- df1[order(-predicted.probability),]
df1S$Gains <- cumsum(df1S$dftest.Personal.Loan)
plot(df1S$Gains,type="n",main="Lift Chart",xlab="Number of Cases",ylab="Cumulative Success")
lines(df1S$Gains,col="blue")
abline(0,sum(df1S$dftest.Personal.Loan)/nrow(df1S),lty = 2, col="red")

library(pROC)
dftest$Personal.Loan <- factor(dftest$Personal.Loan)
prediction3 <- knn(train_input, test_input, train_output, k=15, prob=T)
#
predicted.probability <- attr(prediction3, "prob") # probability of success

## Next, the additional argument "add = TRUE" adds the test ROC to the previous plot
roc_rose <- plot(roc(dfvalidation$Personal.Loan, predicted.probability), print.auc = TRUE, 
                 col = "darkred", print.auc.y = .4, add = TRUE)

