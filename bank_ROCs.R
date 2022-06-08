#
# Read the data. 
#
df <- read.csv("UniversalBank.csv")
df[,1] <- NULL
df$ZIP.Code <- NULL
#
df[,1:7] <- apply(df[,1:7], 2, scale)
df[,9:12] <- apply(df[,9:12], 2, scale)
#
#
library("caret")
set.seed(12457)
inTrain <- createDataPartition(df$Personal.Loan, p=0.6, list=FALSE)
#
dftrain <- data.frame(df[inTrain,]) # with 60% of the data
dftemp <- data.frame(df[-inTrain,]) # with 40% of the data
# 
# Now split in two equal parts
inVal <- createDataPartition(dftemp$Personal.Loan, p=0.5, list=FALSE)
dfvalidation <- data.frame(dftemp[inVal,])
dftest <- data.frame(dftemp[-inVal,])
#
# knn model
library(class)
# 
# The knn function requires inputs to be matrices or vectors
# The 8th column is the dependent variable Personal.Loan
train_input <- as.matrix(dftrain[,-8])
train_output <- as.vector(dftrain[,8])
validate_input <- as.matrix(dfvalidation[,-8])
test_input <- as.matrix(dftest[,-8])
#
#
set.seed(3)
# +-------------------------------------------------------------------------------------+
#
# Now we look for the value of K which minimizes validation error rate
# We will search in the range 1:15
kmax <- 15
ER1 <- rep(0,kmax) # Zero vectors to be updated below with error rates
ER2 <- rep(0,kmax) #
#
# We fit a model for each value of K in the range 1:15
for (i in 1:kmax){
prediction <- knn(train_input, train_input,train_output, k=i)
prediction2 <- knn(train_input, validate_input,train_output, k=i)
prediction3 <- knn(train_input, test_input,train_output, k=i)
#
# The confusion matrix for training data is:
CM1 <- table(dftrain$Personal.Loan,prediction)
# The training error rate is:
ER1[i] <- (CM1[1,2]+CM1[2,1])/sum(CM1)
# The confusion matrix for validation data is: 
CM2 <- table(dfvalidation$Personal.Loan,prediction2)
ER2[i] <- (CM2[1,2]+CM2[2,1])/sum(CM2)
}
#
# Now plot this
plot(c(1,kmax),c(0,0.1),type="n", xlab="k",ylab="Error Rate")
lines(ER1,col="red")
lines(ER2,col="blue")
legend(9, 0.1, c("Training","Validation"),lty=c(1,1), col=c("red","blue"))
#
# Find K to minimize ER2 (validation error)
#
z <- which.min(ER2)
cat("Minimum Validation Error k:", z)
abline(v=z, col="red", lty=2)

prediction3 <- knn(train_input, test_input,train_output, k=i, prob=T)
predicted.probability.knn <- attr(prediction3, "prob")
Predicted_class <- knn(train_input, test_input,train_output, k=i)
predicted.probability.knn <- ifelse(Predicted_class ==1, predicted.probability.knn, 1-predicted.probability.knn)
# ROC Curves
par(pty="s")
library(pROC)
roc_rose <- plot(roc(dftest$Personal.Loan, predicted.probability.knn), 
                 print.auc = TRUE, col = "blue",legacy.axes=T)
#-----------------------------------------------------------------------------------------------------------
# Naive Bayes
#-----------------------------------------------------------------------------------------------------------
#
# Read the data. 
#
df <- read.csv("UniversalBank.csv")
df[,1] <- NULL
df$ZIP.Code <- NULL
#
# The NaiveBayes can take either a factor as the dependent variable or a numerical variable
# It fits different models depending on this choice. If you have a qualitative dependent variable
# you will need to make sure this is a factor
#
df$Personal.Loan <- as.factor(df$Personal.Loan)
df$Family <- as.factor(df$Family)
df$Education <- as.factor(df$Education)
df$Securities.Account <- as.factor(df$Securities.Account)
df$CD.Account <- as.factor(df$CD.Account)
df$Online <- as.factor(df$Online)
df$CreditCard <- as.factor(df$CreditCard)
#
#
library("caret")
set.seed(12457)
inTrain <- createDataPartition(df$Personal.Loan, p=0.6, list=FALSE)
#
dftrain <- data.frame(df[inTrain,]) # with 60% of the data
dftemp <- data.frame(df[-inTrain,]) # with 40% of the data
# 
# Now split in two equal parts
inVal <- createDataPartition(dftemp$Personal.Loan, p=0.5, list=FALSE)
dfvalidation <- data.frame(dftemp[inVal,])
dftest <- data.frame(dftemp[-inVal,])
#
# We require the library e1071
library(e1071)
# Can handle both categorical and numeric input, 
# but output must be categorical
## The function call to run Naive Bayes
model <- naiveBayes(Personal.Loan~., data=dftrain)
model # Generates output including conditional probabilities
# For categorical predictors we get P(X=Xi|Class=j)
# For numerical predictors we get mean and stdev of a continuous 
# probability P(X|Class=j)
#
#
# For class probabilities
predicted.probability <- predict(model, newdata = dftest[,-8], type="raw")
predicted.probability.NB <- predicted.probability[,2]
#
##
roc_rose <- plot(roc(dftest$Personal.Loan, predicted.probability.NB), print.auc = TRUE, 
                 col = "green", print.auc.y = .4, add = TRUE, legacy.axes=T)

