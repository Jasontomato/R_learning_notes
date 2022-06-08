## --------------------------------------------------------------------------------------------
## The Beer Preference data set is used to illustrate how to plot ROC curves
## 
## --------------------------------------------------------------------------------------------
##
# Read the data
#
setwd("G:\\Œ“µƒ‘∆∂À”≤≈Ã\\Graduate course\\758T Data Mining\\Course Practise")
Beer.Preferences <- read.csv("Beer Preferences.csv")
#
# In this case dollar amounts will be read as character strings. 
# You can change that in Excel - or in R as follows
#
#
Beer.Preferences$Income <- as.numeric(sub('$','',as.character(Beer.Preferences$Income),fixed=TRUE))
#
# Suppose we want Light to be the success class
Beer.Preferences$Preference <- factor(Beer.Preferences$Preference,levels=c("Regular","Light"))
#
#
set.seed(12345)
inTrain <- sample(nrow(Beer.Preferences), 0.6*nrow(Beer.Preferences))
#
train <- data.frame(Beer.Preferences[inTrain,])
test <- data.frame(Beer.Preferences[-inTrain,])
## +--------------------------------------------------------------------+
## The first step is to generate predicted class probabilities
## +--------------------------------------------------------------------+
## Training data predicted probabilities
##
fit <- glm(Preference ~ Income+Age+Married+Gender, data = train, family = "binomial")
#
predicted.probability.train <- predict(fit, type = "response") 
Actual <- train$Preference
##
## Test data predicted probabilities
##
predicted.probability.test <- predict(fit, type = "response", newdata = test)
ActualTest <- test$Preference
##
## +-------------------------------------------------------------------------+
## There are multiple ways to plot ROC curves. Let's start with the simplest.
## +-------------------------------------------------------------------------+
## Using the pROC Library (install the pROC package if you don't already have it)
## 
library(pROC)
par(pty="s")
roc_rose <- plot(roc(Actual, predicted.probability.train), print.auc = TRUE, col = "blue",legacy.axes=TRUE)
## Next, the additional argument "add = TRUE" adds the test ROC to the previous plot
roc_rose <- plot(roc(ActualTest, predicted.probability.test), print.auc = TRUE, 
                 col = "green", print.auc.y = .4, add = TRUE)
##
##
## +------------------------------------------------------------------------+
## The remainder of the script shows additional methods for generating ROC curves
#
## +-----------------------------------------------------------------------------------+
## Using the ROCR Library (the main advantage is superior aesthetics)
## +-----------------------------------------------------------------------------------+
library(ROCR)
##
## We had reversed the default levels of Preference - this requires a correction
pd1 <- prediction(predicted.probability.train, Actual,label.ordering = c("Regular", "Light"))
pf1 <- performance(pd1, "tpr","fpr")

pd2 <- prediction(predicted.probability.test, ActualTest,label.ordering = c("Regular", "Light"))
pf2 <- performance(pd2, "tpr","fpr")

plot(pf1, colorize = TRUE)
plot(pf2, add = TRUE, colorize = TRUE)
##
##
pf <- performance(pd1, "auc")
pf    # y.values is the AUC
pf@y.values
##
## +------------------------------------------------------------------------------------+
## I show you code for generating ROC's from scratch.The main reason for doing 
## this would be to deepen your understanding of ROC curves (and R)
## Finally, we do this using ggplot, for better aesthetics.
##
##
## +--------------------------------------------------------------------------------------+
## + The following plots the ROC curve (for the training data)                            +
## +--------------------------------------------------------------------------------------+
##
# 
## Make data for a ROC curve
cutoff <- seq(0, 1, length = 100)
fpr <- numeric(100)  # Creates a vector of 100 0's
tpr <- numeric(100)
Actual <- train$Preference
## We'll collect it in a data frame.  
roc.table <- data.frame(Cutoff = cutoff, FPR = fpr,TPR = tpr)
## At this point we just have a column for cutoffs and two columns of 0's

## TPR is the Sensitivity; FPR is 1-Specificity. We enter these numbers in each row.
for (i in 1:100) {
  roc.table$FPR[i] <- sum(predicted.probability.train > cutoff[i] & Actual == "Regular")/sum(Actual == "Regular")
  roc.table$TPR[i] <- sum(predicted.probability.train > cutoff[i] & Actual == "Light")/sum(Actual == "Light")
}
roc.table.train <- roc.table
## The first line plots the Sensitivity against 1-Specificity
plot(TPR ~ FPR, data = roc.table.train, type = "o",xlab="1 - Specificity",ylab="Sensitivity",col="blue",lty=2)
## Next line adds the central daigonal
abline(a = 0, b = 1, lty = 2,col="red")
##
##
## Note that the values of the cutoffs don't show up on the plot
## You can print the table used for the graph to see the corresponding cutoff values
# roc.table
## +--------------------------------------------------------------------------------------+
## + The following plots both training and test ROCs on the same graph                    +
## +--------------------------------------------------------------------------------------+
##
## Make data for a ROC curve
##
cutoff <- seq(0, 1, length = 100)
fpr <- numeric(100)
tpr <- numeric(100)
ActualTest <- test$Preference
## We'll collect it in a data frame.  (We could also just keep it in three vectors)
roc.table <- data.frame(Cutoff = cutoff, FPR = fpr,TPR = tpr)

## TPR is the Sensitivity; FPR is 1-Specificity
for (i in 1:100) {
  roc.table$FPR[i] <- sum(predicted.probability.test > cutoff[i] & ActualTest == "Regular")/sum(ActualTest == "Regular")
  roc.table$TPR[i] <- sum(predicted.probability.test > cutoff[i] & ActualTest == "Light")/sum(ActualTest == "Light")
}
roc.table.test <- roc.table
lines(TPR ~ FPR, data = roc.table.test, type = "o",xlab="1 - Specificity",ylab="Sensitivity",col="magenta",lty=2)
# +-------------------------------------------------------------------------------------+
library(ggplot2)
##
roc.table.train <- roc.table.train[order(-roc.table.train$Cutoff),]
roc.table.test <- roc.table.test[order(-roc.table.test$Cutoff),]
p = ggplot() + geom_rect(aes(xmin=0,xmax=1,ymin=0,ymax=1), fill="transparent",col="darkgray") +
  geom_line(data = roc.table.train, aes(x = FPR, y = TPR, color=Cutoff),size=1)  + 
  geom_point(data = roc.table.train, aes(x = FPR, y = TPR, color=Cutoff),shape=19,size=2) +
  geom_line(data = roc.table.test, aes(x = FPR, y = TPR, color=Cutoff),size=1) +
  geom_point(data = roc.table.test, aes(x = FPR, y = TPR, color=Cutoff),shape=15,size=2) +
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), color="darkgray") + 
  xlab('False Positive Rate') + ggtitle('ROC Curve') +
  ylab('True Positive Rate') + coord_fixed() +
  theme_minimal() 
print(p)
##
## In general "large" differences between the training and test ROC is evidence of overfitting
##
## +--------------------------------------------------------------------------------------+
## + The following plots the ROC curve for two different models to compare them                           +
## +--------------------------------------------------------------------------------------+
##
# 
## The first line plots the Sensitivity against 1-Specificity
plot(TPR ~ FPR, data = roc.table.train, type = "o",xlab="1 - Specificity",ylab="Sensitivity",col="blue")
## Next line adds the central diagonal
abline(a = 0, b = 1, lty = 2,col="red")
##
##
fit2 <- glm(Preference ~ Income+Age, data = train, family = "binomial")
#
predicted.probability.train2 <- predict(fit2, type = "response") 
## Make data for a ROC curve
cutoff <- seq(1, 0, length = 100)
fpr <- numeric(100)  # Creates a vector of 100 0's
tpr <- numeric(100)
Actual <- train$Preference
## We'll collect it in a data frame.  
roc.table <- data.frame(Cutoff = cutoff, FPR = fpr,TPR = tpr)
## At this point we just have a column for cutoffs and two columns of 0's

## TPR is the Sensitivity; FPR is 1-Specificity. We enter these numbers in each row.
for (i in 1:100) {
  roc.table$FPR[i] <- sum(predicted.probability.train2 > cutoff[i] & Actual == "Regular")/sum(Actual == "Regular")
  roc.table$TPR[i] <- sum(predicted.probability.train2 > cutoff[i] & Actual == "Light")/sum(Actual == "Light")
}
lines(TPR ~ FPR, data = roc.table, type = "o",xlab="1 - Specificity",ylab="Sensitivity",col="magenta",lty=2)
##
## Conclude that the addition of Married and Gender does not add to the model's 
## performance

