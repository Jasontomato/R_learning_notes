setwd("G:\\Œ“µƒ‘∆∂À”≤≈Ã\\Graduate course\\758T Data Mining\\Assignment 2")
df <- read.csv("VoterPref.csv")
str(df)
df$PRE_Dummy <- factor(df$PREFERENCE,levels = c("For","Against"))
df$PRE_Dummy
set.seed(71923)
# Partition dataset
train <- sample(nrow(df),0.7*nrow(df))
dftrain <- df[train,]
dftest <- df[-train,]

# logistic regression
model_two <- glm(PRE_Dummy~AGE+INCOME+factor(GENDER),data =dftrain, family = 'binomial')
summary(model_two)
dftrain$PREDICT2<-  predict(model_two, newdata=dftrain, type = "response")  
dftest$PREDICT2<- predict(model_two, newdata=dftest, type = "response")
# set cut off as 0.5
cutoff <-0.5
dftrain$PREDICT3<- ifelse(dftrain$PREDICT2 > cutoff,'Against','For')  
dftrain$PREDICT3 <- factor(dftrain$PREDICT3,levels = c('For','Against'))
dftest$PREDICT3<- ifelse(dftest$PREDICT2 > cutoff,'Against','For')
dftest$PREDICT3 <- factor(dftest$PREDICT3,levels = c('For','Against'))

#compute sensitivity, specificity, PPV, NPV
confusion_train <- table(dftrain$PRE_Dummy,dftrain$PREDICT3)
confusion_test <- table(dftest$PRE_Dummy,dftest$PREDICT3)
confusion_train
Sensitivity_train <- confusion_train[1,1]/(confusion_train[1,1]+confusion_train[1,2])
Sensitivity_test <- confusion_test[1,1]/(confusion_test[1,1]+confusion_test[1,2])
Specificity_train <- confusion_train[2,2]/(confusion_train[2,2]+confusion_train[2,1])
Specificity_test <- confusion_test[2,2]/(confusion_test[2,2]+confusion_test[2,1])
PPV_train <- confusion_train[1,1]/(confusion_train[1,1]+confusion_train[2,1])
PPV_test <- confusion_test[1,1]/(confusion_test[1,1]+confusion_test[2,1])
NPV_train <- confusion_train[2,2]/(confusion_train[2,2]+confusion_train[1,2])
NPV_test <- confusion_test[2,2]/(confusion_test[2,2]+confusion_test[1,2])

# ROC
Actual <- dftrain$PREFERENCE
predicted.probability.train <- dftrain$PREDICT2
ActualTest <- dftest$PRE_Dummy
predicted.probability.test <- dftest$PREDICT2
library(pROC)
par(pty="s")
roc_rose <- plot(roc(Actual, predicted.probability.train), print.auc = TRUE, col = "blue",legacy.axes=TRUE)
## Next, the additional argument "add = TRUE" adds the test ROC to the previous plot
roc_rose <- plot(roc(ActualTest, predicted.probability.test), print.auc = TRUE, 
                 col = "darkred", print.auc.y = .4, add = TRUE)

# Accuracy along the change of  cut off 
cutoff <- seq(0, 1, length = 100)
ACC <- numeric(100)  # Creates a vector of 100 0's
## We'll collect it in a data frame.  
dftrain_Acc <- data.frame(Cutoff = cutoff, Accuracy = ACC)
for (i in 1:100) {
  dftrain_Acc$Accuracy[i] <- (sum(predicted.probability.train > cutoff[i] & Actual == "Against")+
                        sum(predicted.probability.train < cutoff[i] & Actual == "For"))/
                        length(Actual)
  }
plot(Accuracy ~ cutoff, data = dftrain_Acc, type = "o",xlab="cutoff",ylab="Accuracy",col="blue",lty=2)

cutoff <-0.41
dftest$predictMAx<- ifelse(dftest$PREDICT2 >cutoff,'Against','For') 
confusion_testmax <- table(dftest$PRE_Dummy,dftest$predictMAx)
Accuracy_max <- (confusion_testmax[2,1]+confusion_testmax[1,2])/sum(confusion_testmax)

#part III
cutoff <- seq(0, 1, length = 100)
miscost <- numeric(100)
dftrain_miscost <- data.frame(Cutoff = cutoff, miscost = miscost)
for (i in 1:100) {
  dftrain_miscost$miscost[i] <- 4*sum(predicted.probability.train > cutoff[i] & Actual == "For")+
                                sum(predicted.probability.train < cutoff[i] & Actual == "Against")
}
plot(miscost ~ cutoff, data = dftrain_miscost, type = "o",xlab="cutoff",ylab=" Misclassification Cost",col="green",lty=2)

cutoff <-0.82
dftest$predictMAx<- ifelse(dftest$PREDICT2 >cutoff,'Against','For') 
confusion_testmax <- table(dftest$PRE_Dummy,dftest$predictMAx)
Class_min <- 4*confusion_testmax[1,1]+confusion_testmax[2,2]

# part IV
dftrain_lift <- dftrain[,c('PRE_Dummy','PREDICT2')]
dftrain_lift$PRE_Dummy <- as.numeric(dftrain_lift$PRE_Dummy)-1
names(dftrain_lift)<-c('actural','probability')
pro <-dftrain_lift$probability
dftrain_lift_s <- dftrain_lift[order(-pro),]
dftrain_lift_s$Gains <- cumsum(dftrain_lift_s$actural)
plot(dftrain_lift_s$Gains,type="n",main="Lift Chart in Train dataset",xlab="Number of Cases",ylab="Cumulative Success")
lines(dftrain_lift_s$Gains)
abline(0,sum(dftrain_lift_s$actural)/nrow(dftrain_lift_s),lty = 2, col="red")

dftest_lift <- dftest[,c('PRE_Dummy','PREDICT2')]
dftest_lift$PRE_Dummy <- as.numeric(dftest_lift$PRE_Dummy)-1
names(dftest_lift)<-c('actural','probability')
pro2 <-dftest_lift$probability
dftest_lift_s <- dftest_lift[order(-pro2),]
dftest_lift_s$Gains <- cumsum(dftest_lift_s$actural)
plot(dftest_lift_s$Gains,type="n",main="Lift Chart in Test dataset",xlab="Number of Cases",ylab="Cumulative Success")
lines(dftest_lift_s$Gains)
abline(0,sum(dftest_lift_s$actural)/nrow(dftest_lift_s),lty = 2, col="red")

# define function
decile_Lift <- function(df) {
  #Sort the dataframe
  df <- df[order(-df$predicted.probability),]
  
  #Add rownumbers
  df$roworder <- 1:nrow(df)
  
  #Create a variable that holds the baseline successes for each decile
  baseline <- sum(df$actual) / 10
  
  #Assign decile
  df$decile <- ceiling((df$roworder / nrow(df)) * 10)
  
  #Count successes in each decile
  
  library("data.table")
  dt <- data.table(df)
  dt <- dt[, sum(actual), by = decile]
  dt$baseline <- baseline
  
  #Plot bargraph
  barplot(t(data.frame(dt$V1,dt$baseline)), main="Decile wise comparision of successes in Train ", xlab="Deciles", col=c("darkblue","red"), beside=TRUE, names=dt$decile)
  barplot(t(data.frame(dt$V1)/data.frame(dt$baseline)), main="Decile wise comparision of successes", xlab="Deciles", col=c("darkblue"), beside=TRUE, names=dt$decile)
}
predicted.probability<-dftrain$PREDICT2
actual <- as.numeric(dftrain$PRE_Dummy)-1
dftrain_l2 <- data.frame(predicted.probability,actual)
decile_Lift(dftrain_l2)

predicted.probability<-dftest$PREDICT2
actual <- as.numeric(dftest$PRE_Dummy)-1
dftest_l2 <- data.frame(predicted.probability,actual)
decile_Lift(dftest_l2)
