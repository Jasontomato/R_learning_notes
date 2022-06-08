setwd("G:\\Œ“µƒ‘∆∂À”≤≈Ã\\Graduate course\\758T Data Mining\\Assignment 2")
df <- read.csv("VoterPref.csv")
str(df)
df$PREFERENCE <- factor(df$PREFERENCE,levels = c('For','Against' ))
df$PRE_Dummy <- ifelse(df$PREFERENCE =='Against',1,0)
# setseed
set.seed(71923)
# Partition dataset
train <- sample(nrow(df),0.7*nrow(df))
dftrain <- df[train,]
dftest <- df[-train,]

# part II
library(dplyr)
par(mfrow=c(1,2))
boxplot(INCOME~PREFERENCE ,data = dftrain, main='Income over preference')
boxplot(AGE~PREFERENCE ,data = dftrain, main='Age over preference')

preference_count <- table(dftrain$PREFERENCE)
prop.table(preference_count)
library(radiant)
result <- pivotr(
  dftrain, # Data frame
  cvars = c("GENDER",'PREFERENCE'), 
)
summary(result)

#part III

model_one <- lm(PRE_Dummy~AGE+INCOME+factor(GENDER),data =dftrain)
summary(model_one)
#compute RMSE in train data
e_train <-model_one$residuals
RMSE_train <- sqrt(mean(e_train^2))
# compute RMSE in test data
predicted <- predict(model_one,dftest)
e_test <- (dftest$PRE_Dummy - predicted)
RMSE_test <- sqrt(mean(e_test^2,na.rm=T))
# do classification within 0.5 cut off
dftrain$PREDICT<- ifelse(predict(model_one,data=dftrain)>0.5,1,0)  
dftest$PREDICT<- ifelse(predict(model_one,dftest)>0.5,1,0)  
#propotion of predicted class
pre_test_predict <- table(dftest$PREDICT)
prop.table(pre_test_predict)
# calculate error rate 
confusion_train <- table(dftrain$PRE_Dummy,dftrain$PREDICT)
err_rate_train <- (confusion_train[1,2]+confusion_train[2,1])/
                  sum(confusion_train)
err_rate_train
confusion_test <- table(dftest$PRE_Dummy,dftest$PREDICT)
err_rate_test <- (confusion_test[1,2]+confusion_test[2,1])/
  sum(confusion_test)
err_rate_test


# logistic regression
model_two <- glm(PRE_Dummy~AGE+INCOME+factor(GENDER),data =dftrain, family = 'binomial')
summary(model_two)
dftrain$PREDICT2<-  predict(model_two, newdata=dftrain, type = "response")  
dftest$PREDICT2<- predict(model_two, newdata=dftest, type = "response")
# set cut off as 0.5
dftrain$PREDICT3<- ifelse(dftrain$PREDICT2>0.5,1,0)  
dftest$PREDICT3<- ifelse(dftest$PREDICT2>0.5,1,0)
# calculate err
confusion_train <- table(dftrain$PRE_Dummy,dftrain$PREDICT3)
err_rate_train <- (confusion_train[1,2]+confusion_train[2,1])/
  sum(confusion_train)
err_rate_train
confusion_test <- table(dftest$PRE_Dummy,dftest$PREDICT3)
err_rate_test <- (confusion_test[1,2]+confusion_test[2,1])/
  sum(confusion_test)
err_rate_test
(confusion_train[1,2]+confusion_train[2,1])
(confusion_test[1,2]+confusion_test[2,1])
confusion_test
# set cut off as 0.4
dftest$PREDICT4<- ifelse(dftest$PREDICT2>0.4,1,0) 
confusion_test2 <- table(dftest$PRE_Dummy,dftest$PREDICT4)
err_rate_test2 <- (confusion_test2[1,2]+confusion_test2[2,1])/
  sum(confusion_test2)
err_rate_test2

#predict 
new_data <- data.frame(AGE =36,INCOME =70,GENDER ='F')
predicted <- predict(model_two,new_data, type = "response")
summary(dftest$PREDICT2)
