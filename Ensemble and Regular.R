setwd('D:\\R data')
library(readxl)
library(caret)
library(class)
library(pROC)
library(e1071)
library(tree)
df <- read_excel('credit3.xlsx',sheet = 'Data')
df [,c(3:4,7,10,20)]= lapply(df[,c(3,4,7,10,20)],as.factor)
df_require3 = df

drops3 = c('OBS#','CREDIT_EXTENDED')
df_require3 = df_require3[ , !(names(df_require3) %in% drops3)]
set.seed(12345)
train3 <- sample(nrow(df_require3),0.7*nrow(df_require3))
dftrain3 <- df_require3[train3,]
dftest3 <- df_require3[-train3,]
# linear model
linear.model <- lm(NPV~.,dftrain3)
summary(linear.model)
linear.predict = predict(linear.model, dftest3)

sqrt(mean((dftest3$NPV - linear.predict)^2))

#random forest
library(randomForest)

#
# We first do bagging (which is just RF with m = p)
set.seed(1)
rf.model=randomForest(NPV~.,data=dftrain3,mtry=5,importance=TRUE)
rf.model
rf.predict = predict(rf.model,newdata=dftest3)
sqrt(mean((dftest3$NPV - rf.predict)^2))
# boosting
library(gbm)
set.seed(1)
boost.model=gbm(NPV~.,data=dftrain3,distribution="gaussian",n.trees=5000,interaction.depth=4)
summary(boost.model)
gbm.predict = predict(boost.model,newdata=dftest3,n.trees=5000)
sqrt(mean((dftest3$NPV - gbm.predict)^2))


# regularization






