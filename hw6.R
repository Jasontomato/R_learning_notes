setwd("D://R data")
df <- read_excel('credit3.xlsx',sheet = 'Data')
# df<-read.csv('credit3.csv')
df1<-df
df1[,1]<-NULL
df1[,21]<-NULL
df1$Profitable<-ifelse(df1$NPV>0,1,0)
library(fastDummies)
df1<-fastDummies::dummy_cols(df1,select_columns = c('CHK_ACCT','SAV_ACCT','HISTORY','JOB','TYPE'))
df1$AMOUNT_REQUESTED<-as.numeric(gsub("[,]", "", df$AMOUNT_REQUESTED))
str(df1)

df1$CHK_ACCT<-NULL
df1$SAV_ACCT<-NULL
df1$HISTORY<-NULL
df1$JOB<-NULL
df1$TYPE<-NULL
df1$NPV<-NULL

#2
library(factoextra)
set.seed(12345)
df1sc<-scale(df1[,-16])

#2.	Perform a K-Means clustering of the data with K = 5 using all of the data except NPV and the created PROFITABLE dummy variable. 
#Include the table of cluster centers as Exhibit 1. Use 20 random starts to ensure that you have good clusters. (Don’t forget to set the seed to 12345 first.)
set.seed(12345)
km_5<-kmeans(df1sc,5,nstart=20)
km_5$centers# cluster center

#3. Comment on the profiles developed? 
#Can they be clearly identified using meaningful labels? 
#Try and describe each cluster in words. 	
df1$Cluster<-km_5$cluster
table(df1$Profitable,df1$Cluster)# whether each clusters are fitful to labels


#according to this table, it can find that there is not clear characteristic between clusters and dependent variable.
#It is hard to say that they can be clearly identified using meaningful labels.
#we just can show the center of each cluster. And the parameters are in the pictures of problem 2. 


#4.	The output allows you to identify, for each individual, the cluster they belong to.
#Combine this with the NPV column from the original data (making sure that the Row Id matches). 
df1$NPV<-df$NPV

#a.	Create a bar chart showing the percentage of people in each cluster.
bar_chart_x<-c('1','2','3','4','5')
bar_chart_y<-c(sum(ifelse(df1$Cluster==1,1,0))/nrow(df1),sum(ifelse(df1$Cluster==2,1,0))/nrow(df1),sum(ifelse(df1$Cluster==3,1,0))/nrow(df1),sum(ifelse(df1$Cluster==4,1,0))/nrow(df1),sum(ifelse(df1$Cluster==5,1,0))/nrow(df1))
bar_chart<-data.frame(bar_chart_x,bar_chart_y)
library(ggplot2)
library(tidyverse)
bar_chart%>%   
  ggplot(aes(x=bar_chart_x,y=bar_chart_y))+geom_bar(stat='identity')+
  theme_classic()+
  labs(title='the precentage of each clusters',x='cluster',y='Percentage' )
#b.	Create a table showing average of NPV split up by Cluster Id. 
df1$infer_cluster1<-ifelse(df1$Cluster==1,1,0)
df1$infer_cluster2<-ifelse(df1$Cluster==2,1,0)
df1$infer_cluster3<-ifelse(df1$Cluster==3,1,0)
df1$infer_cluster4<-ifelse(df1$Cluster==4,1,0)
df1$infer_cluster5<-ifelse(df1$Cluster==5,1,0)
cluster1_NPV<-sum(df1$NPV*df1$infer_cluster1)/sum(df1$infer_cluster1)
cluster2_NPV<-sum(df1$NPV*df1$infer_cluster2)/sum(df1$infer_cluster2)
cluster3_NPV<-sum(df1$NPV*df1$infer_cluster3)/sum(df1$infer_cluster3)
cluster4_NPV<-sum(df1$NPV*df1$infer_cluster4)/sum(df1$infer_cluster4)
cluster5_NPV<-sum(df1$NPV*df1$infer_cluster5)/sum(df1$infer_cluster5)

cluster_id<-c('1','2','3','4','5')
average_npv<-c(cluster1_NPV,cluster2_NPV,cluster3_NPV,cluster4_NPV,cluster5_NPV)
table_NPV<-data.frame(cluster_id,average_npv)
table_NPV
#c.	Attach these as Exhibit 2.

#5.	Comment on your results above. Specifically, which cluster is most profitable?

#6.	Use the elbow method to determine what is the optimal K. 
#State what K you decide upon and attach the relevant graph as Exhibit 3. 

set.seed(12345)
wss <- (nrow(df1sc)-1)*sum(apply(df1sc,2,var))
for (i in 2:20) wss[i] <- sum(kmeans(df1sc,
                                     centers=i)$withinss)
plot(1:20, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")


set.seed(12345)
fviz_nbclust(df1sc, kmeans, method = "wss",k.max=20)



#7.	Conduct an association rule analysis of the data to identify attributes that are related to profitability (include the PROFITABLE dummy variable and focus on rules for which this variable is the consequent). 
#Attach any supporting output as Exhibit 4.
df2<-df[,c(3,4,7,9,10,16)]
df2$Profitable<-ifelse(df$NPV>0,1,0)
df2<-fastDummies::dummy_cols(df2,select_columns = c('CHK_ACCT','SAV_ACCT','HISTORY','JOB','EMPLOYMENT'))
df2$CHK_ACCT<-factor(df2$CHK_ACCT)
df2$CHK_ACCT<-NULL
df2$SAV_ACCT<-NULL
df2$HISTORY<-NULL
df2$EMPLOYMENT<-NULL
df2$JOB<-NULL

str(df2)
df2<-na.omit(df2)
library(arules)
library(arulesViz)

df2_matrix<- as.matrix(df2)
df2_matrix <- apply(df2_matrix,1:2, function(x) ifelse(x==1,TRUE,FALSE))
#
df2_transaction <- as(df2_matrix,"transactions")
# To look at the data
itemFrequencyPlot(df2_transaction, col="Pink")
set.seed(12345)
rules<-apriori(data=df2_transaction, parameter=list(supp=0.2,conf = 0.7), 
               appearance = list(default="lhs",rhs="Profitable"),
               control = list(verbose=F))

rules<-sort(rules, decreasing=TRUE,by="lift")
inspect(rules[1:8])
plot(rules, method="grouped")


inspect(rules[1:5])

#
df2$NPV<-df$NPV

rule1_NPV<-sum(df2$CHK_ACCT_3*df2$JOB_2*df2$NPV)
# rule2_NPV<-sum(df2$SAV_ACCT_2*df2$JOB_0*df2$NPV)
# rule3_NPV<-sum(df2$EMPLOYMENT_2*df2$JOB_0*df2$NPV)
# rule4_NPV<-sum(df2$CHK_ACCT_3*df2$JOB_0*df2$NPV)
# rule5_NPV<-sum(df2$SAV_ACCT_2*df2$HISTORY_0*df2$NPV)
# best_rules2_NPV<- sum(rule1_NPV,rule2_NPV,rule3_NPV,rule4_NPV,rule5_NPV)
# best_rules2_NPV
rule1_NPV

#
df3<-as.data.frame(matrix(nrow=1000))
df3$Cluster<-df1$Cluster
df3$Profitable<-df1$Profitable
df3$V1<-NULL
df3<-fastDummies::dummy_cols(df3,select_columns = 'Cluster',remove_selected_columns = TRUE)

df3_matrix<- as.matrix(df3)
df3_matrix <- apply(df3_matrix,1:2, function(x) ifelse(x==1,TRUE,FALSE))
#
df3_transaction <- as(df3_matrix,"transactions")

rules<-apriori(data=df3_transaction, parameter=list(supp=0.01,conf = 0.6), 
               appearance = list(default="lhs",rhs="Profitable"),
               control = list(verbose=F))
plot(rules, method="grouped")

rules.sort<-sort(rules, decreasing=TRUE,by="lift")
inspect(rules.sort[1:5])

df3$NPV <- df$NPV
rule_cluster1_NPV = sum(df3$NPV * df3$Cluster_1)
rule_cluster1_NPV
rule_cluster4_NPV = sum(df3$NPV * df3$Cluster_4)
rule_cluster4_NPV
rule_cluster2_NPV = sum(df3$NPV * df3$Cluster_2)
rule_cluster2_NPV
rule_cluster3_NPV = sum(df3$NPV * df3$Cluster_3)
rule_cluster3_NPV

