library(arules)
library(arulesViz)

setwd('D://R Data')
df = read.transactions(file = "BreadBasket_DMS.csv", format = "single", header = TRUE, sep = ",", cols = c("Transaction","Item"))
# Create an item frequency plot - part of arules package
itemFrequencyPlot(df,topN=25,type="relative")
# Get the rules
rules <- apriori(df, parameter = list(supp = 0.001, conf = 0.8))

# Show the top 5 rules
inspect(rules[1:10])

summary(rules)

rules<-sort(rules, by="confidence", decreasing=TRUE)
inspect(rules[1:20])
#
# For more concise rules - maxlen=3
rules <- apriori(df, parameter = list(supp = 0.001, conf = 0.8,maxlen=3))
inspect(rules[1:5])

plot(rules)
plot(rules, measure=c("support", "lift"), shading="confidence")
