# Attribution: http://www.salemmarafi.com/code/market-basket-analysis-with-r/
# Load the libraries
library(arules)
library(arulesViz)

# Load the data set (this is a compressed sparse matrix and not easily viewable)
data(Groceries)
# Create an item frequency plot - part of arules package
itemFrequencyPlot(Groceries,topN=25,type="relative")
# Get the rules
rules <- apriori(Groceries, parameter = list(supp = 0.001, conf = 0.8))

# Show the top 5 rules
inspect(rules[1:10])
#
summary(rules)
#
rules<-sort(rules, by="confidence", decreasing=TRUE)
inspect(rules[1:20])
#
# For more concise rules - maxlen=3
rules <- apriori(Groceries, parameter = list(supp = 0.001, conf = 0.8,maxlen=3))
inspect(rules[1:5])
#
# Rules targeting (here rhs = whole milk)
rules<-apriori(data=Groceries, parameter=list(supp=0.001,conf = 0.08), 
               appearance = list(default="lhs",rhs="whole milk"),
               control = list(verbose=F))
rules<-sort(rules, decreasing=TRUE,by="confidence")
inspect(rules[1:5])
#
#
# Visualization of rules
library(arulesViz)
plot(rules)
plot(rules, measure=c("support", "lift"), shading="confidence")
#
#
subrules <- rules[quality(rules)$confidence > 0.8]
plot(subrules)
#

rules <- apriori(Groceries, parameter = list(supp = 0.001, conf = 0.8))
plot(rules, method="grouped")
plot(rules, method="grouped", control=list(k=10))
#
# Graph-based visualization
subrules2 <- head(sort(rules, by="lift"), 10)
# The following plot represents itemsets as vertices and rules as directed
# edges between itemsets
plot(subrules2, method="graph", control=list(type="items"))
#

## ------------------------------------------------------------------------------------------
# Interactively examining rules (inspect/filter/zoom in/zoom out)
## ------------------------------------------------------------------------------------------
sel <- plot(subrules, measure=c("support", "lift"), shading="confidence", interactive =TRUE )
sel <- plot(rules, method="grouped", interactive=TRUE)

