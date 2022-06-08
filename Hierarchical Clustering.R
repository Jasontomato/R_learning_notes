#
# Illustrates hierarchical clustering with the Universities data set
#
setwd('D://R Data')
df <- read.csv("Universities.csv")
# Scale the data set (with the exception of university name)
dfsc <- scale(df[,-1])
# 
# If you assign row names, these will be used as labels at the end of 
# the tree (otherwise row number will be used)
row.names(dfsc) <- df$Univ
#
# Function call to hclust (note that argument is a distance matrix,
# since dist() computes the Euclidean distance)
# Additionally we indicate what cluster distance masure to use
#
hc.complete = hclust(dist(dfsc),method="complete")
plot(hc.complete,main="Complete Linkage",xlab="",hang = -1, sub="",cex=0.9)
rect.hclust(hc.complete, k=4)
(groups <- sort(cutree(hc.complete, k=4)))
#
hc.average = hclust(dist(dfsc),method="average")
plot(hc.average,main="Average Linkage",xlab="",hang = -1, sub="",cex=0.9)
rect.hclust(hc.average, k=4)
(groups <- sort(cutree(hc.average, k=4)))
#
hc.single = hclust(dist(dfsc),method="single")
plot(hc.single,main="Single Linkage",xlab="",hang = -1, sub="",cex=0.9)
rect.hclust(hc.single, k=4)
(groups <- sort(cutree(hc.single, k=4)))
#
hc.centroid = hclust(dist(dfsc),method="centroid")
plot(hc.centroid,main="Average Group Linkage",hang = -1, xlab="",sub="",cex=0.9)
rect.hclust(hc.centroid, k=4)
(groups <- sort(cutree(hc.centroid, k=4)))
#


