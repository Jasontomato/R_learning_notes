# -------------------------------------------------
# BUDT758D: Data Visualization & Web Analytics
# Week 7 Section 13: Networks II (igraph)
# -------------------------------------------------
# To wrap long lines in this file, try Code -> Soft Wrap Long Lines

# Load libraries and functions
library(tidyverse)
library(readxl)
library(scales)

# install.packages("igraph")
# detach("package:network", TRUE)
library(igraph)
library(GGally)

# ------------
# Krackhardt
# ------------
# Krackhardt studied high tech workers to understand the office dynamic and captured the organizational chart and the "advice network" among the workers.  
# The file named Krackhardt_advice_edgelist.csv contains the edgelist of workers, where a link between worker W1 and worker W2 represents that W1 asks W2 for advice. 
# The file named Krackhardt_node_attributes.csv contains information about the workers, such as their age, tenure at the firm, level, and  department.

advice <- read.csv("Krackhardt_advice_edgelist.csv")
friendship <- read.csv("Krackhardt_friendship_edgelist.csv")
reports <- read.csv("Krackhardt_reports_edgelist.csv")
krack.attr <- read.csv("Krackhardt_node_attributes.csv")

# Create a graph from a data frame and include the vertex attributes
adgraph <- graph.data.frame(advice, vertices=krack.attr, directed=TRUE)
repgraph <- graph.data.frame(reports, vertices=krack.attr, directed=TRUE)
friendgraph <- graph.data.frame(friendship, vertices=krack.attr, directed=TRUE)

# Plot the networks
plot(adgraph) # default in igraph
plot(repgraph) 
plot(friendgraph) 

# Get attributes
V(repgraph)$name # names of the vertices
E(repgraph) # edges of the vertices
V(repgraph)$Dept # Department of the worker
V(repgraph)$Age

# --------------------------
# Section 1. Report network
# --------------------------

# Change default in the plot
set.seed(202)
plot(repgraph,
     vertex.size = 25,
     edge.arrow.size = 0.5,
     vertex.color =  as.factor(V(repgraph)$Dept),
     palette = 'Set2', # Add a color palette
     vertex.frame.color = 'black', #the color of the border of the dots 
     vertex.label.color = 'white', #the color of the name labels
     vertex.label.font = 2, #the font of the name labels
     vertex.label = V(repgraph)$name, #specifies the lables of the vertices. in this case the 'name' attribute is used
     vertex.label.cex=0.8) #specifies the size of the font of the labels. can also be made to vary
title(main = 'This office has four different departments that feed into the \nhead of the office',
      adj = 0)

# Try-it-yourself: discussion

# Describe the organizational hierarchy of this office.

# Who is the center of the hierarchy? What measure could you use?
which.max(closeness(repgraph,mode = 'all'))
which.max(betweenness(repgraph))
# How deep is the hierarchy? Which measure could you use?
diameter(repgraph)
# Who has the most direct reports? Which measure could you use?
which.max(degree(repgraph,mode = 'in'))
# --------------------------
# Section 2. Advice network
# --------------------------
# Centrality measures
V(adgraph)$indegree <- degree(adgraph, mode = 'in') # lists the degree centrality for each node

# Different layout 
# See the documentation at https://igraph.org/r/#docs for more detail.
# default layout; use force-directed layout algorithm
plot(adgraph,
     layout=layout.fruchterman.reingold)

plot(adgraph,
     layout=layout.grid)

pl3 <- plot(adgraph,  			
            layout= layout.circle)

pl4 <- plot(adgraph,  			
            layout= layout.davidson.harel)	

pl5 <- plot(adgraph,  			
            layout= layout_with_kk)

# Change default in the plot
set.seed(202)
plot(adgraph,
     layout=layout.fruchterman.reingold,
     main='Worker W2 has the highest in-degree in the advice network',	#specifies the title
     edge.arrow.size=0.5,
     vertex.size=V(adgraph)$indegree*2.3,
     vertex.color = "steelblue",
     vertex.label.dist=0.1,			#puts the name labels slightly off the dots
     vertex.frame.color='black', 		#the color of the border of the dots 
     vertex.label.color='white',		#the color of the name labels
     vertex.label.font=2,			#the font of the name labels
     vertex.label=V(adgraph)$name,		#specifies the lables of the vertices. in this case the 'name' attribute is used
     vertex.label.cex=0.5			#specifies the size of the font of the labels. can also be made to vary
)

# Use the grid layout
plot(adgraph,
     layout=layout.grid,
     main='Worker W2 has the highest in-degree in the advice network',	#specifies the title
     edge.arrow.size=0.5,
     vertex.size=V(adgraph)$indegree*2.3,
     vertex.color = "steelblue",
     vertex.label.dist=0.1,			#puts the name labels slightly off the dots
     vertex.frame.color='black', 		#the color of the border of the dots 
     vertex.label.color='white',		#the color of the name labels
     vertex.label.font=2,			#the font of the name labels
     vertex.label=V(adgraph)$name,		#specifies the lables of the vertices. in this case the 'name' attribute is used
     vertex.label.cex=0.5			#specifies the size of the font of the labels. can also be made to vary
)

# ----------------------------
# Section 3 Friendship network
# ----------------------------

# Use different color to denote the level of the worker
library(RColorBrewer)
pal <- brewer.pal(3,"Set2")
vertex.col <- pal[as.factor(V(repgraph)$Level)]

set.seed(202)
plot(friendgraph,
     vertex.size=25,
     edge.arrow.size=0.5,
     vertex.color =  vertex.col,
     palette = 'Set2', # Add a color palette
     vertex.frame.color='black', 		#the color of the border of the dots 
     vertex.label.color='white',		#the color of the name labels
     vertex.label.font=2,			#the font of the name labels
     vertex.label=V(repgraph)$name,		#specifies the lables of the vertices. in this case the 'name' attribute is used
     vertex.label.cex=0.8)
legend('topleft',legend=c("level 3", "level 2", "level 1"),col=c('black'),pch=21, pt.bg=unique(vertex.col))
title(main = "Workers from different levels can become friends. \nFriendship between workers is often one-sided.", adj = 0)

which.max(degree(friendgraph,mode = 'in'))
which.max(closeness(friendgraph, mode='all'))
which.max(betweenness(friendgraph))
