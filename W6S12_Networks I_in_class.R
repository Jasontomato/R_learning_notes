# -------------------------------------------------
# BUDT758D: Data Visualization & Web Analytics
# Week 6 Section 12: Networks I
# -------------------------------------------------
# To wrap long lines in this file, try Code -> Soft Wrap Long Lines
setwd('G:\\Œ“µƒ‘∆∂À”≤≈Ã\\Graduate course\\R dataset')
# Load libraries and functions
library(tidyverse)
library(readxl)
library(scales)

#install.packages("GGally")
#install.packages("sna")
#install.packages("network")
library(GGally)
library(sna)
library(network)

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

# create the advice network with ggnet
# ggnet use force-directed algorithm to position nodes.

adnet <- advice %>%
  network(directed = TRUE)
set.seed(201)
adnet %>%
  ggnet2(label=TRUE,
         size = 14,
         arrow.size = 4,
         arrow.gap = 0.05)

repnet <- reports %>%
  network(directed = TRUE)
set.seed(201)
repnet %>%
  ggnet2(label=TRUE,
         size = 14,
         arrow.size = 4,
         arrow.gap = 0.05)

# Change the size by network measures such as degree
set.seed(201)
adnet%>%
  ggnet2(label=TRUE,
         size="indegree", # size the degree
         max_size = 20, # maximum size
         label.color="black",
         label.size = 3,
         arrow.size = 4,
         arrow.gap = 0.05,
         legend.position = "none") 

# Change the color by department
# Make sure that the names are in the correct order
network.vertex.names(repnet)
krack.attr$Node # They are not the same!

# Match(A,B) find position of A in B
# The example in slids:
M = data.frame(Worker = c("A1","A4","A3","A2"),
               Age = c(25,33,35,46))
Order = c("A2","A3","A4","A1")
# Find potision of "Order" in column "Worker"
correct_order <- match(Order, M$Worker)
M[correct_order , ]

# Find position of network.vertex.names(repnet) in krack.attr
pos <- match(network.vertex.names(repnet), krack.attr$Node)
krack.attr <- krack.attr[pos,]

repnet%v%"Dept" <- krack.attr$Dept # Add the Department attribute

rplot1 <- repnet %>%
  ggnet2(label=T,
         size=14,
         label.color="black",
         node.color= "Dept",
         palette = "Set1",
         label.size = 3,
         arrow.size = 4,
         arrow.gap = 0.05,
         legend.position = "none") 
rplot1

rplot1 +
  labs(title="Krackhardt Reporting Network",
       subtitle = "This office has four different departments that feed into the head of the office.",
       caption = "Source: Krackhardt office study") +
  geom_text(aes(x=0.48, y=0.62, label="W7 is\nthe head"),
            fontface="italic", colour="grey25") +
  theme(panel.border = element_rect(fill=NA,color="black"),
        plot.caption = element_text(face = "italic"))

# Plot the advice network. Color the nodes by their degrees.

# calculate the degree
de <- degree(adnet)
dei <- degree(adnet, cmode="indegree")

# Label the person with the highest in-degree 
de_label <- network.vertex.names(adnet)[which.max(dei)]

# Create a palette of shades of grey showing the degree
# Side note: grey values range from 1 (darkest) to 99 (lightest)
# The lowest degree (lightest shade) will have a grey value of 85
# The highest degree (darkest shade) will have a grey value of 25
dei_colors <-  scales::cscale(dei, 
                              seq_gradient_pal("grey85", "grey25"))
dei_label_colors <- ifelse(dei==max(dei), "white", "black")

set.seed(201)
adplot2 <- adnet %>%
  ggnet2(label=TRUE,
         size=10,
         node.color= dei_colors,
         label.color=dei_label_colors,
         label.size = 3,
         edge.color = "grey80",
         arrow.size = 2,
         arrow.gap = 0.03,
         legend.position = "none") 
adplot2

adplot2 +
  labs(title="Krackhardt Advice Network",
       subtitle = "Worker W2 has the highest in-degree in the advice network for the office.",
       caption = "Source: Krackhardt office study") +
  theme(panel.border = element_rect(fill=NA,color="black"),
        plot.caption = element_text(face = "italic")) 

# Make a discreate and a sequential palette
set.seed(201)
adplot3 <- adnet%>%
  ggnet2(label=TRUE,
         size=10,
         node.color= dei, 
         label.color= "white",
         label.size = 3,
         edge.color = "grey80",
         arrow.size = 2,
         arrow.gap = 0.03,
         legend.position = "none") 
adplot3

# make the sequential palette
dei_brewer_colors <- scales::dscale(dei %>% cut(5), 
                                    brewer_pal(palette = 2))
dei_label_colors <- ifelse(dei==max(dei), "white", "black")

set.seed(201)
adplot4 <- adnet %>%
  ggnet2(label=TRUE,
         size=10,
         node.color= dei_brewer_colors,
         label.color= dei_label_colors,
         label.size = 3,
         edge.color = "grey80",
         arrow.size = 2,
         arrow.gap = 0.03,
         legend.position = "none") 
adplot4

adplot4 +
  labs(title="Krackhardt Advice Network",
       subtitle = "Worker W2 has the highest in-degree in the advice network for the office.",
       caption = "Source: Krackhardt office study") +
  theme(panel.border = element_rect(fill=NA,color="black"),
        plot.caption = element_text(face = "italic")) 

# --------------------
# In-class activity
# -------------------

# Use ggnet2 to plot the friendship network. Change the node colors by in-degree. Who is the most popular worker?

fri <- friendship %>%
  network(directed = TRUE)
de <- degree(fri)
dei <- degree(fri, cmode="indegree")
dei_label_colors <- ifelse(dei==max(dei), "green", "gold")
set.seed(201)
fri %>%
  ggnet2(label=TRUE,
         size = 14,
         node.color= dei_label_colors,
         arrow.size = 4,
         arrow.gap = 0.05)

