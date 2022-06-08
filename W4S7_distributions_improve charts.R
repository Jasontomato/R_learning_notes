# -------------------------------------------------
# BUDT758D: Data Visualization & Web Analytics
# Week 4 Section 7-8: Histogram and Boxplot
# -------------------------------------------------
# To wrap long lines in this file, try Code -> Soft Wrap Long Lines
setwd("G:\\Œ“µƒ‘∆∂À”≤≈Ã\\Graduate course\\R dataset")

# install.packages("tidyverse")
library(tidyverse)

# set working directory to the current folder
currentdir <- getwd()
setwd(currentdir)

# Read Data
supermarket_sales <- read.csv("supermarket_sales_data.csv")
house_price <- read.csv('kc_house_data_sample.csv')

# ---------------------
# Section 1. Histogram
# ---------------------
# a) Create a histogram of the total purchase ("Total" in the data set)
# We can define "aes" in ggplot() or later when we specify the chart. 
# If we define "aes" in ggplot, all the layers added to the figure will then use the same x and y-axis.
summary(supermarket_sales$Total)
supermarket_sales %>%
  ggplot(aes(x=Total)) + # For histogram, we only need to define x-axis
  geom_histogram(breaks=seq(0,1100,100), fill="lightblue", color="white") + # fill: filled color; color: border color
  labs(title="The distribution of total purchase is right skewed with a \n median at $253.9",
       caption = "Kaggle: Supermarket Sales",
       x="Total purchase", y="Frequency") +
  theme_classic() +
  theme(axis.ticks = element_blank(), # remove ticks
        axis.title = element_text(face="bold"), 
        plot.caption = element_text(face="italic"),
        plot.title = element_text(size=14),
        panel.grid.major.y = element_line(color="grey95")) # remove vertical major grid lines on

# b) Create a histogram of the "Rating" 
supermarket_sales %>%
  ggplot(aes(x=Rating)) +
  geom_histogram(binwidth=1, fill="lightblue", color="white") +# in this case we set binwidth instead of the number of bins since the range of ratings is from 4 to 10.
  labs(title="Customer ratings range from 4 to 10 with a median at 7",
       caption = "Kaggle: Supermarket Sales",
       x="Rating", y="Frequency") +
  theme_classic() +
  theme(axis.ticks = element_blank(), # remove ticks
        axis.title = element_text(face="bold"), 
        plot.caption = element_text(face="italic"),
        plot.title = element_text(size=14),
        #panel.grid.major.y = element_line(color="grey95")
        ) # remove vertical major grid lines on

summary(supermarket_sales$Rating)

# ---------------------------------------------
# Section 2: Box Plot of Sales By Product Line
# ---------------------------------------------
# Create a data set grouped by product line
sales_productline <- supermarket_sales %>%
  group_by(`Product.line`)  

# Create a plot
sales_productline %>%   
  ggplot(aes(x=`Product.line`, y=Total)) +
  geom_jitter(width = 0.05, height = 0, color="grey80") +
  geom_boxplot(fill="lightblue", alpha=0.5,outlier.color = NA) # alpha sets the opacity of a geom

# This trick updates the order of the factor levels by the Product line
sales_productline_ordered <- supermarket_sales %>%
  mutate(`Product.line` = fct_reorder(`Product.line`, # reorder the category
                                      Total, median,.desc=TRUE))

# Create a new plot
set.seed(201)
sales_productline_ordered %>%   
  ggplot(aes(x=`Product.line`, y=Total)) +
  geom_jitter(width = 0.05, height = 0, color="grey80") +
  geom_boxplot(fill="lightblue", alpha=0.5,outlier.color = NA)

# Improve the plot by making horizontal boxplots.
set.seed(201)
sales_productline_ordered %>%   
  ggplot(aes(y=`Product.line`, x=Total)) + # we need to switch x and y-axis.
  geom_jitter(width = 0, height = 0.05, color="grey80") + # we also need to switch width and height
  geom_boxplot(fill="lightblue", alpha=0.5,outlier.color = NA) +
  labs(title="Fashion accessories have the lowest median total purchase.\n Total purchases are similar across other product lines.",
       caption = "Kaggle: Supermarket Sales",
       x="Total purchase", y="") +
  theme_minimal() +
  theme(axis.ticks = element_blank(), # remove ticks
        axis.title.x = element_text(face="bold",margin = margin(t = 10)), # x-axis title is too close to axis ticks labels.
        plot.caption = element_text(face="italic"),
        plot.title = element_text(size=12),
        panel.grid.minor = element_blank(), # remove minor grid lines
        panel.grid.major.y = element_line(color="grey90"),
        panel.grid.major.x = element_blank(),
        panel.background = element_rect(color  = 'yellow')) +# remove vertical major grid lines on
  scale_y_discrete(labels=c("Health and\n beauty","Sports and\n travel","Home and\n lifestyle","Food and\n beverages","Electronic\n accessories", "Fashion\n accessories" )) 

# Another way of improving the plot
sales_productline_ordered %>%   
  ggplot(aes(x=`Product line`, y=Total)) +
  geom_jitter(width = 0.05, height = 0, color="grey80") +
  geom_boxplot(fill="lightblue", alpha=0.5,outlier.color = NA) + 
  labs(title="Fashion accessories have the lowest median total purchase.\n Total purchases are similar across other product lines.",
       caption = "Kaggle: Supermarket Sales",
       y="Total purchase", x="") +
  theme_minimal() +
  theme(axis.ticks = element_blank(), # remove ticks
        axis.title = element_text(face="bold"), 
        plot.caption = element_text(face="italic"),
        plot.title = element_text(size=12),
        panel.grid.minor = element_blank(), # remove minor grid lines
        panel.grid.major.x = element_line(color="grey95"),
        panel.grid.major.y = element_blank()) +# remove vertical major grid lines on
  scale_x_discrete(labels=c("Health and\n beauty","Sports and\n travel","Home and\n lifestyle","Food and\n beverages","Electronic\n accessories", "Fashion\n accessories" )) 



