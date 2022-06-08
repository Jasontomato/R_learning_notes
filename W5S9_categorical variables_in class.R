# -------------------------------------------------
# BUDT758D: Data Visualization & Web Analytics
# Week 5 Section 9: Bar Charts
# -------------------------------------------------
# To wrap long lines in this file, try Code -> Soft Wrap Long Lines
setwd("G:\\ÎÒµÄÔÆ¶ËÓ²ÅÌ\\Graduate course\\758D Data Visualization&Web\\data")

# install.packages("tidyverse")
# install.packages("GGally")
library(tidyverse)
library(GGally)

# set working directory to the current folder
# Session -> set working directory -> To Source File Location

# Read Data
sales <- read.csv("supermarket_sales_data.csv")

# -----------------------------
# Section 1. Basic Bar chart
# -----------------------------

# Create a chart that shows the mean of sales per product line
# Compute the mean of Sales per Product Line
sales_productline <- sales %>%
  group_by(`Product.line`) %>%
  summarise(mean_total = mean(Total)) 

sales_productline %>%
  ggplot(aes(x=`Product.line`, y=mean_total)) +
  geom_col()

sales_productline_ordered <- sales_productline %>%
  mutate(`Product.line` = fct_reorder(`Product.line`, mean_total, .desc=TRUE))%>%
  mutate(color_id = ifelse(mean_total>306,'T','N'))
    

productline_bar <- sales_productline_ordered %>%
  ggplot(aes(y=`Product.line`, x=mean_total)) +
  geom_col(fill='darkred') 

productline_bar

# Improve the chart
productline_bar1 <- productline_bar +
  labs(x = "Mean Total Sales",
       y = "",
       caption = "Source: Supermarket Sales on Kaggle",
       title = "Fashion accessories have the lowest sales") +
  theme_minimal() +
  theme(axis.title.x = element_text(face="bold",margin = margin(t = 10)), # x-axis title is too close to axis ticks labels.
        axis.text = element_text(face="bold",size=10),
        plot.caption = element_text(face="italic"),
        plot.title = element_text(size=14),
        panel.grid.minor = element_blank(), # remove minor grid lines
        panel.grid.major.x = element_line(color="grey70"),
        panel.grid.major.y = element_blank()) 
productline_bar1

# Manually change y-axis labels
# Print out product categories
levels(factor(sales_productline_ordered$`Product line`))

productline_bar2 <- productline_bar1 +
  scale_y_discrete(labels = c("Home and \nlifestyle","Sports and\n travel", "Health and \nbeauty", "Food and \nbeverages", "Electronic \naccessories", "Fashion \naccessories")) 
productline_bar2

# Add data labels
productline_bar2 +
  geom_text(aes(x=mean_total-20, 
                label=round(mean_total, digits = 0)),
            color="white") 

sales_productline_ordered$mean_total-20
# ------------------------------------------
# Section 2. Cluster and Stacked bar chart
# ------------------------------------------
# Create a chart of the number of Purchases by Payment method and Gender.
sales_paycity <-  sales %>%
  group_by(Payment, Gender) %>%
  summarize(count = n()) 

clusterbar <- sales_paycity %>%
  ggplot(aes(x=Gender, y=count)) +
  geom_col(aes(fill=Payment),width=0.7,position="dodge") # it will create a stacked bar chart if we remove position = "dodge".
clusterbar

clusterbar +
  labs(y = "Number of purchases",
       x = "",
       caption = "Source: Supermarket Sales on Kaggle",
       title = "Cash and Ewallet are the most prefered payment method for \nfemale and male customers, respectively.") +
  theme_minimal() +
  theme(axis.title.x = element_text(face="bold"), # x-axis title is too close to axis ticks labels.
        axis.text = element_text(face="bold",size=10),
        legend.position="bottom",
        legend.margin = margin(t=-15),
        plot.caption = element_text(face="italic"),
        plot.title = element_text(size=14),
        panel.grid.minor = element_blank(), # remove minor grid lines
        panel.grid.major.y = element_line(color="grey70"),
        panel.grid.major.x = element_blank()) +
  scale_fill_brewer(name="",palette ="RdYlBu")

ggsave('clusterbarchart.jpeg', clusterbar1, width=8, height=8)
# -------------------------
# In-class activity
# -------------------------
# Create a stacked bar chart of the number of properties by "bedrooms" and "conditions".
# Filter out bedrooms = 1

house_price <- read_csv("kc_house_data_sample.csv")
house_conbed <-  house_price %>%
  # 1. filter out bedrooms=1
  # 2. before grouping by bedrooms and condition, turn these two variables into categorical variables.

# Create a stacked bar chart. x-axis represents the number of bedrooms; y-axis represents the number of properties; color represents conditions
clusterbar <- house_conbed %>%
  ggplot(aes(x=bedrooms, y=count)) +
  geom_col(aes(fill=condition),width=0.7) # it will create a stacked bar chart if we remove position = "dodge".

# Improve the chart: add labels, change grid lines, change the position of the legend, change labels on the x-aixs, change color palette, etc. 

# save your chart to a jpeg file upload on Canvas.


