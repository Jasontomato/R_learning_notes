# -------------------------------------------------
# BUDT758D: Data Visualization & Web Analytics
# Week 4 Section 7-8: Scatter plots and Heat Map
# -------------------------------------------------
# To wrap long lines in this file, try Code -> Soft Wrap Long Lines
setwd("G:\\Œ“µƒ‘∆∂À”≤≈Ã\\Graduate course\\758D Data Visualization&Web\\data")

# install.packages("tidyverse")
# install.packages("GGally")
library(tidyverse)
library(GGally)

# Read Data
kc_house <- read.csv('kc_house_data_sample.csv')

# ---------------------------------
# Section 1. Pairwise Scatter Plot
# ---------------------------------
pair_data <- kc_house %>%
  filter(bedrooms >= 2) %>% # very few records with bedroom = 1
  select(bedrooms, sqft_living, sqft_living15, price)%>%
  mutate(bedrooms=factor(bedrooms)) %>%# first way of changing bedrooms to a categorical variable
  mutate(bedrooms = stringr::str_replace(bedrooms, "2", "2-bedroom")) %>%
  mutate(bedrooms = stringr::str_replace(bedrooms, "3", "3-bedroom")) %>%
  mutate(bedrooms = stringr::str_replace(bedrooms, "4", "4-bedroom"))
#  mutate(bedrooms=ifelse(bedrooms==2, "2-bedroom", ifelse(bedrooms==3, "3-bedroom", "4-bedroom"))) # another way of changing bedrooms to a categorical variable

pair_data %>%
  ggpairs(aes(col=bedrooms,alpha = 0.4)) + # alpha sets the opacity of a geom
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) # this line will rotate tick marks on the bottom by 90 degrees. hjust = 1 means the text will be right aligned.

# -----------------------
# Section 2 Scatter Plot
# -----------------------
kc_house %>%
  ggplot(aes(x=sqft_living,y=price/1e6)) +
  geom_point(color='grey40') + 
  geom_smooth(method = "lm", se = TRUE, col='darkred',lty = 2) + # se = FALSE will remove intervals around the line.
  labs(title="Real estate price increases as the size of living space increases.",
       caption = "Kaggle: Supermarket Sales",
       x="Size of  living space (sqft)", y="Price ($millions)") +
  theme_classic() +
  theme(axis.title.x = element_text(face="bold",margin = margin(t = 10)), # x-axis title is too close to axis ticks labels.
        axis.text = element_text(face="bold",size=10),
        plot.caption = element_text(face="italic"),
        plot.title = element_text(size=12),
        panel.grid.minor = element_line(color="grey95"), # remove minor grid lines
        panel.grid.major.y = element_line(color="grey95"),
        panel.grid.major.x = element_blank()) + 
  scale_x_continuous(breaks = seq(1000,6000,1000))
kc_house
# ------------------------------
# Section 3 Correlation heat map
# ------------------------------
# Make the Correlation Data
numeric_variables <- kc_house %>%
  select(sqft_living, sqft_above, sqft_basement,sqft_living15, price)

data_hm <- numeric_variables %>% 
  cor() %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "Var1") %>% 
  pivot_longer(sqft_living:price, 
               names_to="Variable", values_to="corr") 

# Make the chart
heatmap <- data_hm %>%
  ggplot(aes(x=Var1, y=Variable, fill=corr)) + 
  geom_tile()
heatmap

heatmap1 <- heatmap +
  scale_x_discrete(labels= c("Price", "Space \nabove ground", "Basement \nspace","Living \nspace", "Neighbour \nliving space"))  +
  scale_y_discrete(labels= c("Price", "Space \nabove ground", "Basement \nspace", "Living\nspace", "Neighbour \nliving space")) +
  scale_fill_gradient2(name="Correlation",midpoint = 0,
                       low="red4", mid="white", high = "blue4")
heatmap1

# Improve the chart
heatmap1 + 
  labs(title = "Price is positively correlated with all of the other variables.") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(face="bold",size=11),
        plot.title = element_text(face="bold",size=14))


housing <- kc_house %>% select(bedrooms,condition)%>%
  mutate(bedrooms=factor(bedrooms))%>%
  mutate(bedrooms = stringr::str_replace(bedrooms, "1", "1-bedroom")) %>%
  mutate(bedrooms = stringr::str_replace(bedrooms, "2", "2-bedroom")) %>%
  mutate(bedrooms = stringr::str_replace(bedrooms, "3", "3-bedroom")) %>%
  mutate(bedrooms = stringr::str_replace(bedrooms, "4", "4-bedroom"))%>%
  group_by(bedrooms, condition) %>%
  summarize(count = n()) 
housing

stackbar <- housing %>% 
  ggplot(aes(x=condition, y=count)) +
  geom_col(aes(fill=bedrooms),width=0.7) # it will create a stacked bar chart if we remove position = "dodge".
stackbar
ggsave('stackbarchart.jpeg', stackbar, width=8, height=8)
