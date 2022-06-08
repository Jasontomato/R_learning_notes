# -------------------------------------------------
# BUDT758D: Data Visualization & Web Analytics
# Week 5 Section 10: Time series
# -------------------------------------------------
# To wrap long lines in this file, try Code -> Soft Wrap Long Lines
setwd('G:\\Œ“µƒ‘∆∂À”≤≈Ã\\Graduate course\\758D Data Visualization&Web\\data')
# install.packages("tidyverse")
library(tidyverse)
library(lubridate)
library("readxl")
bike_data <- read_excel('bike_sharing_data.xlsx',sheet = 'data')

# ---------------------
# Section 1. Bar chart
# ---------------------

# Create a chart to show the quarterly average demand.

bike_data_quarter <-  bike_data %>%
  mutate(date = as.Date(date,"%m/%d/%y",tz='UTC')) %>%
  mutate( year = year(date), quarter = quarter(date)) %>%
  mutate(yearqaurter = paste(year,"-", quarter, sep = "")) %>%
  group_by(yearqaurter) %>%
  summarise(mean_total_count = mean(count))

ref_mean <- mean(bike_data_quarter$mean_total_count)
bike_data_quarter %>%
  ggplot(aes(x = yearqaurter, y = mean_total_count)) +
  geom_col() + 
  # Add a reference line which shows the mean value
  geom_hline(yintercept = ref_mean, col = 'orange', lty = 2, lwd = 1.2)

# ---------------------
# In-class activity 1
# ---------------------

# Highlight bars that are above the average
highlight_bike = bike_data_quarter %>% 
  mutate(ToHighlight = ifelse( yearqaurter == c('2012-3','2012-2','2012-4') , "yes", "no" ))
# Improve the chart

# Save the chart and submit the cart on Canvas

# ---------------------
# Section 2. Line chart
# ---------------------

# a. Create a line chart to show the monthly average demand.

bike_data_month <- bike_data %>%
  mutate(date = as.Date(date,"%m/%d/%y")) %>%
  mutate(yearmonth = as.Date(as.yearmon(date))) %>% # extract year and month
  group_by(yearmonth) %>%
  summarise(mean_total_count = mean(count), mean_registered = mean(registered), mean_casual = mean(casual)) 

# Create an initial chart
yearmonth_chart <- bike_data_month %>%
  ggplot(aes(x=yearmonth, y = mean_total_count)) +
  geom_line(col='blue2', lwd=1.3) 
yearmonth_chart

# Improve the chart
yearmonth_chart +
  labs(y = "Average bike-sharing demand",
       x = "",
       title = "Demand in 2012 is higher than in 2011. Bike-sharing is more popular in summer.",
       caption = "Source: Kaggle bike-sharing demand") + 
  theme_classic() + 
  theme(axis.text = element_text(face="bold",size=10),
        plot.caption = element_text(face="italic"),
        plot.title = element_text(size=14),
        panel.grid.major.y = element_line(color="grey85")) +
  scale_x_date(date_labels = "%B %Y") + # change the labels on x-axis
  # Add a vertical line at 2012-01
  geom_vline(xintercept = as.Date("2012-01-01"), col = 'grey60', lty=2)

# a. Produce multiple lines to show monthly demand by weather conditions.

# Prepare the data
bike_data_month <- bike_data %>%
  mutate(date = as.Date(date,"%m/%d/%y")) %>%
  mutate(yearmonth = as.Date(as.yearmon(date)), weather=factor(weather)) %>%
  group_by(yearmonth,weather) %>%
  summarise(mean_total_count = mean(count), mean_registered = mean(registered), mean_casual = mean(casual)) 

# Create an initial chart
yearmonth_chart <- bike_data_month %>%
  ggplot(aes(x=yearmonth, y = mean_total_count, col=weather)) +
  geom_line(lwd=1.3) +
  geom_point(size=2) # Add points to the chart
yearmonth_chart

# Improve the chart
yearmonth_chart +
  labs(y = "Average bike-sharing demand",
       x = "",
       title = "Demand in 2012 is higher than in 2011. Bike-sharing is more popular in summer.",
       caption = "Source: Kaggle bike-sharing demand") + 
  theme_classic() + 
  theme(axis.text = element_text(face="bold",size=10),
        plot.caption = element_text(face="italic"),
        plot.title = element_text(size=14),
        panel.grid.major.y = element_line(color="grey85"),
        legend.position = c(0.8,0.1),
        legend.direction = "horizontal",
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  scale_x_date(date_labels = "%b %Y") + 
  # Add a vertical line at 2012-01
  geom_vline(xintercept = as.Date("2012-01-01"), col = 'grey60', lty=2) + 
  scale_color_brewer(name="Weather condition", palette = "RdYlBu") +
  # Add a text box with the information of weather condition
  annotate("text", x = as.Date("2011-01-01"), y = 340, hjust=0, size=3,
           label = "Weather condition:
1: Clear, Few clouds, Partly cloudy, Partly cloudy
2: Mist+Cloudy, Mist+Broken clouds, Mist+Few clouds, Mist
3: Light Snow, Light Rain+Thunderstorm+Scattered clouds, Light Rain+Scattered clouds
4: Heavy Rain+Ice Pallets+Thunderstorm+Mist, Snow+Fog ") +
  # Change xlim so we can see the entire text box
  lims(y = c(0,360))

# -----------------------------
# Section 3. Stacked area chart
# -----------------------------

# Create a stacked area chart to show monthly average rentals from registered and casual users.

# Prepare data
bike_data_month1 <- bike_data %>%
  mutate(date = as.Date(date,"%m/%d/%y")) %>%
  mutate(yearmonth = as.Date(as.yearmon(date))) %>%
  group_by(yearmonth) %>%
  summarise(mean_total_count = mean(count), mean_registered = mean(registered), mean_casual = mean(casual)) 
bike_data_month1

# Create the initial chart
areachart <- bike_data_month1 %>%
  ggplot() +
  geom_area(aes(x=yearmonth,y=mean_total_count),fill='steelblue') +
  # Since mean_casual is another column, we can simply add it to the chart by calling another geom_area function
  geom_area(aes(x=yearmonth,y=mean_casual),fill='lightblue') 
areachart

# Improve the chart
areachart +
  labs(x = "",
       y = "Bike-shaing demand",
       title = "Rentals from registered users are consistantly higher than those from \ncausal users",
       caption = "Source: Kaggle bike-sharing demand") + 
  scale_x_date(expand=c(0,0), date_labels = "%b %Y") + 
  scale_y_continuous(expand=c(0,0)) +
  theme_classic() + 
  theme(axis.text = element_text(face="bold",size=10),
        plot.caption = element_text(face="italic"),
        plot.title = element_text(size=14),
        panel.grid.major.y = element_line(color="grey85"),
        legend.position = c(0.8,0.1),
        legend.direction = "horizontal",
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  annotate("text", x = as.Date("2011-07-01"), y = 25, size=5,
           label = "Causual users",col='white') +
  annotate("text", x = as.Date("2011-07-01"), y = 100, size=5,
           label = "Registered users",col='white') 

# Another toy example
data <- tibble(x = c(1,1,2,2,3,3,4,4,5,5), y = c(1,3,2,6,1,8,7,10,9,15), category = c("A","B","A","B","A","B","A","B","A","B"))
data %>%
  ggplot() +
  geom_area(aes(x=x,y=y,fill=category))

# ---------------------
# In-class activity 2
# ---------------------
digital_sales <- read_csv('digital_sales.csv')

# Create a chart to show the four time series of retail/digital sales in California/New York.
