# -------------------------------------------------
# BUDT758D: Data Visualization & Web Analytics
# Week 5 Section 10: Time series
# -------------------------------------------------
# To wrap long lines in this file, try Code -> Soft Wrap Long Lines
setwd("G:\\Œ“µƒ‘∆∂À”≤≈Ã\\Graduate course\\R dataset")

# install.packages("tidyverse")
library(tidyverse)
library(lubridate)
library(zoo)
library(readxl)
bike_data <- read_xlsx('bike_sharing_data.xlsx',sheet = 'data')

# ---------------------
# Section 1. Bar chart
# ---------------------

# Create a chart to show the quarterly average demand.
bike_data$date <- as.Date(bike_data$date,format= "%y-%m-%d")

bike_data_quarter <-  bike_data %>%
  mutate(date = as.Date(date,"%m/%d/%y")) %>%
  mutate( year = year(date), quarter = quarter(date)) %>%
  mutate(yearqaurter = paste(year,"-", quarter, sep = "")) %>%
  group_by(yearqaurter) %>%
  summarise(mean_total_count = mean(count))

ref_mean <- mean(bike_data_quarter$mean_total_count)
bike_quarter_line <- bike_data_quarter %>%
  ggplot(aes(x = yearqaurter, y = mean_total_count)) +
  geom_col() + 
  # Add a reference line which shows the mean value
  geom_hline(yintercept = ref_mean, col = 'orange', lty = 2, lwd = 1.2)
bike_quarter_line
# ------------------
# In-class activity 
# ------------------
# Highlight bars that are above the average
# Improve the chart
# Save the chart and submit the cart on Canvas
bike_data_quarter <- bike_data_quarter %>%
  mutate(col_ind = ifelse(mean_total_count > mean(mean_total_count),"Y","N"))
ref_mean <- mean(bike_data_quarter$mean_total_count)
bike_quarter_bar <- bike_data_quarter %>%
  ggplot(aes(x = yearqaurter, y = mean_total_count)) +
  geom_col(aes(fill=col_ind))  +
  # Add a reference line which shows the mean value
  geom_hline(yintercept = ref_mean, col = 'black', lty = 2, lwd = 1.2) + 
  labs(x = "Year and quarter",
       y = "Mean hourly bike-sharing demand",
       caption = "Kaggle: Bike-shaing demand",
       title = "Bike-shaing demand in the most recent three quaters is\n much higher than before") +
  # Set up the theme
  theme_bw() +
  theme(axis.title.x = element_text(face="bold",margin = margin(t = 10), size=12), # x-axis title is too close to axis ticks labels.
        axis.title.y = element_text(face="bold",margin = margin(r = 10), size=12),
        axis.text = element_text(face="bold",size=14),
        plot.caption = element_text(face="italic"),
        plot.title = element_text(size=14),
        panel.grid.minor = element_blank(), # remove minor grid lines
        panel.grid.major.y = element_line(color="grey50"),
        panel.grid.major.x = element_blank(),
        legend.position = "right") +
  # Manually change y-axis labels
  geom_text(aes(y=mean_total_count-10, 
                label=round(mean_total_count, digits = 0),size=5),
            color="white") +
  scale_fill_manual(values=c("grey","darkgreen")) +
  annotate("text",x="2011-2",y=210,label=paste("Average:",round(ref_mean,1)),size=5)
bike_quarter_bar

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
       title = "Demand in 2012 is higher than in 2011. Bike-sharing is more popular when weather condition is better.",
       caption = "Source: Kaggle bike-sharing demand") + 
  theme_classic() + 
  theme(axis.text = element_text(face="bold",size=10),
        plot.caption = element_text(face="italic"),
        plot.title = element_text(size=14),
        panel.grid.major.y = element_line(color="grey85")
        )+ 
  scale_x_date(date_labels = "%B %Y") + # change the labels on x-axis
# Add a vertical line at 2012-01
  geom_vline(xintercept = as.Date("2012-01-01"), col = 'grey60', lty=2)

# b. Produce multiple lines to show the average hourly demand by months and by weather conditions.

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
month_weather_line <- yearmonth_chart +
  labs(y = "Average bike-sharing demand",
       x = "",
       title = "Demand in 2012 is higher than in 2011. Bike-sharing is more popular when weather condition is better.",
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

month_weather_line

# -----------------------------
# Section 3. Stacked area chart
# -----------------------------

# Create a stacked area chart to show average hourly rentals by month and by use types (registered or casual users).

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
registercasual_area <- areachart +
  labs(x = "",
       y = "Bike-shaing demand",
       title = "Rentals from registered users are consistantly higher than those from \ncasual users",
       caption = "Source: Kaggle bike-sharing demand") + 
  scale_x_date(expand=c(0,0), date_labels = "%b %Y") + 
  scale_y_continuous(expand=c(0,0)) +
  theme_classic() + 
  theme(axis.text = element_text(face="bold",size=10),
        plot.caption = element_text(face="italic"),
        plot.title = element_text(size=14),
        panel.grid.major.y = element_line(color="grey85"),
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  annotate("text", x = as.Date("2011-07-01"), y = 25, size=5,
           label = "Causual users",col='white') +
  annotate("text", x = as.Date("2011-07-01"), y = 100, size=5,
           label = "Registered users",col='white') 

registercasual_area

# -----------------------------
# Section 4. Combine charts
# -----------------------------

# Add a text box with some insights.
insights_box <-  ggplot() +
  lims(x=c(-0.1,0.1),y=c(-0.1,0.1)) +
  theme_void() + # empty theme
  annotate("text",-0.1,0,label='Bike-sharing is more popular under better weather conditions. \nRentals from registered users are consistantly higher than those from casual users. \nAdditionally, bike-sharing demand is increasing fast in the most recent three quarters.', hjust=0, size=5, fontface = "bold") +
  xlab(NULL)

# Since now we have a text box with insights, we can use simple titles in the individual charts.
bike_quarter_bar1 <- bike_quarter_bar+
  labs(title="Hourly bike-sharing demand from 2011 Q1 to 2012 Q4")
registercasual_area1 <- registercasual_area +
  labs(title="Rentals from registered users and casual users")
month_weather_line1 <- month_weather_line +
  labs(title="Deamand and weather conditions")

library(patchwork)

# Patchwork is easy to use but we cannot change the size of the charts.
comb_patchwork <- insights_box/(bike_quarter_bar1 + registercasual_area1)/month_weather_line1
comb_patchwork
ggsave("bike_sharing_stud_patchwork.jpeg",comb_patchwork,width = 12,height = 11)

# Using the ggdraw function in package "cowplot" will enable us to change the size of the charts

library(cowplot) # streamlined plot theme for ggplot

combined_figure = ggdraw() +
  draw_plot(insights_box, x = 0, y = 0.9, width = 1, height = 0.1) +
  draw_plot(bike_quarter_bar1, x = 0, y = .45, width = .5, height = .45) +
  draw_plot(registercasual_area1, x = 0.5, y = 0.45, width = 0.5, height = .45) +
  draw_plot(month_weather_line1, x = 0, y = 0, width = 1, height = 0.45) 

ggsave("bike_sharing_study.jpeg",combined_figure,width = 12,height = 11.5)

