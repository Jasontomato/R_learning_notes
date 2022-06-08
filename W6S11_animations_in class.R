# -------------------------------------------------
# BUDT758D: Data Visualization & Web Analytics
# Week 6 Section 11: Animation
# -------------------------------------------------
# To wrap long lines in this file, try Code -> Soft Wrap Long Lines

library(tidyverse)
library(gapminder) # Excerpt of the Gapminder data on life expectancy, GDP per capita, and population by country.
library(gganimate)
library(scales)
library(gifski)
# -----------------------------
# Section 1. Animation by year
# -----------------------------
# This is a scatter plot over years. Year ranges from 1952 to 2007 in increments of 5 years. Each point represent a country in a specific year. 

scatter_all <- gapminder %>%
  ggplot(aes(x=gdpPercap, y=lifeExp, size = pop, colour = country)) +
  geom_point(alpha = 0.7) + # make the points transparent
  labs(title ='World Health vs. Poverty',
       x = "GDP per capita", y = "Life expectancy") +
  scale_colour_manual(values = country_colors) + #predefined color for country
  scale_size(range = c(2, 12)) +
  scale_x_log10(labels = comma) + # avoid scientific notation
  theme_classic() +
  theme(legend.position = "none")

scatter_all
# Facet Wrap: create a different graph per year. facet_wrap() wraps a 1d sequence panels into 2d. This is generally a better use of screen space.
scatter_time_wrap <- scatter_all + 
  facet_wrap(~year) 
scatter_time_wrap  

# Make an animation
scatter_time_ani <- scatter_all +
  # Here comes the gganimate specific bits
  labs(title ='World Health vs. Poverty',
       subtitle = 'Year: {frame_time}') +
  transition_time(year) +
  ease_aes('linear') # defines how a value change to another
animate(scatter_time_ani, height = 500,  renderer = gifski_renderer(),width = 500)
scatter_time_ani
anim_save("output.gif",scatter_time_ani,height = 500, width = 500)

# look at just Australia and NZ
scatter_time_oceania <- gapminder %>%
  filter(continent == 'Oceania') %>%
  ggplot(aes(gdpPercap, lifeExp, size = pop, colour = country)) +
  geom_point(alpha = 0.7) +
  labs(x = "GDP per capita", y = "Life expectancy") +
  geom_text(aes(x=gdpPercap, y=lifeExp+1, label = country),
            size=4) +
  scale_colour_manual(values = country_colors) +
  scale_size(range = c(2, 12)) +
  scale_x_log10(labels = comma) + 
  theme_classic() +
  theme(legend.position = "none")

scatter_time_oceania

# Cumulative graph
scatter_time_oceania1 <- scatter_time_oceania +
  # Here comes the gganimate specific bits
  labs(title ='World Health vs. Poverty',
       subtitle = 'Adding Year: {current_frame}') +
  transition_manual(year, cumulative = TRUE) + # cumulative = TRUE adds to the graph
  ease_aes('linear')
animate(scatter_time_oceania1,renderer = gifski_renderer(), height = 500, width = 500)

# Non-cumulative graph
scatter_time_oceania2 <- scatter_time_oceania +
  # Here comes the gganimate specific bits
  labs(title ='World Health vs. Poverty',
       subtitle = 'Adding Year: {current_frame}') +
  transition_manual(year, cumulative = FALSE) + #one at a time, abrupt
  ease_aes('linear')
animate(scatter_time_oceania2, height = 500, width = 500)

# ----------------------------------
# Section 2. Animation by continent
# ----------------------------------
scatter_time_continent <- scatter_all +
  facet_wrap(~continent) +
  # Here comes the gganimate specific bits
  labs(title ='World Health vs. Poverty',
       subtitle = 'Adding Year: {frame_time}') +
  transition_time(year) +
  ease_aes('linear')
animate(scatter_time_continent, renderer = gifski_renderer(),height = 500, width = 500)

# Create an animation to fade continents in and out. Consider only the data of 2007.
gapminder_2007 <- gapminder %>%
  filter(year == 2007)

scatter_all_2007 <- gapminder_2007 %>%
  ggplot(aes(x=gdpPercap, y=lifeExp, size = pop, colour = country)) +
  geom_point(alpha = 0.7) + # make the points transparent
  labs(title ='World Health vs. Poverty in 2007',
       x = "GDP per capita", y = "Life expectancy") +
  scale_colour_manual(values = country_colors) +
  scale_size(range = c(2, 12)) +
  scale_x_log10(labels = comma) + # avoid scientific notation
  theme_classic() +
  theme(legend.position = "none")

scatter_cont_ani_fade <- scatter_all_2007 +
  # Here comes the gganimate specific bits
  labs(title="GDP vs. Life Expectancy in 2007",
       subtitle = 'Continent: {closest_state}',
       x = 'GDP per capita', y = 'life expectancy') +
  transition_states(continent, 
                    state_length = 4) +
  enter_appear() +
  exit_fade() +
  ease_aes('linear')
animate(scatter_cont_ani_fade, height = 500, width = 500)

# ------------------
# In-class activity
# ------------------

# Make an animated line chart for the stock market that charts the Close of S&P 500 stock index for each day. Place weekday (in the correct order) on the x-axis, and Close on the y-axis. The data covers the period from Feb 2021 to Feb 2022.

stocks = read.csv("snp_stock.csv")
stocks

# First, convert column "Date" into the date format using as.Date
stocks$Date <- as.Date(stocks$Date,"%d-%b-%y")
# Step 1. Create a chart over the entire time period.
move_line <- stocks%>% 
  ggplot(aes(x=Date))+
  geom_line(aes(y=Open, color = "darkred"))+
  geom_line(aes(y =Close), color="steelblue", linetype="twodash") +
  labs(title="stock price changing from APR2021 to Jan2022",
       x = '', y = 'stock price') +
  theme_classic()
move_line

move<-move_line+
  transition_manual(Date,cumulative = TRUE) +
  ease_aes('linear')
animate(move, renderer = gifski_renderer(), height = 500, width = 500)

move
# Step 2. Create a cumulative chart using transition_manual.

# save your chart
anim_save("output.gif",move,height = 500, width = 500)


