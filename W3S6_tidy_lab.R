# -------------------------------------------------
# BUDT758D: Data Visualization & Web Analytics
# Week 3 Section 6: Getting started with tidyverse
# -------------------------------------------------

# To wrap long lines in this file, try Code -> Soft Wrap Long Lines
# This script is adapted from R for Data Science (R4DS)

# install.packages("tidyverse")
library(tidyverse)


# ------------------
# SECTION 1: Tibbles
# ------------------

# Most R packages use regular data frames, but packages in tidyverse use "tibbles" 

# You can turn a data frame into a tibble:
as_tibble(iris)

# Or you can build a tibble from scratch:
tibble(
  x = 1:5, 
  y = 1, 
  z = x ^ 2 + y
)

# There are two main differences between data frames and tibbles:
# 1) Tibbles print much nicer (for instance, each column reports its type)
# 2) You can subset columns in tibbles by name using $ or by position using [[]]
df <- tibble(
  x = runif(5),
  y = rnorm(5)
)
# Extract by name
df$x
df[["x"]]
# Extract by position
df[[1]]

# You can also turn a tibble back into a data frame
as.data.frame(df)

# Load in the data and create a tibble
currentdir <- getwd()
setwd(currentdir) # set working directory
t1 <- proc.time()
movies <- read_csv("recent_movies.csv")
proc.time() - t1

t1 <- proc.time()
movies_1 <- read.csv("recent_movies.csv")
movies <- as_tibble(movies_1)
proc.time() - t1

# ----------------
# SECTION 2: dplyr
# ----------------

# Adapted from chapter 5 of R for Data Science.

# Load a library with the data of flights departed from NYC in 2013.
# install.packages("nycflights13") 
library(nycflights13)

# Read the documentation on this data
?flights
# Print the tibble
flights

# There are 5 functions that make up most of dplyr's capabilities: filter, arrange, select, mutate, summarise (and group_by).

# filter() allows us to subset observations based on values
# what does this line of code do?
jan1 <- filter(flights, month==1, day==1) 

# what about this one?
filter(flights, month==11 | month==12) 

filter(flights, month %in% c(11,12)) 

# You can do more complicated filtering
filter(flights, month==11, day <= 10)

# Arrange() changes the order of rows based on values of variables. 
# this statement reorders the rows in flights by year then month then day.
# by default, in ascending order.
arrange(flights, year, month, day)

# Use desc() to sort in descending order
arrange(flights, desc(dep_delay))
arrange(flights, desc(month), desc(day))
# Note that arrange() puts na's at the end!

# select() subsets variables
select(flights, year, month, day) 

# Selects all columns from year to day
select(flights, year:day) 

# Selects all columns except those from year to day
select(flights, -(year:day))

# Related: you can rename columns
rename(flights, tail_num = tailnum) 

# mutate() adds new variables that are functions of existing ones

# Lets make a smaller tibble so we can look more closely
flights_sml <- select(flights, year:day, ends_with("delay"), distance, air_time) 

# Add some new columns: gain and speed
mutate(flights_sml,
       gain = dep_delay - arr_delay,
       speed = distance / air_time * 60)

# We can also refer to the new columns:
mutate(flights_sml,
       gain = dep_delay - arr_delay,
       hours = air_time / 60,
       gain_per_hour = gain / hours)

# Transmute() keeps only the newly created variables
transmute(flights,
          gain = dep_delay - arr_delay,
          hours = air_time / 60,
          gain_per_hour = gain / hours)

# There are tons of useful functions you can use for mutate. See Section 5.5.1 for more!

# summarise() collapses data frames and is used in conjunction with group_by()
# this code calculates the average delay per date
# Note that na.rm argument ignores missing values
by_day <- group_by(flights, year, month, day)
summarise(by_day, delay = mean(dep_delay, na.rm=TRUE))

# Combining operations with pipe.

# Let's look at the relationship between distance and delay by destination.
# We need to group by destination and compute the average distance and delay. We also need to count the number of flights to each destination.
# The following code does all that, plus filters out destinations w/ fewer than 20 flights, and also Honolulu because it's too far.
by_dest <- group_by(flights, dest) 
delay <- summarise(by_dest,
                   count = n(),
                   dist = mean(distance, na.rm = TRUE),
                   delay = mean(arr_delay, na.rm = TRUE)
)

delay <- filter(delay, count > 20, dest != "HNL")

# So far we have to name each intermediate tibble. The pipe function %>% strings statements together into one line of code. You can read %>% as "then". Therefore, the following code reads as "Get the flights data, then group by dest, then summarise, then filter.
delays <- flights %>% 
  group_by(dest) %>% 
  summarise(
    count = n(),
    dist = mean(distance, na.rm = TRUE),
    delay = mean(arr_delay, na.rm = TRUE)
  ) %>% 
  filter(count > 20, dest != "HNL")

# -----------------------
# SECTION 3:  Tidy Data
# -----------------------

# The rules of "tidy data" are best explained in Chapter 12 of R4DS.
# To summarize:
# 1) Each variable must have its own column
# 2) Each observation must have its own row
# 3) Each value must have its own cell
# This sounds overly simple, but most datasets aren't tidy!

# There are a few useful functions to make data tidy: pivot_longer(), pivot_wider(), separate(), and unite()
# Check R4DS 12.3 for documentation.

# Let's work through an example using the tidyr WHO dataset. This data has tuberculosis cases by year, country, age, gender, and diagnosis

# Print the tibble
who
# Look at the documentation for the dataset
?who

# It looks like country, iso2, and iso3 are redundant variables for country. Also, new_sp_m014:new_rel_f65 are currently variables. We use pivot_longer() to turn variables into values.
# The following code turns new_sp_m014:new_rel_f65 into one column. It makes the original variable names a new column called "key", then the values are put into another new column named "cases".
# If you want to print all the columns, do print(data.frame(who))
who1 <- who %>% 
  pivot_longer(
    cols = new_sp_m014:newrel_f65, 
    names_to = "key", 
    values_to = "cases",
    values_drop_na = TRUE
    )
who1

# Now instead of 60 columns, who1 has 6 columns. The variable "key" has the following codes (from ?who): 
# The names for columns five through 60 are made by combining "new_"  to a code for method of diagnosis (rel = relapse, sn = negative pulmonarysmear, sp = positive pulmonary smear, ep = extrapulmonary), to a code for gender (f = female, m = male), to a code for age group (014 = 0-14 yrs of age, 1524 = 15-24 years of age, 2534 = 25 to 34 years of age, 3544 = 35 to 44 years of age, 4554 = 45 to 54 years of age, 5564 = 55 to 64 years of age, 65 = 65 years of age or older).

# Therefore, next we pull out all the information encoded in "key" into new variables
levels(as.factor(who1$key)) # Inspect levels in variable "key".
# or use colnames
colnames(who)

# Notice that we should have "new_rel" but instead it's "newrel", so we need to fix it using mutate() 
who2 <- who1 %>%  
  mutate(names_from = stringr::str_replace(key, "newrel", "new_rel")) 
who2

# Now, we can use separate() to separate the "key" variable using "_" as the separator
who3 <- who2 %>% 
  separate(names_from, c("new", "type", "sexage"), sep = "_") 
who3

# The last step is to separate the sexage column into sex and age
#we separate after the first character
who4 <- who3 %>% 
  separate(sexage, c("sex", "age"), sep = 1)
who4

#Lets drop some redundant columns
who5 <- who4 %>% 
  select(-new, -iso2, -iso3)
who5

# ------------------------------
# SECTION 4: Try it by yourself!
# ------------------------------

# fivethirtyeight.com is a famous data journalism website. They have generously released some of the datasets they've used to write articles as a public Rlibrary. You may have to do:
install.packages("fivethirtyeight")

# Load the fivethirtyeight library and look at the "drinks" tibble.
library(fivethirtyeight)

# We are going to be looking at the drinks data.
data(drinks)
count(drinks)
drinks

# Q1. We need to treat servings of alcohol as a value, with the type of alcohol as a new variable. Create a new tibble that does the following:
# 1) Turn variables into values: Treat the type of alcohol as a new variable (new column) called "type". Then, the servings values should be put into another new column named "servings".
# 2) Alcohol type should be "beer" or "wine" or "spirit", not "beer_servings", "spirit_servings" and "wine_servings". 
# 3) Drop total_litres_of_pure_alcohol.
# The resulting table should have 3 columns and 579 rows.

drinks1 <- drinks %>% 
  pivot_longer(
    cols = beer_servings:wine_servings, 
    names_to = "type", 
    values_to = "servings",
    values_drop_na = TRUE
  )
drinks1
drinks2 <- drinks1 %>% separate(type,c('type','drop'),sep='_')
drinks2 <- drinks2 %>% select(-2,-4)
drinks2
# Attach a screenshot of the first 10 rows of your tidy data.

# Q2: Which country drinks the most servings of beer, wine, and spirits, respectively?
# Hint: you may need to use the group_by and filter functions. 
drinks3 <- drinks2 %>% group_by(type)%>% filter(servings >0 ) %>%
  arrange(desc(servings))

drinks3



# Q3: Which country of those with total servings (beer+wine+spirits) > 0 has the highest proportion of wine servings?
# Hint: add new columns of the proportion of wine servings.
drinks4 <- drinks2 %>% group_by(country) %>%
    mutate(total_servings = sum(servings))%>%
    filter(total_servings >0 & type =='wine') %>%
    mutate(proportion = servings/total_servings) %>%
    ungroup()%>% #make it still a group 
    filter(proportion == max(proportion))

drinks4


# Q4: What percent of total servings consumed in the USA are beer, wine, and spirits, respectively?
# 
drinks5 <- drinks2 %>% filter(country == 'USA') %>% 
  mutate(prop = servings/sum(servings))


drinks5
