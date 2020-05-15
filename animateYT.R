# Library
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(gganimate)
library(gifski)
library(av)
library(gapminder)

# Data
data <- read.csv(file.choose(), header = T)

# Handling dates
datanew <- data %>% 
  mutate(date_confirmation = dmy(date_confirmation))

# Animated time series plot
datanew %>% group_by(date_confirmation) %>% 
  summarise(count = n()) %>% 
  mutate(cuml = cumsum(count)) %>% 
  ggplot(aes(x = date_confirmation, y = cuml)) +
  geom_line(color = 'red') +
  geom_point(size = 1.5) +
  geom_area(fill = 'pink') 

# Data completion
datanew$day <- day(datanew$date_confirmation)
datanew$month <- month(datanew$date_confirmation)

new <- datanew %>% 
  filter(month == 3) %>% 
  group_by(day, country) %>% 
  summarise(count = n())
new <- data.frame(complete(new, day, country,
                           fill = list(count = 0)))

# Animated daily line plot
new %>% filter(country == 'United States' |
                 country == 'France' |
                 country == 'United Kingdom' |
                 country == 'Germany') %>% 
  ggplot(aes(x = day, y = count, 
             group = country,
             color = country)) +
  geom_line() +
  geom_point() 

# Data for bar plot
new <- datanew %>% 
  filter(country == 'United States' |
           country == 'France' |
           country == 'United Kingdom' |
           country == 'Germany') %>% 
  filter(month == 2| month == 3) %>% 
  group_by(country, month) %>% 
  summarise(count = n()) 

# Bar plot
p <- new %>% ggplot(aes(x = country, 
                   y = count,
                   fill = country)) +
  geom_bar(stat = 'identity') +
  geom_point(size = 1.5) +
  scale_y_log10() +
  theme_bw() +
  guides(fill = F) 

# Animated bar plot by month
p + transition_time(as.integer(month)) 

# Animated bar plot by country
p + transition_states(count) 

# Data
p <- ggplot(gapminder,
            aes(x = gdpPercap, y = lifeExp,
                size = pop, color = country)) +
  geom_point(show.legend = F, alpha = 0.7) +
  scale_x_log10() +
  labs(x = 'GDP Per Capita',
       y = 'Life Expectancy') +
  scale_size(range = c(2, 15))

# Animated bubble plot
p + transition_time(year) 

