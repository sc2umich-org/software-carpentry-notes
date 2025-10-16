# preamble
library(tidyverse)

gapminder_1997 <- read_csv("data/gapminder_1997.csv")

# Learning about assigning objects to values
#  name <- "Moses"
#  age <- 17
# Update age
#  age <- 19
# You cannot start a variable name with a number
# You should not start with a capital letter
# You cannot use spaces in variable names
# read_csv is a function
# Append a ? before a function to get a help page
# Sys.Date() returns the Year-Month-Date
# getwd() returns the current working directory
# sum() without arguments returns 0
# in round(), you can specify how many decimal places
# Ex: round(3.1415, digits = 2)

# Plot data!
#ggplot(data = gapminder_1997) + 
  #aes(x = gdpPercap) +
  #labs(x = "GDP Per Capita") +
  #aes(y = lifeExp) +
  #labs(y = "Life Expectancy (Years)") +
  #geom_point() +
  #labs(title = "Do people in wealthy countries live longer?") +
  #aes(color = continent) +
  #scale_color_brewer(palette = "Dark2") +
  ##scale_color_viridis_d(option = "turbo") +
  #aes(size = pop/1000000) +
  #labs(size = "Population (in millions)") +
  #aes(shape = continent)

# You need to add the aes before your can add labs  
# The + adds "grammar", it adds function. 
# These can also be under the same additional function call with commas separating each new addition
# aes is short for aesthetics, this is for visual aspects like axis data set, it doesn't actually plot the data
# labs is short for labels

# Cleaned up version
  
ggplot(data = gapminder_1997)+
  aes(x = gdpPercap, 
      y = lifeExp, 
      color = continent, 
      size = pop/1000000) +
  labs(title = "Do people in wealthy countries live longer?", 
       x = "GDP Per Capita", 
       y = "Life Expectancy (Years)", 
       size = "Population (in millions)") + 
  geom_point() +
  scale_color_brewer(palette = "Dark2")

# All gapminder data

gapminder_data <- read_csv("gapminder_data.csv")

# Dimensions of the data
# dim(gapminder_data)
# Preview some data
# head(gapminder_data)

ggplot(data = gapminder_data) +
  aes(x = year,
      y = lifeExp,
      color = continent,
      group = country) +
  #geom_point()
  geom_line()

# Plot population vs. lifexp

ggplot(data = gapminder_data) +
  aes(x = pop/1000000,
      y = lifeExp,
      color = continent,
      group = country) + 
  geom_line() +
  labs(title = "Population vs. Life Expectancy",
       x = "Population (in millions)",
       y = "Life Expectancy (years)")



# Discreet plots


# Box Plots

ggplot(data = gapminder_1997) +
  aes(x = continent,
      y = lifeExp) +
  geom_boxplot()

# Violin plot instead?

ggplot(data = gapminder_1997) +
  aes(x = continent,
      y = lifeExp) +
  geom_violin() +
  geom_jitter()

# Width gives the distribution of the specific life expectancies. Oceania only has 2 countries so it looks as such.
# geom_jitter is like geom_point, but it randomly separates the points horizontally so you can identify each individual point
# Order of functions in important in plotting, each additional geom call puts said plot on top of the other plot

#alternative for calling aes with+
ggplot(data = gapminder_1997,
       mapping = aes(x = continent,
                     y = lifeExp)) + 
  geom_violin() +
  geom_jitter(aes(size = pop/1000000)) +
  labs(size = "Population (in millions)")

# Colored violin

ggplot(data = gapminder_1997,
       mapping = aes(x = continent,
                     y = lifeExp)) + 
  # geom_violin(color = "cadetblue",
  #             fill = "purple")
  geom_violin(aes(fill = continent), 
              alpha = 0.5)

# aes(fill = "springgreen") XXX wrong!
# fill = "springgreen" (no aes) correct!

# Histogram of life expectancy

ggplot(gapminder_1997,
       aes(x = lifeExp)) +
  #geom_histogram() we want to specify bin amount!
  geom_histogram(bins = 30)

ggplot(gapminder_1997, 
       aes(x = lifeExp)) +
  geom_density()

# Themes

ggplot(gapminder_1997,
       aes(x = lifeExp)) +
  #geom_histogram() we want to specify bin amount!
  geom_histogram(bins = 30) +
  #theme_bw()
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 1))

# Facets
ggplot(gapminder_1997,
       aes(x = gdpPercap,
           y = lifeExp)) +
  geom_point() +
  #facet_wrap(vars(continent))
  facet_grid(rows = vars(continent))


# How to save a plot

#publish, export, ggsave

ggsave("awesome_plot.jpeg",
       width = 6,
       height = 4)

