# preamble
library(tidyverse)

gapminder_data <- read_csv("gapminder_data.csv")

summarize(gapminder_data,
          averageLifeExp = mean(lifeExp))

# This other thing below does the same thing

gapminder_data %>% 
  summarize(averageLifeExp = mean(lifeExp))
  
# %>% is the pipe operator

gapminder_data_summarized <- 
  gapminder_data %>% 
  summarize(averageLifeExp = mean(lifeExp))

# Filter narrows down your rows

gapminder_data %>%
  filter(year == 2007) %>%
  summarize(average = mean(lifeExp))

gapminder_data %>%
  group_by(year) %>%
  summarize(average = mean(lifeExp))

gapminder_data %>%
  group_by(continent) %>%
  summarize(average = mean(lifeExp),
          min = min(lifeExp))

# Notice that these output the same type of data as is put in (dataframe in, dataframe out)

# The mutate function

gapminder_data %>% 
  mutate(gdp = pop * gdpPercap)

gapminder_data %>%
  select(pop, year)

gapminder_data %>%
  select(-continent)

# Create a dataframe with only the country, continent, year, and lifeExp

gapminder_data %>%
  select (-pop, -gdpPercap)

# Data can be described as wide or long, the data we have here in long

# What if we want to turn this long data into wide data, that's where the pivot function comes in

gapminder_data %>%
  select(-pop, -gdpPercap) %>%
  pivot_wider(names_from = year, values_from = lifeExp)

# Filter for the year 2007 and the continent the Americas,
# the year and continent columns and 
# call this new data frame gapminder_data_2007

gapminder_data_2007 <-
  gapminder_data %>%
  filter(year == 2007, continent == "Americas") %>%
  select(-year, -continent)


read_csv("co2-un-data.csv")
# The format is off, row 1 has the headers

read_csv("co2-un-data.csv",
         skip = 1)
# First column header is off, something isn't quite right

co2_emissions_dirty <- 
  read_csv("co2-un-data.csv",
           skip = 2,
           col_names = c('region', 'country', 'year', 
                         'series', 'value', 'footnotes', 
                         'source'))
# c() is the concatenate function

co2_emissions_dirty %>%
  select(-region, -footnotes, -source)

# The series column is not very nice, it's too long

co2_emissions <-
  co2_emissions_dirty %>%
  select(-region, -footnotes, -source) %>%
  mutate(series = recode(series, 
                         "Emissions (thousand metric tons of carbon dioxide)" =
                           "total_emissions",
                         "Emissions per capita (metric tons of carbon dioxide)" =
                           "per_capita_emissions")) %>%
  pivot_wider(names_from = series, values_from = value) %>%
  filter(year == 2005) %>%
  select(-year) %>%
  mutate(country = recode(country, 
                          'Bolivia (Plurin. State of' = 'Bolivia'),
         'United States of America' = 'United States',
         'Venezuela (Boliv. Rep. of)' = 'Venezuela')

# DO NOT break a line between the quotes that specify a string (it will make this recode function break)

# What if we don't know all the possible variations for those series names
# We can use the unique function
# unique(co2_emissions_dirty$series)


# Combining data sets

# We'll use the function inner_join, it uses a "key" to reference how to combine 
# the two datasets

inner_join(gapminder_data_2007, co2_emissions)

# This automatically uses the country name as the key, most likely because they both have this in common
# We can also explicitly tell it to use the country column as the key

inner_join(gapminder_data_2007, co2_emissions, by = 'country')

# Note, " and ' are equivalent in RStudio

# Interesting... our output has 21 rows when the 2007 data has 25
# Let's use anti_join to figure out which rows aren't working together
# basically which rows aren't similar and are being dropped

anti_join(gapminder_data_2007, co2_emissions, by = 'country')

# Bolivia, the US and Venezuela have extra text, Puerto Rico isn't a country, so it isn't included in co2_emissions

co2_emissions <-
  co2_emissions_dirty %>%
  select(-region, -footnotes, -source) %>%
  mutate(series = recode(series, 
                         "Emissions (thousand metric tons of carbon dioxide)" =
                           "total_emissions",
                         "Emissions per capita (metric tons of carbon dioxide)" =
                           "per_capita_emissions")) %>%
  pivot_wider(names_from = series, values_from = value) %>%
  filter(year == 2005) %>%
  select(-year) %>%
  mutate(country = recode(country, 
                          'Bolivia (Plurin. State of)' = 'Bolivia',
                          'United States of America' = 'United States',
                          'Venezuela (Boliv. Rep. of)' = 'Venezuela'))

# What we just did was fix the column names

gapminder_data_2007_fixed <-
  gapminder_data_2007 %>%
  mutate(country = recode(country, 'Puerto Rico' = 'United States')) %>%
  group_by(country) %>%
  summarize(lifeExp = sum(lifeExp * pop)/sum(pop),
            gdpPercap = sum(gdpPercap * pop)/sum(pop),
            pop = sum(pop))

# Then we combine puerto rico with the US


anti_join(gapminder_data_2007, co2_emissions, by = 'country')

gapminder_co2 <-
  inner_join(gapminder_data_2007_fixed, co2_emissions, by = 'country')

# Now we can join the two data sets

gapminder_co2_final <-
  gapminder_co2 %>%
  mutate(region = if_else(country == 'Canada' | 
                          country == 'United States' | 
                          country == 'Mexico',
                          'north', 'south'))
# Now this is the final dataset after all the cleaning up

# Finally, we can save this as a csv

write_csv(gapminder_co2_final, 'gapminder_co2.csv')

# Lets analyze the data to answer some questions

ggplot(gapminder_co2_final,
       aes(x = gdpPercap,
           y = per_capita_emissions)) +
  geom_point() +
  labs(x = 'GDP (per capita)',
       y = 'CO2 emitted (per capita)',
       title = "There is a strong association between a nation's GDP \nand the amount of CO2 it produces") +
  geom_smooth(method = 'lm')

# This answers the first question
# Is there a strong association between a country's GDP and the amount of CO2
# it produces

# What is the north's population's association with CO2 emissions

gapminder_co2_final %>%
  group_by(region) %>%
  summarize(sumtotal = sum(total_emissions),
            sumpop = sum(pop))
# We get:
#  region sumtotal    sumpop
#<chr>     <dbl>     <dbl>
#  1 north  6656037. 447173470
#  2 south   889332. 451697714

