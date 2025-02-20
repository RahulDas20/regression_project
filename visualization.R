library(tidyverse)
library(dslabs)
data("gapminder")

gapminder |>
  filter(year == 2015 & country %in% c("Sri Lanka", "Turkey", "Poland", "Malaysia", "Russia", "Vietnam", "South Africa", "South Korea")) |>
  select(country, infant_mortality)


gapminder |>
  filter(year == 1962) |>
  ggplot(aes(fertility, life_expectancy)) +
  geom_point()


gapminder |>
  filter(year == 1962) |>
  ggplot(aes(fertility, life_expectancy, color = continent)) +
  geom_point()


## if we want to compare two plots side by side, we can achieve this by faceting the variables.
filter(gapminder, year %in% c("1962", "2012")) |>
  ggplot(aes(fertility, life_expectancy, color = continent)) +
  geom_point() +
  facet_grid(year ~ continent)


#if we want to facet by a single variable then
filter(gapminder, year %in% c("1962","2012")) |>
  ggplot(aes(fertility, life_expectancy,colour = continent)) +
  geom_point() +
  facet_grid(. ~ year)

i

countries <- c("South Korea","Germany")
gapminder |> filter(country %in% countries & !is.na(fertility)) |>
  ggplot(aes(year, fertility, colour = country)) +
  geom_line()
