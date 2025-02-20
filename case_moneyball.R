library(dslabs)
library(tidyverse)
library(Lahman)
library(tidyr)
library(broom)

data("Teams")

Teams_filtered <- Teams %>%
  filter(yearID %in% 1962:2001) %>%
  mutate(BB = BB / G, HR = HR / G, R = R / G)

fit <- lm(R ~ BB + HR, data = Teams_filtered)


tidy(fit, conf.int = TRUE)


#Now we are actually going to calculate the data with all the variables
teams_data <- Teams |>
  filter(yearID %in% 1962:2002) |>
  mutate(BB = BB/G,
         singles = (H - X2B - X3B - HR) / G,
         doubles = X2B / G,
         triples = X3B / G,
         HR = HR / G,
         R = R / G) |>
  select(teamID, yearID, BB, singles, doubles, triples, HR, R)

teams_filter <- teams_data %>%
  filter( yearID <= 2001)

fit1 <- lm(R ~ BB + singles + doubles + triples + HR,data = teams_filter)

tidy(fit1, conf.int = TRUE)
