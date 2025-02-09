library(Lahman)
library(tidyverse)
library(broom)

fit <- Teams |>
  filter(yearID %in% 1962:2001) |>
  mutate(BB = BB/G, HR = HR/G, R = R/G)


lm.fit1 <- lm(R ~ BB + HR, data = fit)
summary(lm.fit1)

#But our runs don't only depend on BB and HR, they also depend on singles, doubles and triples as well
teams_data <- Teams |>
  filter(yearID %in% 1962:2002) |>
  mutate(BB = BB/G,
         singles = (H - X2B -X3B -HR )/G,
         doubles = X2B/G,
         triples = X3B/G,
         HR = HR/G,
         R = R / G
) |> select(teamID, yearID, BB, singles, doubles, triples, HR, R)

fit <- teams_data |>
  filter(yearID <= 2001)


lm.fit1  <- lm(R ~ BB + singles + doubles + triples + HR, data =  fit)
summary(lm.fit1)

#Now we will predict the number of runs based on the predicted model

teams_data$R_hat <- predict(lm.fit1, newdata = teams_data)

teams_data |> filter(yearID == 2002) %>%
  ggplot(aes(R_hat, R, label = teamID)) +
  geom_point() +
  geom_text(nudge_x = 0.05, cex = 3) +
  geom_abline()


#Our model does a good job to predict runs which we can see from the predicted runs vs actual runs by different teams

#these statistics are for teams 
#but we need player specific statistics

pa_per_game <- Batting |> filter(yearID == 2002) |>
  group_by(teamID) |>
  summarise(pa_per_game = sum(AB+BB)/max(G)) |>
  pull(pa_per_game) |>
  mean()

players <- Batting |>
  filter(yearID %in% 1997:2001) |>
  group_by(playerID) |>
  mutate(PA = BB + AB)