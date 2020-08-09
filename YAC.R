library(nflfastR)
library(tidyverse)

seasons <- 2010:2019
pbp <- purrr::map_df(seasons, function(x) {
  readr::read_csv(
    glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.csv.gz")
  )
})

df <- 
  pbp %>% 
  filter(pass == 1, !is.na(yards_after_catch)) %>%
  group_by(receiver_player_name, season) %>%
  summarise(
    yoe = sum(yards_after_catch - xyac_mean_yardage)
  ) %>%
  ungroup() %>%
  group_by(receiver_player_name) %>%
  arrange(desc(season)) %>%
  mutate(
    yoe_prev = lead(yoe),
    season_prev = lead(season)
  ) %>%
  na.omit() 

summary(lm(yoe ~ yoe_prev, data = df))

df %>%
  ggplot(aes(yoe_prev, yoe)) + 
  geom_point() + 
  geom_smooth(method = 'lm') + 
  theme_minimal() + 
  labs(
    title = "YAC year over year by receiver"
  ) 

df <- 
  pbp %>% 
  filter(pass == 1, !is.na(yards_after_catch)) %>%
  group_by(passer_player_name, season) %>%
  summarise(
    yoe = sum(yards_after_catch - xyac_mean_yardage)
  ) %>%
  ungroup() %>%
  group_by(passer_player_name) %>%
  arrange(desc(season)) %>%
  mutate(
    yoe_prev = lead(yoe),
    season_prev = lead(season)
  ) %>%
  na.omit() 

summary(lm(yoe ~ yoe_prev, data = df))

df %>%
  ggplot(aes(yoe_prev, yoe)) + 
  geom_point() + 
  geom_smooth(method = 'lm') + 
  theme_minimal() + 
  labs(
    title = "YAC year over year by passer"
  )

