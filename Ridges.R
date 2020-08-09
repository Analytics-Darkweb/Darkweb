
library(tidyverse)
library(data.table)
library(ggridges)

pbp <- fread("Darkweb/pbp_2010-2019.csv")

df <- 
  pbp %>%
  select(game_id, temp, season, wind) %>%
  unique() %>%
  na.omit()


ggplot(df, aes(x = temp, y = season, group = season, fill = stat(x))) + 
  geom_density_ridges_gradient(rel_min_height = 0.01, scale = .9, quantile_lines = TRUE, quantiles = 2) + 
  scale_fill_viridis_c(name = "Temp. (F)", option = "C") + 
  theme_minimal() + 
  theme(legend.position="bottom") + 
  scale_y_continuous(breaks = seq(2010, 2019, 1)) + 
  labs(
    title = "NFL Outdoor game temps, 2010-2019",
    subtitle = "Analytics Darkweb",
    caption = "Data from @nflfastR",
    x = "Temperature (F)",
    y = "Season"
  )

ggsave("Darkweb/Ridges.png", dpi = 1000)

pbp %>%
  filter(pass == 1, !is.na(wind), !is.na(epa)) %>%
  group_by(wind) %>%
  count() %>%
  arrange(desc(wind))

pbp %>%
  filter(pass == 1, !is.na(wind), !is.na(epa), wind <= 25) %>%
  ggplot(aes(x = epa, y = wind, group = wind)) + 
  geom_density_ridges_gradient(rel_min_height = 0.01, scale = .9, quantile_lines = TRUE, quantiles = 4) + 
  theme_minimal() +
  labs(
    title = "Passing EPA does not appear to decrease as windspeed increases.",
    subtitle = "Analytics Darkweb",
    caption = "Data from @nflfastR",
    y = "Windspeed [MPH]",
    x = "Expected Points Added"
  )
