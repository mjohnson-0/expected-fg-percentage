library(nflfastR)
library(tidyverse)
library(nflscrapR)

gameId <- 2019111100
nflscrapR::scrape_json_play_by_play(gameId) %>%
  select(desc, play_type, epa, home_wp) %>% head(5) %>% 
  knitr::kable(digits = 3)

fast_scraper(gameId, source = "gc") %>%
  clean_pbp() %>%
  select(desc, play_type, epa, home_wp, name) %>% head(5) %>% 
  knitr::kable(digits = 3)

games_2019 <- fast_scraper_schedules(2019) %>% filter(season_type == 'REG') %>% head(3) %>% pull(game_id)

seasons <- 2010:2019
pbp <- purrr::map_df(seasons, function(x) {
  readRDS(
    url(
      glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.rds")
    )
  )
})

roster <- readRDS(url("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/roster-data/roster.rds"))


tictoc::tic('loading all games from 2009')
games_2009 <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2009.rds')) %>% filter(season_type == 'REG')
tictoc::toc()
#> loading all games from 2009: 3.2 sec elapsed
games_2009 %>% filter(!is.na(cpoe)) %>% group_by(passer_player_name) %>%
  summarize(cpoe = mean(cpoe), Atts=n()) %>%
  filter(Atts > 200) %>%
  arrange(-cpoe) %>%
  head(5) %>% 
  knitr::kable(digits = 1)

rcpoe <- pbp %>% 
  filter(!is.na(cpoe)) %>% 
  group_by(receiver_player_name, season) %>% 
  summarise(cpoe = mean(cpoe), Atts = n(), cpoe100 = mean(cpoe)/n()*100) %>% 
  filter(Atts >= 50) %>% 
  ungroup() %>% 
  group_by(receiver_player_name) %>% 
  mutate(py_cpoe100 = lag(cpoe100)) %>% 
  ungroup()

ggplot(data = rcpoe) +
  geom_point(aes(x = Atts, y = cpoe100))



kick <- pbp %>% 
  filter(extra_point_attempt == 1 | field_goal_attempt == 1) %>% 
  mutate(kick_attempt = ifelse(extra_point_attempt == 1 | field_goal_attempt == 1, 1, 0),
         kick_result = ifelse(extra_point_attempt == 1, extra_point_result, field_goal_result),
         points_scored = score_differential_post - score_differential,
         kick_success = ifelse(points_scored > 0, 1, 0),
         roof = ifelse(roof %in% c("dome", "closed"), 1, 0),
         wind = ifelse(is.na(wind) == T, 0, wind)) %>% 
  select(kick_attempt, kick_success, kick_distance, roof, surface, temp, wind, weather)

library(mgcv)

g1 <- gam(data = kick, kick_success ~ s(kick_distance) + roof + s(wind),
    family = binomial("logit"))

kick <- kick %>% 
  mutate(kick_prob = predict(g1, ., type = "response"))

ggplot(data = kick) +
  geom_smooth(aes(x = kick_distance, y = kick_success))

kick_group <- kick %>% 
  group_by(kick_distance) %>% 
  summarise(success = mean(kick_success)) %>% 
  ungroup() %>% 
  mutate(prob = predict(g1, ., type = "response"))

ggplot(data = kick_group, aes(x = kick_distance)) +
  geom_line(aes(y = success, color = "Act.")) +
  geom_line(aes(y = prob, color = "Prob"))

Metrics::mae(kick$kick_success, kick$kick_prob)
