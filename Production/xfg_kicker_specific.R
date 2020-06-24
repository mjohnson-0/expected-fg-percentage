## Kicker Effect xFG Model

# Load Packages
library(nflscrapR)
library(nflfastR)
library(tidyverse)
library(lme4)
library(DT)

# Load data
seasons <- 2010:2019
pbp <- purrr::map_df(seasons, function(x) {
  readRDS(
    url(
      glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.rds")
    )
  )
})

# Clean data
kick <- pbp %>% 
  filter((extra_point_attempt == 1 | field_goal_attempt == 1)) %>% 
  mutate(kick_attempt = ifelse(extra_point_attempt == 1 | field_goal_attempt == 1, 1, 0),
         kick_result = ifelse(extra_point_attempt == 1, extra_point_result, field_goal_result),
         points_scored = score_differential_post - score_differential,
         kick_success = ifelse(points_scored > 0, 1, 0),
         roof = ifelse(roof %in% c("dome", "closed"), 1, 0),
         wind = ifelse(is.na(wind) == T, 0, wind),
         wind_rs = wind/max(wind),
         temp = ifelse(is.na(temp) == T, 75, temp),
         rain_snow = case_when(
           str_detect(weather, "^0% Chance of Rain") == T ~ 0,
           str_detect(weather, "Zero Percent Chance of Rain") == T ~ 0,
           str_detect(weather, "No chance of rain") == T ~ 0,
           str_detect(weather, "Rain") == T ~ 1,
           str_detect(weather, "rain") == T ~ 1,
           str_detect(weather, "Snow") == T ~ 1,
           str_detect(weather, "snow") == T ~ 1,
           T ~ 0,
         ),
         dist_rs = (kick_distance - min(kick_distance))/(max(kick_distance) - min(kick_distance)),
         season = as.factor(season)) %>% 
  select(kicker_player_name, kicker_player_id, kick_attempt, kick_success, kick_distance, roof, surface, temp, wind, wind_rs, dist_rs, rain_snow, weather, season)

xfg_model <- glmer(data = train, 
                kick_success ~ wind + rain_snow + kick_distance + (1|kicker_player_name),
                family = binomial,
                control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

arm::display(xfg_model)

## Create xFG Table for specific kicker

xfg_table <- expand.grid(
  kicker_player_name = "M.Crosby", # Structure: FirstInitial.LastName
  kick_distance = seq(18, 65, length.out=48),
  wind = seq(0, 30, length.out=31),
  rain_snow = seq(0, 1, length.out = 2)
)

xfg_table <- xfg_table %>% 
  mutate(xFG = predict(xfg_model, ., type = "response"))

## Find xFG for current scenario

# Change variables due to conditions
xfg <- expand.grid(
  kicker_player_name = "J.Tucker", # Structure: FirstInitial.LastName
  kick_distance = 38,
  wind = 10,
  rain_snow = 0
)

xfg %>% 
  mutate(xFG = predict(xfg_model, ., type = "response"))
