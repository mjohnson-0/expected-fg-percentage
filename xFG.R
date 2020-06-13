## Expected Field Goal Percentage
library(nflscrapR)
library(nflfastR)
library(tidyverse)
library(mgcv)
library(lme4)

seasons <- 2000:2019
pbp <- purrr::map_df(seasons, function(x) {
  readRDS(
    url(
      glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.rds")
    )
  )
})

kick <- pbp %>% 
  filter(extra_point_attempt == 1 | field_goal_attempt == 1) %>% 
  mutate(kick_attempt = ifelse(extra_point_attempt == 1 | field_goal_attempt == 1, 1, 0),
         kick_result = ifelse(extra_point_attempt == 1, extra_point_result, field_goal_result),
         points_scored = score_differential_post - score_differential,
         kick_success = ifelse(points_scored > 0, 1, 0),
         roof = ifelse(roof %in% c("dome", "closed"), 1, 0),
         wind = ifelse(is.na(wind) == T, 0, wind),
         temp = ifelse(is.na(temp) == T, 75, wind),
         rain_snow = case_when(
           str_detect(weather, "^0% Chance of Rain") == T ~ 0,
           str_detect(weather, "Zero Percent Chance of Rain") == T ~ 0,
           str_detect(weather, "No chance of rain") == T ~ 0,
           str_detect(weather, "Rain") == T ~ 1,
           str_detect(weather, "rain") == T ~ 1,
           str_detect(weather, "Snow") == T ~ 1,
           str_detect(weather, "snow") == T ~ 1,
           T ~ 0
         ),
         season = as.factor(season)) %>% 
  select(kicker_player_name, kicker_player_id, kick_attempt, kick_success, kick_distance, roof, surface, temp, wind, rain_snow, weather, season)

## Split into train & test data

set.seed(15)
kick <- kick[sample(nrow(kick)),]

train <- kick %>% head(dim(kick)[1]*.9)
test <- kick %>% tail(dim(kick)[1]*.1)

## Training different Models

### GAM

gam1 <- gam(data = train, kick_success ~ kick_distance + wind + season + rain_snow,
          family = binomial("logit"))
summary(gam1)

### LMER

lmer1 <- glmer(data = train, kick_success ~ kick_distance +  wind + season + rain_snow + (1|kicker_player_name),
            family = binomial)
arm::display(lmer1)

## Testing Models

test <- test %>% 
  mutate(kick_prob_gam = predict(gam1, ., type = "response")) %>% 
  mutate(kick_prob_lmer = predict(lmer1, ., type = "response", allow.new.levels = T))

Metrics::mae(test$kick_success, test$kick_prob_gam)
Metrics::mae(test$kick_success, test$kick_prob_lmer)


kick_group <- kick %>% 
  group_by(kick_distance) %>% 
  summarise(success = mean(kick_success), n = n()) %>% 
  ungroup() %>% 
  mutate(wind = 0,
         season = 2019, 
         rain_snow = 0) %>% 
  mutate(prob = predict(gam1, ., type = "response"))


ggplot(data = kick_group, aes(x = kick_distance)) +
  geom_line(aes(y = success, color = "Act.")) +
  geom_line(aes(y = prob, color = "Prob")) + 
  geom_smooth(aes(y = success))


kickers <- kick %>% filter(season == 2018) %>% 
  mutate(kick_prob_gam = predict(gam1, ., type = "response")) %>% 
  mutate(kick_prob_lmer = predict(lmer1, ., type = "response", allow.new.levels = T)) %>% 
  group_by(kicker_player_name) %>% 
  summarise(
    kicks = n(),
    success_rate = mean(kick_success),
    pred_rate_gam = mean(kick_prob_gam),
    pred_rate_lmer = mean(kick_prob_lmer)
  ) %>% 
  mutate(diff = success_rate - pred_rate_gam )


