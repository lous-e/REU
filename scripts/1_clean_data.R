rm(list = ls())

#importing libraries
library(tidyverse)

options(scipen = 9999)

#set year to view
year <- 2021

#load the data
data <- read.csv(stringr::str_interp("./datasets/nfl_pbp_${year}.csv", list(year = year)), stringsAsFactors = TRUE, header = TRUE)
data <- data[, -1]

#Step-1: filter out postseason games, end of play, timeouts, overtime, ties
df1 <- data %>%
  filter(season_type == 'REG') %>%
  filter(!is.na(play_type)) %>% 
  filter(timeout == 0) %>% 
  filter(game_half != "Overtime") %>%
  filter(result != 0) %>%
  select(game_id, total_home_score, total_away_score, game_seconds_remaining, result, game_half, posteam, home_team)

#Step-2: add column for binary result outcome, (positive) score differential
df2 <- 
  df1 %>%
  mutate(
    home_result = if_else(result > 0, 1, 0),
    diff = abs(total_home_score - total_away_score)
  )

#Step-3: seconds to brackets based on game_seconds_remaining
f <- function(t, diff){
  min <- ifelse(t == 3600, 59, t%/%60)
  int <- ifelse(t >= 1800 & t < 1980, (t %/% 30) / 2, min)
  int <- ifelse(t >= 900 & t < 960, (t %/% 30) / 2, int)
  int <- ifelse(t >= 0 & t < 300 & diff <= 20, (t %/% 20) / 3, int)
  return(data.frame(minutes = min, int = int))
}

df3 <- df2 %>% bind_cols(f(df2$game_seconds_remaining, df2$diff))
times <- df3 %>% group_by(int) %>% summarise(number = n()) #hacky way to get all time brackets

#Step-4: create time grid and populate new rows with information from previous row
df4 <- df3 %>%
  group_by(game_id, int) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  group_by(game_id) %>%
  complete(int = times$int) %>%
  fill(total_home_score, total_away_score, result, game_half, posteam, home_team, home_result, diff, .direction = 'up') %>%
  arrange(game_id, -int) %>%
  ungroup()

#Step-5: transform into score differential and select fields
df5 <- df4 %>%
  mutate(home_leading = if_else(total_home_score > total_away_score, 1, if_else(diff == 0, sample(0:1, n(), replace = TRUE), 0))) %>%
  mutate(
    leading_score = if_else(home_leading == 1, total_home_score, total_away_score),
    leading_result = if_else(home_leading == 1, home_result, 1 - home_result),
    leading_pos = if_else(home_leading == 1, if_else(posteam == home_team, 1, 0), if_else(posteam == home_team, 0, 1))
  ) %>%
  select(game_id, leading_result, int, diff, leading_score, home_leading, leading_pos)

sig <- function(x) 1/(1 + exp(-x/7))

#Step-6: apply transformations
df6 <- df5 %>% 
  filter(int != 0) %>%
  mutate(
    logInt = log(int),
    sigDiff = sig(diff),
    sqrtDiff = sqrt(diff),
  )

#write to output file
write.csv(data.frame(df6), stringr::str_interp("./cleaned/nfl_${year}.csv", list(year = year)))