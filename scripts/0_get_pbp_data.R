rm(list = ls())

#importing libraries
library(nflreadr)
library(nflplotR)
library(stringr)

#set year to whatever year's data is needed
year <- 2021

#load the data
data <- load_pbp(year)

#write to an output file
write.csv(data.frame(data), stringr::str_interp("./datasets/nfl_pbp_${year}.csv", list(year = year)))