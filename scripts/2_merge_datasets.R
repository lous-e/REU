rm(list = ls())

#load packages
library(sigmoid)

#load the data
data_2023 <- read.csv("./cleaned/nfl_pbp_2023.csv", header = TRUE)
data_2023 <- data_2023[, -1]
data_2022 <- read.csv("./cleaned/nfl_pbp_2022.csv", header = TRUE)
data_2022 <- data_2022[, -1]

#merge the two datasets
data <- merge(x = data_2022, y = data_2023, all = TRUE)

#apply transformations to covariates
write.csv(data.frame(data), "./cleaned/nfl_pbp_2022_2023.csv")