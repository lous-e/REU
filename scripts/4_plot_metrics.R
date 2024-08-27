rm(list = ls())

#set a seed for reproducibility
set.seed(4197)

#importing libraries
library(tidyverse)
library(cvAUC)
library(gridExtra)

#define custom transform function
sig <- function(x) 1/(1 + exp(-x/7))

#load the data
data <- read.csv("./cleaned/nfl_pbp_2022_2023.csv", stringsAsFactors = TRUE, header = TRUE)
data <- data[, -1]

#note on commented items in data: 
# 1) cross validated super learner - to train 
# 2) represent future work with home/away information

#load the learners
sl.fit <- readRDS("sl_tune_2.rds")

# cv_sl.fit <- readRDS("cv_sl_tune_2.rds")

#get the weights for cv_sl:
# get_cv_weights <- function(cv_sl){
#   meta_weights = data.frame(coef(cv_sl))
#   means = lapply(1:ncol(meta_weights), function(i) mean(meta_weights[,i]))
#   sds = lapply(1:ncol(meta_weights), function(i) sd(meta_weights[,i]))
#   mins = lapply(1:ncol(meta_weights), function(i) min(meta_weights[,i]))
#   maxes = lapply(1:ncol(meta_weights), function(i) max(meta_weights[,i]))
#   weights <- data.frame(
#     learners <- colnames(meta_weights),
#     mean <- simplify2array(means),
#     sd <- simplify2array(sds),
#     min <- simplify2array(mins),
#     max <- simplify2array(maxes)
#   )
#   weights <- weights[order(weights$mean, decreasing = TRUE), ]
# }
# weights <- get_cv_weights(cv_sl.fit)

#PLOT 1: Weights of Individual Learners
plot_weights <- function(){
  sort(sl.fit$coef, decreasing=TRUE)
  coef_weights <- tibble(name = sl.fit$libraryNames, weight = sl.fit$coef, row.names = 1:length(sl.fit$coef)) %>%
    filter(weight != 0)
  ggplot(coef_weights, aes(x = name, y = weight)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    geom_text(aes(label = round(weight, 3)), vjust = -0.3, color= "black", size = 3.5) +
    theme_minimal()
}

#call
weights_plot <- plot_weights()
png(filename = "plots/sl_weights.png", height = 640, width = 640) #save
print(weights_plot)
dev.off()

#PLOT 2: SL fit vs best candidate algorithm
plot_fit <- function(){
  ggplot() + 
    geom_point(aes(x = sl.fit$library.predict[,which.max(sl.fit$coef)], y = sl.fit$SL.predict), shape = 1) +
    geom_abline(aes(slope = 1, intercept = 0), col = "blue") +
    labs(x = sl.fit$libraryNames[which.max(sl.fit$coef)], y = 'SL') +
    theme_minimal()
}

#call
fit_plot <- plot_fit()
png(filename = "plots/sl_best_candidate.png", height = 640, width = 640) #save
print(fit_plot)
dev.off()

#PLOT 3: CI of cv-AUCs

#Step-1: define function to get CIs for each cv-AUC
get_AUC <- function(fit, col){
  auc <- ci.cvAUC(predictions = fit$Z[,col], labels = data$leading_result, folds = fit$folds)
  tibble(
    Algorithm=fit$libraryNames[col], 
    AUC=auc$cvAUC, 
    CI.lower=auc$ci[1], 
    CI.upper=auc$ci[2]
  )
}

#Step 2: get AUCs for each of the algorithms
alg.aucs <- 
  tibble(index = 1:length(sl.fit$libraryNames)) %>%
  mutate(
    result = map(index, ~get_AUC(fit = sl.fit, col = .x))
  ) %>%
  unnest(result) %>%
  dplyr::select(-index)

#Step 3: get AUC for SuperLearner
sl.auc <- cvAUC(predictions = sl.fit$SL.predict, labels=data$leading_result)
alg.aucs <- rbind(alg.aucs, data.frame(Algorithm="SL", AUC=sl.auc$cvAUC, CI.lower=NA, CI.upper=NA))

#Step 4: get AUC for CV_SL
# cv_sl.auc <- ci.cvAUC(predictions = cv_sl.fit$SL.predict, labels=data$leading_result, folds = cv_sl.fit$folds)
# alg.aucs <- rbind(alg.aucs, data.frame(Algorithm="CV_SL", AUC=cv_sl.auc$cvAUC, CI.lower=cv_sl.auc$ci[1], CI.upper=cv_sl.auc$ci[2]))

#Step 5: plot
alg.aucs$Algorithm <- factor(alg.aucs$Algorithm, levels=alg.aucs$Algorithm[order(alg.aucs$AUC)])
png(filename = "plots/CI_CVAUC.png", height = 640, width = 640) #save
ggplot(alg.aucs) + 
  geom_errorbarh(aes(AUC, Algorithm, xmin=CI.lower, xmax=CI.upper))+
  geom_point(aes(AUC, Algorithm))
dev.off()

#PLOT 4: raw v/s predicted probabilities at a given score differentials
predict.SL.gam.te <- function(object, newdata, ...) {
  mgcv::predict.gam(object = object$object, newdata = newdata, type = "response")
}

#to plot for superLearner
plot_raw_vs_predicted <- function(lrnr){
  alg_name <- ifelse(lrnr == 0, "superlearner", sl.fit$libraryNames[lrnr])
  print(paste0('plotting raw v/s predicted for ', alg_name, sep = ""))
  int <- (data %>% group_by(int) %>% summarise() %>% pull(int))
  diff <- 0:19
  
  getPreds <- function(i){
    pred_grid <- expand.grid(int = int, diff = diff, home_leading = i) %>%
      dplyr::mutate(
        logInt = log(int),
        sigDiff = sig(diff)
      )
    predictions <- tibble(
      int = pred_grid$int,
      home_leading = i,
      diff = pred_grid$diff
    )
    
    if (lrnr == 0){
      predictions$predicted_prob <- predict(sl.fit, pred_grid[, c("logInt", "sigDiff")], onlySL = TRUE)$pred[,1]
    }
    else {
      predictions$predicted_prob <- predict(sl.fit, pred_grid[, c("logInt", "sigDiff")], onlySL = FALSE)$library.predict[,lrnr]
    }
    raw_win <- data %>%
      filter(home_leading == i) %>%
      select(logInt, int, diff, leading_result) %>%
      group_by(int, diff) %>%
      reframe(
        raw_prob = mean(leading_result)
      )
    return(list("pred" = predictions, "raw" = raw_win))
  }
  # home <- getPreds(1)
  # away <- getPreds(0)
  # 
  # preds_home <- home$pred
  # raw_home <- home$raw
  # preds_away <- away$pred
  # raw_away <- away$raw
  # 
  # predictions_away <- left_join(preds_away, raw_away, by = join_by("int", "diff"))
  # predictions_home <- left_join(preds_home, raw_home, by = join_by("int", "diff"))
  
  preds <- getPreds(0)
  preds <- left_join(preds$pred, preds$raw, by = join_by("int", "diff"))
  
  plot_preds <- function(points, i){
    # if (i == 1){
    #   to_plot <- predictions_home
    # }
    # else{
    #   to_plot <- predictions_away
    # }
    # to_plot <- to_plot %>% filter(diff == points)
    to_plot <- preds %>% filter(diff == points)
    ggplot(to_plot, aes(x = int)) + 
      scale_x_continuous(breaks = seq(0, 60, by = 20), limits = c(0, 60)) +  
      scale_y_continuous(breaks = seq(0, 1, by = 0.2), limits = c(0, 1)) + 
      geom_line(aes(y = predicted_prob), color = "black") + 
      geom_line(aes(y = raw_prob), color = "red") +
      labs(x = NULL, y = NULL) +
      ggtitle(paste("Up by", points, "points")) + 
      theme_minimal() + 
      theme(plot.title = element_text(hjust = 0.5, size = 10))
  }
  
  #generate plots for each score differential
  plots_home <- lapply(0:19, function(k) plot_preds(k, 1))
  plots_away <- lapply(0:19, function(k) plot_preds(k, 0))

  filename <- paste("plots/predicted_vs_real_away_", alg_name, ".png", sep = "")
  png(filename = filename, height = 1080, width = 1920)
  grid.arrange(grobs=plots_away, ncol=5, bottom = "Time remaining (minutes)", left = "Probability of winning", top = alg_name)
  dev.off()
  
  # plot <- grid.arrange(grobs=plots_home, ncol=5, bottom = "Time remaining (minutes)", left = "Probability of winning", top = alg_name)
  filename <- paste("plots/predicted_vs_real_home_", alg_name, ".png", sep = "")
  png(filename = filename, height = 1080, width = 1920)
  grid.arrange(grobs=plots_home, ncol=5, bottom = "Time remaining (minutes)", left = "Probability of winning", top = alg_name)
  dev.off()
  
  #on a single plot
  # print('works')
  # plot_preds_2 <- function(preds_home, preds_away, points){
  #   preds_home <- preds_home %>% filter(diff == points)
  #   preds_away <- preds_away %>% filter(diff == points)
  #   # to_plot <- preds %>% filter(diff == points)
  #   ggplot(preds_home, aes(x = int)) + 
  #     scale_x_continuous(breaks = seq(0, 60, by = 20), limits = c(0, 60)) +  
  #     scale_y_continuous(breaks = seq(0, 1, by = 0.2), limits = c(0, 1)) + 
  #     geom_line(aes(y = predicted_prob), color = "blue") + 
  #     geom_line(aes(y = raw_prob), color = "blue", alpha = 0.5) +
  #     geom_line(data = preds_away, aes(x = int, y = predicted_prob), color = "red") + 
  #     geom_line(data = preds_away, aes(x = int, y = raw_prob), color = "red", alpha = 0.5) +
  #     labs(x = NULL, y = NULL) +
  #     ggtitle(paste("Up by", points, "points")) + 
  #     theme_minimal() + 
  #     theme(plot.title = element_text(hjust = 0.5, size = 10))
  # }
  # 
  # #generate plots for each score differential
  # plots <- lapply(0:19, function(k) plot_preds_2(predictions_home, predictions_away, k))
  # 
  # filename <- paste("plots/predicted_vs_real_", alg_name, ".png", sep = "")
  # png(filename = filename, height = 1080, width = 1920)
  # grid.arrange(grobs=plots, ncol=5, bottom = "Time remaining (minutes)", left = "Probability of winning", top = alg_name)
  # dev.off()
  # 
}

lapply(0:ncol(sl.fit$Z), plot_raw_vs_predicted)

#PLOT 5: calibration plots for different learners

#function to plot for a single learner
plot_calibration <- function(i){
  alg_name <- sl.fit$libraryNames[i]
  model_predictions <- data.frame(data, data.frame(alg_preds = sl.fit$Z[,i]))
  #control N: increasing leads to finer bins
  N <- 100
  model_predictions2 <- model_predictions %>%
    mutate(group = floor(alg_preds*N)/N) %>%
    group_by(group) %>%
    mutate(
      raw_prob = mean(leading_result),
      mean_group = mean(alg_preds)
    ) %>%
    summarise(
      mean_group = mean(mean_group),
      raw_prob = mean(raw_prob)
    )
  #plot
  filename <- paste("plots/calibration_", alg_name, ".png", sep = "")
  cal_plot <- ggplot(data = model_predictions2) +
    geom_point(aes(x = mean_group, y = raw_prob)) + 
    stat_smooth(data = data.frame(model_predictions), aes(x = alg_preds, y = leading_result)) +
    geom_abline(intercept = 0) + 
    scale_x_continuous(breaks = seq(0, 1, by = 0.2), limits = c(0, 1)) +  
    scale_y_continuous(breaks = seq(0, 1, by = 0.2), limits = c(0, 1)) + 
    ggtitle(paste("Calibration plot for", alg_name)) + 
    theme(plot.title = element_text(hjust = 0.5))
  png(filename = filename)
  print(cal_plot)
  dev.off()
  return(list("preds1" = model_predictions, "preds2" = model_predictions2))
}

#call
lapply(1:ncol(sl.fit$Z), plot_calibration)

#PLOT 6: ROC curves for different learners
plot_ROC <- function(i) {
  if (i == 0){ #SuperLearner
    alg_name <- "superlearner"
    predictions <- as.numeric(cv_sl.fit$SL.predict)
  } else {
    alg_name <- sl.fit$libraryNames[i]
    predictions <- sl.fit$Z[,i]
  }
  filename <- paste("plots/roc_", alg_name, ".png", sep = "")
  png(filename = filename)
  plot.roc(
    roc(response = data$leading_result, predictor = predictions),
    add = FALSE,
    main = paste("ROC plot for ", alg_name, sep = ""),
    print.auc = TRUE,
    xlim = c(1, 0),
    ylim = c(0, 1)
  )
  dev.off()
}

#get plots
lapply(0:ncol(sl.fit$Z), plot_ROC)