rm(list = ls())

#set seed for replicability
set.seed(4197)

#load packages
library(pROC)
library(DHARMa)
library(mgcViz)

#loading the dataset
data_2021 <- read.csv("./cleaned/nfl_pbp_2021.csv", header = TRUE)
data_2021 <- data_2021[, -1]

#loading the superlearner
# sl.fit_1 <- readRDS
# sl.fit_2 <- readRDS("sl_tune_3_2covars_better.rds")("sl_tune_new.rds")
# sl.fit_3 <- readRDS("sl_tune_4_3covars.rds")

#get predictions
plotROC <- function(fit){
  pred <- predict(fit, data_2021[, c("logInt", "sigDiff")], onlySL = TRUE)
    plot.roc(
    roc(response = data_2021$leading_result, predictor = pred$pred[,1]),
    add = FALSE,
    main = "ROC plot",
    print.auc = TRUE,
    xlim = c(1, 0),
    ylim = c(0, 1)
  )
}

lapply(list(sl.fit), plotROC)

## anova
## whatever other metric we use to test
gam.check(sl.fit$fitLibrary$SL.gam.te.1_All$object, rep = 1000)
gam.check(sl.fit$fitLibrary$SL.gam.te.2_All$object, rep = 1000)
gam.check(sl.fit$fitLibrary$SL.gam.te.3_All$object, rep = 1000)


simulationOutput <- simulateResiduals(fittedModel = sl.fit$fitLibrary$SL.gam.te.1_All$object, plot = T)
