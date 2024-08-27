rm(list = ls())

#set a seed for reproducibility
set.seed(4197)

#importing libraries
library(mgcv)
library(tidyverse)
library(SuperLearner)

#load the data
data <- read.csv("./cleaned/nfl_pbp_2022_2023.csv", header = TRUE)
data <- data[,-1]

#define the outcome and covariates
outcome <- "leading_result"
game_id <- "game_id"
covars <- c("logInt", "sigDiff")

#ridge
SL.ridge <- function(...) SL.glmnet(alpha = 0, ...)

#possible tuning for earth (not included in SuperLearner)
# SL.earth.2 <- function(...) SL.earth(degree = 2, trace = 1, nprune = 5, glm=list(family=binomial),...)
# hyper_grid <- expand.grid(
#   degree = 1:3,
#   nprune = seq(2, 100, length.out = 5) %>% floor()
# )
# for (mm in 1:nrow(hyper_grid)) {
#   eval(parse(file = "",
#              text = paste ("SL.earth.", mm ,
#                            " <- function (... , degree = ", hyper_grid [mm , 1] ,
#                            " , nprune = " , hyper_grid [mm , 2] , ") {
# SL.earth (... , degree = degree, nprune = nprune ) }" , sep = "" )))
# }


#xgboost (not included here)

#gam from mgcv
SL.gam.te <- function(Y, X, newX, family, obsWeights, model = TRUE, 
                      gam.model = as.formula(paste("Y ~ te(", paste(colnames(X), collapse = ", "), ", bs = 'ts')")),
                      ...) {
  # Fit
  fit.gam <- mgcv::gam(formula = gam.model, data = data.frame(Y, X), 
                       family = family, weights = NULL, model = model,
                       select = TRUE, method = "REML", ...)
  # Predict
  pred <- predict.gam(fit.gam, newdata = newX, type = "response")
  
  # Return predictions as a matrix
  if (is.vector(pred)) pred <- matrix(pred, ncol = 1)
  
  # Prepare the output in the same format as SL.gam
  fit <- list(object = fit.gam)
  out <- list(pred = pred, fit = fit)
  # Return the output
  class(out$fit) <- c("SL.gam.te")
  return(out)
}

#tune hyperparameters for GAM:

#model with all hyperparameters
SL.gam.te.1 <- function(...) SL.gam.te(gam.model = as.formula(
  paste("Y ~ s(logInt, bs = 'cr', k = 7) + s(sigDiff, bs = 'cr', k = 7) + ti(logInt, sigDiff, bs = 'cr', k = 10)")
), ...)

SL.gam.te.2 <- function(...) SL.gam.te(gam.model = as.formula(
  paste("Y ~ s(logInt, bs = 'bs', k = 7) + s(sigDiff, bs = 'bs', k = 7) + ti(logInt, sigDiff, bs = 'bs', k = 7)")
), ...)

SL.gam.te.3 <- function(...) SL.gam.te(gam.model = as.formula(
  paste("Y ~ s(logInt, bs = 'ts', k = 7) + s(sigDiff, bs = 'ts', k = 10) + ti(logInt, sigDiff, bs = 'ts', k = 14)")
), ...)

#parallelize
# (num_cores = RhpcBLASctl::get_num_cores())
# (cluster = parallel::makeCluster(num_cores))
# parallel::clusterEvalQ(cluster, library(SuperLearner))
# parallel::clusterExport(cluster, c("SL.gam.te.1", "SL.gam.te.2", "SL.gam.te.3"))
# parallel::clusterSetRNGStream(cluster, 1)

#train SL
SL.library <- c("SL.mean", "SL.glm",
                "SL.ridge",
                "SL.gam.te.1", "SL.gam.te.2", "SL.gam.te.3")

sl.fit <- SuperLearner(
  Y = data[,outcome], 
  X = data[,covars], 
  SL.library = SL.library, 
  verbose = TRUE,
  id = data[,game_id],
  method = 'method.NNloglik',
  family = 'binomial'
)

#save to .rdata file
saveRDS(sl.fit, "sl_tune_3.rds")

#train cross-validated Super Learner
cv_sl.fit <- CV.SuperLearner(
   V = 10,
   Y = data[,outcome],
   X = data[covars],
   SL.library = SL.library,
   verbose = TRUE,
   id = data$game_id,
   method = 'method.AUC'
)   

#save to .rdata file
saveRDS(cv_sl.fit, "cv_sl_tune_8.rds")