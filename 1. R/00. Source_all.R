#@ Diploma_Holland
#@ Чтение необходимых библиотек и файлов
#@ Дата: 14.04.2025
#@ Разработчик: Глушков Егор
#@ Изменения: -


# 1. Библиотеки                                        ####
install_pkgs <- function(libs) {
  uninstall_pkgs <- libs[!(libs %in% installed.packages()[, "Package"])]
  if (length(uninstall_pkgs)) install.packages(uninstall_pkgs)
  suppressMessages(lapply(libs, require, character.only = TRUE))
  return(invisible(NULL))
}


libs <- c(
  "tidyverse", "data.table", "here", "dotty", "caret", "plotly", "arrow",
  "R6", "xgboost", "lightgbm", "randomForest", "catboost", "e1071", "FNN",
  "glmnet", "pls", "stacking", "rpart", "ranger", "reticulate", "purrr"
)

install_pkgs(libs)
library(MASS, include.only = "stepAIC")
library(mclust, include.only = "softmax")


# 2. Файлы                                             ####
source(paste0(here::here(), "/1. R/02. Data_preparation.R"))
source(paste0(here::here(), "/1. R/03. Regression_functions.R"))
source(paste0(here::here(), "/1. R/04. Regressor_R6classes.R"))
source(paste0(here::here(), "/1. R/10. Classification_functions.R"))
source(paste0(here::here(), "/1. R/11. Weights_selection.R"))


# 3. Константы                                         ####
SEED <- 143
set.seed(SEED)
here::here()
