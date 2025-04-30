#@ Diploma_Holland
#@ Чтение необходимых библиотек и файлов
#@ Дата: 14.04.2025
#@ Разработчик: Глушков Егор
#@ Изменения: -


# 1. Библиотеки                                        ####

libs <- c(
  "tidyverse", "data.table", "here", "dotty", "caret", "plotly", "arrow",
  "R6", "xgboost", "lightgbm", "randomForest", "catboost", "e1071", "FNN",
  "glmnet", "pls", "stacking", "rpart", "extraTrees", "reticulate"
  # , "Rcpp"
)

uninstall_pkgs <- libs[!(libs %in% installed.packages()[, "Package"])]
if (length(uninstall_pkgs)) install.packages(uninstall_pkgs)
# devtools::install_github("cran/extraTrees")

suppressMessages(lapply(libs, require, character.only = TRUE))
library(MASS, include.only = "stepAIC")



# 2. Файлы                                             ####
source(paste0(here::here(), "/1. R/02. Data_preparation.R"))
source(paste0(here::here(), "/1. R/03. Regression_functions.R"))
source(paste0(here::here(), "/1. R/04. Regressor_R6classes.R"))
source(paste0(here::here(), "/1. R/10. Classification_functions.R"))
# sourceCpp(paste0(here::here(), "/1. R/05. Rcpp_functions.cpp")) # rcpp_rmse


# 3. Константы                                         ####
set.seed(142)
here::here()
