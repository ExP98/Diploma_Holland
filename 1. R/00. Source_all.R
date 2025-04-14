#@ Diploma_Holland
#@ Чтение необходимых библиотек и файлов
#@ Дата: 14.04.2025
#@ Разработчик: Глушков Егор
#@ Изменения: -


# 1. Библиотеки                                        ####

libs <- c(
  "tidyverse", "data.table", "here", "dotty", "caret", "plotly", "arrow"
  # , "Rcpp"
)

suppressMessages(lapply(libs, require, character.only = TRUE))


# 2. Файлы                                             ####
source(paste0(here::here(), "/1. R/02. Data_preparation.R"))
source(paste0(here::here(), "/1. R/03. Regression_functions.R"))
source(paste0(here::here(), "/1. R/04. Regressor_R6classes.R"))
source(paste0(here::here(), "/1. R/10. Classification_functions.R"))
# sourceCpp(paste0(here::here(), "/1. R/05. Rcpp_functions.cpp")) # rcpp_rmse


# 3. Константы                                         ####
set.seed(142)
here::here()
