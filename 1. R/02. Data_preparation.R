#@ Diploma_Holland
#@ Подготовка данных
#@ Дата: 03.02.2025
#@ Разработчик: Глушков Егор
#@ Изменения: февраль 2025


# 1. Библиотеки                                        ####
# library(tidyverse)
# library(data.table)
# library(here)


# 2. Функции                                           ####
extract_psytest_result <- function(res_str) {
  res <- res_str %>%
    str_extract("\\{[^\\}]*\\}") %>%
    str_replace_all("\\{|\\}", "") %>%
    str_split_1(",") %>%
    str_split(":") %>%
    sapply(\(elem) setNames(as.list(elem[[2]]), elem[[1]])) %>%
    as.data.frame()
  return(res)
}


transform_data_to_wide <- function(data_) {
  wide_data_ <- data_ %>% 
    copy() %>% 
    .[, result := lapply(result, extract_psytest_result)] %>% 
    dcast(id ~ test_short_name, value.var = "result") %>% 
    .[, -c("golland_v2")] %>% 
    unnest(cols = -id, names_sep = " / ") %>% 
    as.data.table() %>% 
    .[, names(.) := lapply(.SD, as.integer)]
    # .[, names(.SD) := lapply(.SD, \(x) replace(x, is.na(x), 0)), .SDcols = patterns("golland")]
  return(wide_data_)
}


rename_to_short <- function(wide_data_) {
  shrt_nms_df <<- wide_data_ %>%
    copy() %>% 
    colnames() %>% 
    data.table(full_name = .) %>% 
    .[, test_name := sapply(full_name, \(x) str_split_1(x, " / ")[1])] %>% 
    .[, short_test := case_match(test_name, 
                                 "big_five" ~ "BF",
                                 "temperament" ~ "EY",
                                 "cattell_poll" ~ "CT",
                                 "golland" ~ "HL",
                                 "leonhard_poll" ~ "LN",
                                 "schwartz_poll" ~ "SC",
                                 "golland_v2" ~ "HL2")] %>% 
    .[, .(full_name, test_name, short_test, cur = 1:.N), by = short_test] %>% 
    .[, .(full_name, short_name = if_else(!is.na(short_test), str_c(short_test, "_", cur), full_name))]
  
  wide_data_ <- wide_data_ %>% rename_all(~shrt_nms_df[["short_name"]])
  return(wide_data_)
}


separate_X_y <- function(wide_data) {
  features <- wide_data %>% 
    copy() %>% 
    .[, .SD, .SDcols = !(names(wide_data) %like% "HL|HL2|id")] %>% 
    as.matrix()
  
  targets <- wide_data %>% 
    copy() %>% 
    .[, .SD, .SDcols = names(wide_data) %like% "HL_"] %>% 
    as.matrix()
  return(list(X = features, Y = targets))
}


train_test_split <- function(features, targets, train_size = 0.8) {
  split_idx <- sample(c(TRUE, FALSE), nrow(features), replace = TRUE, prob = c(train_size, 1-train_size))
  
  .[X_train_unscaled, X_test_unscaled] <- list(features[split_idx, ], features[!split_idx, ])
  .[Y_train, Y_test] <- list(targets[split_idx, ], targets[!split_idx, ])
  
  mean_train <- apply(X_train_unscaled, 2, mean, na.rm = TRUE)
  sd_train <- apply(X_train_unscaled, 2, sd, na.rm = TRUE)
  
  saveRDS(mean_train, "3. Shiny_app/model/mean_scale.rds")
  saveRDS(sd_train,   "3. Shiny_app/model/sd_scale.rds")
  saveRDS(colnames(X_train_unscaled), "3. Shiny_app/model/coln_order.rds")
  
  X_train <- scale(X_train_unscaled, center = mean_train, scale = sd_train)
  X_test  <- scale(X_test_unscaled, center = mean_train, scale = sd_train)
  
  return(list(
    X_train = X_train,
    X_test = X_test,
    Y_train = Y_train, 
    Y_test = Y_test,
    split_idx = split_idx
  ))
}


# 3. Скрипты подготовки данных                         ####
wide_data <- fread(paste0(here(), "/0. Data/AllTestsData_092024.csv")) %>% 
  transform_data_to_wide() %>% 
  rename_to_short() %>% 
  .[, names(.SD) := lapply(.SD, \(x) replace(x, is.na(x), 0)), .SDcols = patterns("HL")] %>% 
  # у первых двух субъектов сумма кодов Голланда не 42. Исправим это:
  .[id == "10749", HL_1 := 14] %>% 
  .[id == "39803", HL_6 := 1] 
  
wide_data2 <- fread(paste0(here(), "/0. Data/AllTestsData_022025_1.csv")) %>% 
  setnames(old = "group_id", new = "id") %>% 
  transform_data_to_wide() %>% 
  rename_to_short() %>% 
  .[, names(.SD) := lapply(.SD, \(x) replace(x, is.na(x), 0)), .SDcols = patterns("HL")] %>% 
  .[id == "736", HL_6 := 2] # лишь у одного сумма кодов не равна 42

untd_dt <- bind_rows(wide_data, wide_data2) %>% .[, id := .I]
# rm(wide_data, wide_data2)

.[features, targets] <- separate_X_y(wide_data)
.[X_train, X_test, Y_train, Y_test, split_idx] <- train_test_split(features, targets)
