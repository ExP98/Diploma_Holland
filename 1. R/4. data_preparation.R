#@ Diploma_Holland
#@ Подготовка данных
#@ Дата: 03.02.2025
#@ Разработчик:  Глушков Егор


# 1. Библиотеки                                        ####
# library(tidyverse)
library(data.table)
library(here)


# 2. Функции                                           ####
here::here()

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


# 3. Скрипты подготовки данных                         ####
wide_data <- fread(paste0(here(), "/0. Data/AllTestsData_092024.csv")) %>% 
  .[, result := lapply(result, extract_psytest_result)] %>% 
  dcast(id ~ test_short_name, value.var = "result") %>% 
  .[, -c("golland_v2")] %>% 
  unnest(cols = -id, names_sep = " / ") %>% 
  as.data.table() %>% 
  .[, names(.) := lapply(.SD, as.integer)] %>% 
  .[, names(.SD) := lapply(.SD, \(x) replace(x, is.na(x), 0)), .SDcols = patterns("golland")] %>% 
  # у первых двух субъектов сумма кодов Голланда не 42. Исправим это:
  .[id == "10749", `golland / X1` := 14] %>% 
  .[id == "39803", `golland / X6` := 1]
  
new_short_names <- data.table(full_name = colnames(copy(wide_data))) %>% 
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

wide_data <- wide_data %>% rename_all(~new_short_names[["short_name"]])
