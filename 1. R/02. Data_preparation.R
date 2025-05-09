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
## 2.1 Основные функции обработки данных               ####

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


train_test_split <- function(features, targets, train_size = 0.8, split_idx = NULL) {
  if (is.null(split_idx)) {
    set.seed(SEED)
    split_idx <- sample(c(TRUE, FALSE), nrow(features), replace = TRUE, prob = c(train_size, 1-train_size))
  }

  .[X_train_unscaled, X_test_unscaled] <- list(features[split_idx, ], features[!split_idx, ])
  .[Y_train, Y_test] <- list(targets[split_idx, ], targets[!split_idx, ])
  
  mean_train <- apply(X_train_unscaled, 2, mean, na.rm = TRUE)
  sd_train <- apply(X_train_unscaled, 2, sd, na.rm = TRUE)
  
  # saveRDS(mean_train, "3. Shiny_app/model/mean_scale.rds")
  # saveRDS(sd_train,   "3. Shiny_app/model/sd_scale.rds")
  # saveRDS(colnames(X_train_unscaled), "3. Shiny_app/model/coln_order.rds")
  
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


## 2.2 Имена признаков                         ####

get_translation_names_dict <- function(short_names_df = shrt_nms_df) {
  psytest_names_df <- tribble(
    ~code, ~name,
    "golland",        "Тест Голланда",
    "leonhard_poll",  "Опросник Леонгарда-Шмишека",
    "temperament",    "Личностный опросник Айзенка",
    "cattell_poll",   "16-факторный опросник Кеттелла",
    "big_five",       "Пятифакторный опросник личности",
    "schwartz_poll",  "Ценностный опросник Шварца",
    "golland_v2",     "Обновленный тест Голланда"
  )
  
  leonhard_rus_factors <- tibble(
    labels = paste0("Г - ", 1:10),
    names = c("Гипертимность", "Дистимность", "Циклотимность", "Неуравновешенность", 
              "Застревание", "Эмотивность", "Экзальтированность", "Тревожность",
              "Педантичность", "Демонстративность")
  )
  
  cattell_rus_factors <- tribble(
    ~labels, ~names,
    "A",  "Открытость - Замкнутость",
    "B",  "Развитое мышление - Ограниченное мышление",
    "C",  "Эмоциональная стабильность - Эмоциональная неустойчивость",
    "E",  "Независимость - Податливость",
    "F",  "Беспечность - Озабоченность",
    "G",  "Сознательность - Беспринципность",
    "H",  "Смелость - Застенчивость",
    "I",  "Чувственность - Твердость",
    "L",  "Подозрительность - Доверчивость",
    "M",  "Мечтательность - Практичность",
    "N",  "Утонченность - Простота",
    "O",  "Склонность к чувству вины - Спокойная самоуверенность",
    "Q1", "Радикализм - Консерватизм",
    "Q2", "Самостоятельность - Зависимость от группы",
    "Q3", "Самоконтроль, сильная воля - Недостаток самоконтроля, индифферентность",
    "Q4", "Внутренняя напряженность - Внутренняя расслабленность"
  )
  
  feature_replacement <- function(str) {
    str %>% 
      str_replace_all("индивидуальный.приоритет", "ИП") %>%
      str_replace_all("нормативный.идеал", "НИ") %>%
      str_replace_all("^X(\\.){2,}|(\\.){2,}?$", "") %>%
      str_replace("(\\.){2,}", " - ") %>%
      str_replace_all("_", " ") %>% 
      str_replace_all("\\.", " - ") %>% 
      str_trim()
  }
  
  golland_enum <- function(str) {
    riasec <- c(
      "Реалистический (R)", 
      "Исследовательский (I)",
      "Артистический (A)",
      "Социальный (S)",
      "Предприимчивый (E)",
      "Традиционный (C)"
    )
    # riasec <- c("R", "I", "A", "S", "E", "C")
    return(str %>% str_replace("X", "") %>% as.numeric() %>% riasec[.])
  }
  
  replace_col_using_df <- function(data, var_name, dict_df) {
    data <- data %>% 
      merge(dict_df %>% setnames(old = colnames(.), new = c(var_name, "V2")), 
            all.x = TRUE, by = var_name) %>% 
      .[, paste0(var_name) := if_else(is.na(V2), get(var_name), V2)] %>% 
      .[, -c("V2")]
    return(data)
  }
  
  translation_names_dict <- short_names_df %>% 
    copy() %>% 
    .[full_name != "id" & !str_detect(full_name, "golland_v2")] %>% 
    separate_wider_delim(cols = "full_name", delim = " / ", names = c("test", "feature")) %>% 
    as.data.table() %>% 
    .[, feature := feature %>% feature_replacement()] %>% 
    .[test == "golland", feature := feature %>% golland_enum()] %>% 
    replace_col_using_df("test",    psytest_names_df) %>%
    replace_col_using_df("feature", leonhard_rus_factors) %>%
    replace_col_using_df("feature", cattell_rus_factors)
  
  return(translation_names_dict)
}


# 3. Скрипты подготовки данных                         ####
wide_data <- fread(paste0(here(), "/0. Data/AllTestsData_092024.csv")) %>% 
  transform_data_to_wide() %>% 
  rename_to_short() %>% 
  .[, names(.SD) := lapply(.SD, \(x) replace(x, is.na(x), 0L)), .SDcols = patterns("HL")] %>% 
  # у первых двух субъектов сумма кодов Голланда не 42. Исправим это:
  .[id == "10749", HL_1 := 14] %>% 
  .[id == "39803", HL_6 := 1] 
  
wide_data2 <- fread(paste0(here(), "/0. Data/AllTestsData_022025_1.csv")) %>% 
  setnames(old = "group_id", new = "id") %>% 
  transform_data_to_wide() %>% 
  rename_to_short() %>% 
  .[, names(.SD) := lapply(.SD, \(x) replace(x, is.na(x), 0L)), .SDcols = patterns("HL")] %>% 
  .[id == "736", HL_6 := 2] # лишь у одного сумма кодов не равна 42

untd_dt <- bind_rows(wide_data, wide_data2) %>% .[, id := .I]
# rm(wide_data, wide_data2)

.[features, targets] <- separate_X_y(wide_data)
.[X_train, X_test, Y_train, Y_test, split_idx] <- train_test_split(features, targets)
