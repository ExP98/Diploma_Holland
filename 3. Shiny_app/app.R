#@ Diploma_Holland
#@ Веб-стенд
#@ Дата: февраль-апрель 2025
#@ Разработчик: Глушков Егор


# 1. Библиотеки                                        ####
library(shiny)
library(readxl)
library(DT)
library(data.table)
library(shinyBS)


# 2. Датасет с ограничениями, модель       ####
# set.seed(123)
constraints <- read.xlsx2(paste0(here(), "/3. Shiny_app/psytest_constraints.xlsx"),
                          sheetName = "constraints") %>% 
  as.data.table() %>% 
  .[, names(.SD) := lapply(.SD, as.numeric), .SDcols = c("feature_num", "min", "max", "median")] %>% 
  .[test != "Тест Голланда"]

mean_scale <- readRDS(here("3. Shiny_app/model/mean_scale.rds"))
sd_scale   <- readRDS(here("3. Shiny_app/model/sd_scale.rds"))
coln_order <- readRDS(here("3. Shiny_app/model/coln_order.rds"))
cv_glm     <- readRDS(here("3. Shiny_app/model/MODEL_cv_glm.rds"))

glm_predict <- function(model, newdata) {
  glm_pred <- predict(model, newx = newdata, s = "lambda.min")[,,1] %>% 
    smart_integer_round()
  return(glm_pred)
}

riasec_codes <- c("R", "I", "A", "S", "E", "C")

code_desc <- data.table(
  riasec_codes = riasec_codes,
  desc = c(
    "R (Реалистичный).\nПредпочитает практические задачи, работу руками и с техникой. Часто выбирает профессии, связанные с физическим трудом или природой. Примеры: инженер 🛠️, механик, строитель, фермер.",
    "I (Исследовательский).\nЛюбит анализировать данные, исследовать гипотезы и решать интеллектуальные задачи. Стремится к научным открытиям и пониманию сложных систем. Примеры: учёный 🔬, программист, биолог, химик.",
    "A (Артистический).\nЦенит креативность, свободу самовыражения и нешаблонное мышление. Часто выбирает профессии, где важны эстетика и эмоциональная глубина. Примеры: дизайнер 🎨, музыкант, писатель, актер.",
    "S (Социальный).\nНаходит удовлетворение в поддержке, обучении и взаимодействии с людьми. Важны эмпатия, коммуникация и желание улучшать общество. Примеры: учитель 📚, психолог, социальный работник, врач.",
    "E (Предприимчивый).\nСтремится к лидерству, управлению и достижению амбициозных целей. Часто выбирает карьеру в бизнесе, политике или юриспруденции. Примеры: менеджер 💼, предприниматель, юрист, маркетолог.",
    "C (Конвенциональный).\nПредпочитает чёткие инструкции, структуру и работу с цифрами/документами. Ценит аккуратность и системный подход. Примеры: бухгалтер 📊, архивариус, налоговый инспектор, логист."
  )
)


# 3. Вспомогательные функции                            ####

create_test_ui <- function(test_name) {
  dt <- constraints[test == test_name][, ':='(idx = .I, col = ceiling(.I / 6))]
  nn <- nrow(dt)
  full_name <- str_glue("Тест {test_name} ({nn} {correct_word(nn)})")
  
  bsCollapsePanel(
    title = strong(full_name),
    value = paste0("panel", test_name),
    
    if (test_name == "Ценностный опросник Шварца") {
      tags$p("НИ – нормативный идеал, ИП – индивидуальный приоритет",
             style = "font-style:italic; margin:5px 0 10px;")
    },
    
    checkboxInput(paste0("test", test_name), "Включить этот тест"),
    
    lapply(sort(unique(dt$col)), function(cn) {
      sub <- dt[col == cn]
      fluidRow(
        lapply(seq_len(nrow(sub)), function(i) {
          row <- sub[i]
          column(2,
                 div(style = "margin-bottom:10px;",
                     tags$div(
                       style = "min-height:3em; display:flex; align-items:flex-end;",
                       tags$label(`for` = row$feature_short_name,
                                  paste0(row$idx, ". ", row$feature),
                                  style = "width:100%; font-weight:bold;")
                     ),
                     numericInput(
                       inputId = row$feature_short_name, label = NULL, value = row$median, 
                       min = row$min, max = row$max, step = 1, width = "100%"
                     ),
                     div(paste0("Допустимо: от ", row$min, " до ", row$max),
                         style = "font-size:0.75em; color:#666; margin-top:2px;")
                 )
          )
        })
      )
    })
  )
}


correct_word <- function(nn) {
  case_when(
    nn %% 10 %in% 2:4 & !nn %% 100 %in% 12:14 ~ "фактора",
    nn %% 10 == 1  & nn %% 100 != 11          ~ "фактор",
    TRUE                                      ~ "факторов"
  )
}


make_conclusion <- function(pred_vec) {
  # GLOBAL: code_desc, riasec_codes
  if (is.null(pred_vec) || length(pred_vec) == 0) {
    return(list("", ""))
  }
  
  res <- data.table(
    riasec_codes = riasec_codes,
    pred = pred_vec,
    conf_deg = (mclust::softmax(pred_vec) * 100) %>% as.vector() %>% round(1)
  ) %>% 
    .[, text := paste0(riasec_codes, " (", conf_deg, "%)")] %>% 
    merge(code_desc, by = "riasec_codes", all.x = TRUE) %>% 
    .[order(-pred)]
  
  pred_df <- rbind(pred_vec) %>% as.data.table() %>% rename_all(~riasec_codes)
  
  left_text <- paste0(
    pander::pandoc.table.return(pred_df, style = 'multiline'),
    "Коды Голланда:\n",
    " • Наиболее вероятные: ", res[1:3, text] %>% paste0(collapse = ", "), "\n",
    " • Менее вероятные: \t",  res[4:6, text] %>% paste0(collapse = ", "), "\n\n",
    "Обозначения:\n",
    "X (Y%), где X - код Голланда, соответствующий типу личности,\n\t    Y - степень уверенности, что данный код Голланда входит в верхнюю триаду\n"
  )
  
  right_text <- paste0(
    "\nВаши типы личности:\n\n",
    res[1:3, desc] %>% paste0(" • ", ., collapse = "\n\n"),
    "\n"
  )
  return(list(left_text, right_text))
}


# 4. Фронт                                        ####
ui <- fluidPage(
  titlePanel("Предсказание кода Голланда по результатам психометрических тестов"),
  
  tags$head(
    tags$style(HTML("
      #tests_panel .panel-default > .panel-heading,
      #tests_panel .panel-default > .panel-body {
        background-color: #E9EFFC !important;
        border-color: #ddd !important;
        border-radius: 5px;
      }
      #tests_panel .panel {
        margin-bottom: 20px;
      }
      #tests_panel .panel-body {
        padding: 10px;
      }
      .info-btn {
        position: fixed; top: 20px; right: 20px; width: 35px; height: 35px;
        border-radius: 50%; background-color: #337ab7;
        color: white; border: none; font-weight: bold;
        cursor: pointer; box-shadow: 0 2px 5px rgba(0,0,0,0.3);
        z-index: 1000;
      }
    "))
  ),
  
  fluidRow(
    column(
      12,
      do.call(bsCollapse, c(
        list(id = "tests_panel", open = character(0)),
        lapply(constraints[, unique(test)], function(tn) create_test_ui(tn))
      )), 
      
      fluidRow(column(
        4,
        offset = 4,
        actionButton("calc", "Подсчитать", class = "btn-primary btn-block",
                     style = "margin-top: 20px; margin-bottom: 20px;")
      )),
      hr(),
      h3("Результаты прогноза"),
      fluidRow(
        column(6, htmlOutput("res_left")),
        column(6, htmlOutput("res_right"))
      ),
      tags$div(style = "height: 50px;")
    )
  ),
  
  actionButton("info", label = "i", class = "info-btn")
)


# 5. Сервер                                        ####
server <- function(input, output) {
  # реактивное хранилище для результатов
  rv <- reactiveValues(results = NULL)
  
  observeEvent(input$calc, {
    err_msg <- \(test, feature, min_, max_) {
      str_glue("Тест {test}, фактор {feature}: значение должно быть от {min_} до {max_}") %>% as.character()
    }
    
    all_tests <- constraints[, unique(test)]
    used_tests <- all_tests[sapply(all_tests, \(t) input[[paste0("test", t)]])]
  
    if (length(used_tests) > 0) {
      filled_data <- constraints %>%
        copy() %>% 
        .[test %in% used_tests] %>% 
        .[, val := input[[feature_short_name]], by = .I]
      
      errors <- filled_data %>%
        .[is.na(val) | val < min | val > max] %>% 
        .[, err_msg(test, feature, min, max)]
      
      if (length(errors) > 0) {
        lapply(errors, \(e) showNotification(e, type = "error"))
        return(NULL)
      }
      
      all_answers <- filled_data %>% 
        bind_rows(anti_join(
          constraints[, .(feature_short_name, val = median)],
          filled_data,
          by = join_by(feature_short_name)
        )) %>%
        as.data.table() %>%
        .[, .(feature_short_name, val, id = 1)] %>% 
        dcast(id ~ feature_short_name, value.var = "val") %>%
        .[, paste0(setdiff(constraints[, unique(feature_short_name)], colnames(.))) := 10] %>% 
        .[, id := NULL] %>% 
        relocate(coln_order) %>% 
        as.matrix() %>% 
        scale(center = mean_scale, scale = sd_scale)
      
      rv$results <- glm_predict(cv_glm, all_answers)
    }
    
    text_res <- make_conclusion(rv$results)
    
    output$res_left  <- renderUI({
      div(style = "white-space: pre-wrap; font-family: monospace;", HTML(text_res[[1]]))
    })
    output$res_right <- renderUI({
      div(style = "white-space: pre-wrap; font-family: monospace;", HTML(text_res[[2]]))
    })
  })
  
  observeEvent(input$info, {
    showModal(modalDialog(
      title = "Информация о проекте",
      tags$div(
        style = "font-size: 16px;",
        tags$p(icon("info-circle"), " О проекте:"),
        tags$ul(
          tags$li("Название: \"Предсказание кода Голланда по результатам психометрических тестов\""),
          tags$li("Автор: Глушков Егор Александрович"),
          tags$li("Дата создания: 2025 год"),
          tags$li("Версия: 1.0")
        ),
        tags$p("Данный проект выполнен в рамках выпускной квалификационной работы магистра")
      ),
      footer = modalButton("Закрыть"),
      easyClose = TRUE,
      size = "m"
    ))
  })
}


# 6. Запуск                                        ####
shinyApp(ui = ui, server = server)
