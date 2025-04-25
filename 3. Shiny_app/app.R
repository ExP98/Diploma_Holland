#@ Diploma_Holland
#@ Веб-стенд
#@ Дата: февраль-апрель 2025
#@ Разработчик: Глушков Егор


# 1. Библиотеки                                        ####
library(shiny)
library(readxl)
library(DT)
library(shinyBS)


# 2. Датасет с ограничениями       ####
# set.seed(123)
constraints <- read.xlsx2(paste0(here(), "/3. Shiny_app/psytest_constraints.xlsx"),
                          sheetName = "constraints") %>% 
  as.data.table() %>% 
  .[, names(.SD) := lapply(.SD, as.numeric), .SDcols = c("feature_num", "min", "max")] %>% 
  .[test != "Тест Голланда"]


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
                       row$feature_short_name, NULL,
                       row$min, min = row$min, max = row$max, step = 1,
                       width = "100%"
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
      verbatimTextOutput("res")
    )
  ),
  
  actionButton("info", label = "i", class = "info-btn")
)


# 5. Сервер                                        ####
server <- function(input, output) {
  # реактивное хранилище для результатов
  rv <- reactiveValues(results_df = NULL)
  
  observeEvent(input$calc, {
    err_msg <- \(test, feature, min_, max_) {
      str_glue("Тест {test}, фактор {feature}: значение должно быть от {min_} до {max_}") %>% as.character()
    }
    
    all_tests <- constraints[, unique(test)]
    used_tests <- all_tests[sapply(all_tests, \(t) input[[paste0("test", t)]])]
    browser()
    if (length(used_tests) > 0) {
      errors <- constraints %>%
        copy() %>% 
        .[test %in% used_tests] %>% 
        .[, val := input[[feature_short_name]], by = .I] %>%
        .[is.na(val) | val < min | val > max] %>% 
        .[, err_msg(test, feature, min, max)]
      
      if (length(errors) > 0) {
        lapply(errors, \(e) showNotification(e, type = "error"))
        return(NULL)
      }
    }
    
    calculate_sum <- function() {
      results <- list()
      for (t in constraints[, unique(test)]) {
        if (input[[paste0("test", t)]]) {
          factors <- constraints[test == t]
          vals <- sapply(factors$feature_short_name, function(id) input[[id]])
          results[[as.character(t)]] <- data.frame(test = t, sum = sum(vals, na.rm = TRUE))
        }
      }
      do.call(rbind, results)
    }
    
    rv$results_df <- calculate_sum()
    output$res <- renderPrint(rv$results_df)
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
