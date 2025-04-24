#@ Diploma_Holland
#@ Веб-стенд
#@ Дата: февраль-апрель 2025
#@ Разработчик: Глушков Егор


# 1. Библиотеки                                        ####
library(shiny)
library(tidyverse)
library(readxl)
library(DT)


# 2. Датасет с ограничениями       ####
set.seed(123)
constraints <- read.xlsx2(paste0(here(), "/3. Shiny_app/psytest_constraints.xlsx"),
                          sheetName = "constraints") %>% 
  as.data.table() %>% 
  .[, names(.SD) := lapply(.SD, as.numeric), .SDcols = c("feature_num", "min", "max")]


# 3. Вспомогательные функции                            ####

create_test_ui <- function(test_num, test_name) {
  test_constraints <- constraints %>% filter(test == test_num)
  factors_count <- nrow(test_constraints)
  
  factors_per_row <- 6
  rows <- split(1:factors_count, ceiling(seq_along(1:factors_count) / factors_per_row))
  
  fluidRow(column(
    12,
    div(
      style = "border: 1px solid #ddd; padding: 10px; margin-bottom: 20px; background-color: #E9EFFC;
      border-radius: 5px;",
      fluidRow(column(
        12, checkboxInput(paste0("test", test_num), strong(test_name), width = '100%')
      )),
      conditionalPanel(
        condition = paste0("input.test", test_num),
        lapply(rows, function(row) {
          fluidRow(lapply(row, function(i) {
            current <- test_constraints[i, ]
            column(2,
                   div(
                     style = "margin-bottom: 10px;",
                     tags$div(
                       style = "min-height: 3em; display: flex; align-items: flex-end;",
                       tags$label(
                         `for` = current$feature_short_name,
                         paste0(i, ". ", current$feature),
                         style = "display: block; width: 100%; font-weight: bold;"
                       )
                     ),
                     numericInput(
                       inputId = current$feature_short_name,
                       label = NULL, # paste0(i, ". ", current$feature)
                       value = current$min,
                       min = current$min,
                       max = current$max,
                       step = 1,
                       width = '100%'
                     ),
                     div(
                       style = "font-size: 0.75em; color: #666; margin-top: 2px;",
                       paste0("Допустимо: от ", current$min, " до ", current$max)
                     )
                   ))
          }))
        })
      )
    )
  ))
}


# 4. Фронт                                        ####
ui <- fluidPage(
  titlePanel("Предсказание кода Голланда по результатам психометрических тестов"),
  fluidRow(
    column(12,
           lapply(unique(constraints$test), function(t) {
             test_name <- paste0("Тест ", t, " (", nrow(constraints %>% filter(test == t)), " факторов)")
             create_test_ui(t, test_name)
           }),
           
           fluidRow(
             column(4, offset = 4,
                    actionButton("calc", "Подсчитать", class = "btn-primary btn-block",
                                 style = "margin-top: 20px; margin-bottom: 20px;"))
             ), 
           hr(),
           h3("Результаты прогноза"),
           verbatimTextOutput("res")
           )
    ),
  
  tags$head(
    tags$style(HTML("
      .info-btn {
        position: fixed; top: 20px; right: 20px; width: 35px; height: 35px;
        border-radius: 50%; background-color: #337ab7;
        color: white; border: none; font-weight: bold;
        cursor: pointer; box-shadow: 0 2px 5px rgba(0,0,0,0.3);
        z-index: 1000;
      }
    "))
  ),
  actionButton("info", label = "i", class = "info-btn")
  )


# 5. Сервер                                        ####
server <- function(input, output) {
  # реактивное хранилище для результатов
  rv <- reactiveValues(results_df = NULL)
  
  observeEvent(input$calc, {
    errors <- list()
    
    tests <- unique(constraints$test)
    for (t in tests) {
      factors <- constraints %>% filter(test == t)
      if (input[[paste0("test", t)]]) {
        for (i in 1:nrow(factors)) {
          feature_short_name <- factors$feature_short_name[i]
          val <- input[[feature_short_name]]
          minv <- factors$min[i]
          maxv <- factors$max[i]
          if (is.na(val) || val < minv || val > maxv) {
            errors <- c(errors, str_glue("Тест {t}, фактор {factors$feature[i]}: значение должно быть в интервале от {minv} до {maxv}"))
          }
        }
      }
    }
    
    if (length(errors) > 0) {
      showNotification(paste(errors, collapse = "\n"), type = "error")
      return(NULL)
    }
    
    calculate_sum <- function() {
      results <- list()
      for (t in unique(constraints$test)) {
        if (input[[paste0("test", t)]]) {
          factors <- constraints %>% filter(test == t)
          vals <- sapply(factors$feature_short_name, function(id) input[[id]])
          results[[as.character(t)]] <- data.frame(test = t, sum = sum(vals, na.rm = TRUE))
        }
      }
      do.call(rbind, results)
    }
    
    rv$results_df <- calculate_sum()
    
    output$res <- renderPrint({
      print(rv$results_df)
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
