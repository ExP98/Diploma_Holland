#@ Diploma_Holland
#@ Веб-стенд
#@ Дата: 24.02.2025
#@ Разработчик: Глушков Егор


# 1. Библиотеки                                        ####
library(shiny)
library(tidyverse)
library(DT)


# 2. Генерация пробного датасета с ограничениями       ####
# Генерируем датафрейм с ограничениями для факторов
set.seed(123)
constraints <- data.frame(
  test = rep(1:3, times = c(5, 10, 4)),
  factor = c(1:5, 1:10, 1:4),
  min = sample(0:50, 19, replace = TRUE),
  max = sample(51:100, 19, replace = TRUE)
)

# Корректируем максимумы чтобы max >= min
constraints$max <- pmax(constraints$max, constraints$min + 1)


# 3. Вспомогательные функции                            ####

create_test_ui <- function(test_num, factors_count, test_name) {
  test_constraints <- constraints[constraints$test == test_num, ]
  
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
            current <- test_constraints[test_constraints$factor == i, ]
            column(2,
                   div(
                     style = "margin-bottom: 15px;",
                     numericInput(
                       inputId = paste0("t", test_num, "_f", i),
                       label = paste("Фактор", i),
                       value = current$min,
                       min = current$min,
                       max = current$max,
                       step = 1,
                       width = '100%'
                     ),
                     div(
                       style = "font-size: 0.8em; color: #666;",
                       paste0("Допустимо: ", current$min, "-", current$max)
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
           create_test_ui(1, 5, "Тест 1 (5 факторов)"),
           create_test_ui(2, 10, "Тест 2 (10 факторов)"),
           create_test_ui(3, 4, "Тест 3 (4 фактора)"),
           
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
  observeEvent(input$calc, {
    errors <- list()
    
    check_test <- function(test_num, factors_count) {
      if (input[[paste0("test", test_num)]]) {
        test_constraints <- constraints[constraints$test == test_num, ]
        
        for (i in 1:factors_count) {
          val <- input[[paste0("t", test_num, "_f", i)]]
          current <- test_constraints[test_constraints$factor == i, ]
          if (is.na(val) || val < current$min || val > current$max) {
            errors <<- c(
              errors,
              str_glue("Тест {test_num}, фактор {i}: значение должно быть в интервале от {current$min} до {current$max}")
            )
          }
        }
      }
    }
    
    check_test(1, 5)
    check_test(2, 10)
    check_test(3, 4)
    
    if (length(errors) > 0) {
      showNotification(paste(errors, collapse = "\n"), type = "error")
      return(NULL)
    }
    
    generate_result <- function() {
      repeat {
        res <- sample(0:14, 6, replace = TRUE)
        if (sum(res) == 42)
          return(res)
      }
    }
    
    output$res <- renderPrint({
      res <- generate_result()
      cat("Предсказанные значения:\n")
      cat(paste0("Фактор ", 1:6, ": ", res, collapse = "\n"))
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
