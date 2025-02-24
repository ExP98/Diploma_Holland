library(shiny)

create_test_ui <- function(test_num, factors_count, test_name) {
  factors_per_row <- 6
  rows <- split(1:factors_count, ceiling(seq_along(1:factors_count) / factors_per_row))
  
  fluidRow(column(
    12,
    div(
      style = "border: 1px solid #ddd; padding: 10px; margin-bottom: 20px;",
      fluidRow(column(
        12, checkboxInput(paste0("test", test_num), strong(test_name), width = '100%')
      )),
      conditionalPanel(
        condition = paste0("input.test", test_num),
        lapply(rows, function(row) {
          fluidRow(lapply(row, function(i) {
            column(2, div(
              style = "margin-bottom: 15px;",
              numericInput(
                inputId = paste0("t", test_num, "_f", i),
                label = paste("Фактор", i),
                value = 15,
                min = 15,
                max = 75,
                step = 1,
                width = '100%'
              )
            ))
          }))
        })
      )
    )
  ))
}



ui <- fluidPage(titlePanel("Прогнозирование психологических тестов"),
                fluidRow(
                  column(
                    12,
                    create_test_ui(1, 5, "Тест 1 (5 факторов)"),
                    create_test_ui(2, 10, "Тест 2 (10 факторов)"),
                    create_test_ui(3, 4, "Тест 3 (4 фактора)"),
                    
                    fluidRow(column(
                      12,
                      actionButton("calc", "Подсчитать", class = "btn-primary btn-block")
                    )),
                    
                    hr(),
                    h3("Результаты прогноза:"),
                    verbatimTextOutput("res")
                  )
                ))



server <- function(input, output) {
  observeEvent(input$calc, {
    errors <- list()
    
    check_test <- function(test_num, factors_count) {
      if (input[[paste0("test", test_num)]]) {
        values <- sapply(1:factors_count, function(i)
          input[[paste0("t", test_num, "_f", i)]])
        if (any(is.na(values)) || any(values < 15 | values > 75)) {
          errors <<- c(errors,
                       paste("Некорректные значения в Тесте", test_num))
        }
      }
    }
    
    check_test(1, 5)
    check_test(2, 10)
    check_test(3, 4)
    
    if (length(errors)) {
      showNotification(paste(errors, collapse = "\n"), type = "error")
      return()
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
      cat("Прогнозируемые значения:\n")
      cat(paste0("Фактор ", 1:6, ": ", res, collapse = "\n"))
      cat("\n\nСумма: ", sum(res), "\n")
      cat(sapply(1:5, \(i) input[[paste0("t", 1, "_f", i)]]))
    })
  })
}


shinyApp(ui, server)