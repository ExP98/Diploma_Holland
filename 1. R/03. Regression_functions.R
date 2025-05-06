#@ Diploma_Holland
#@ Функции моделей регрессии
#@ Дата: 14.03.2025
#@ Разработчик: Глушков Егор
#@ Изменения: март-апрель 2025


# 1. Библиотеки                                                  ####
# library(Rcpp)
# sourceCpp(paste0(here::here(), "/1. R/Rcpp_functions.cpp")) # rcpp_rmse


# 2. Функции                                                     ####
# 2.1 Метрики, функции потерь, корректировка предсказания        ####

# Есть аналогичная rcpp-реализация (rcpp_rmse), но она такая же по скорости
my_rmse     <- function(y1, y2) sqrt(sum((y1 - y2)^2) / length(y1))
cosine_sim  <- function(y1, y2) (sum(y1 * y2)) / (sqrt(sum(y1 ^ 2)) * sqrt(sum(y2 ^ 2)))
cosine_dist <- function(y1, y2) sqrt(2 * (1 - cosine_sim(y1, y2)))


get_max_indx <- \(row) row %>% order(decreasing = TRUE) %>% .[1:3]

calc_C_index_threes <- function(r1, r2) {
  calc_pair_match <- function(x, y) {
    if (x == y) return(3)
    if (x == ((y - 2) %% 6 + 1) | x == (y %% 6 + 1)) return(2)
    if (x == ((y - 3) %% 6 + 1) | x == ((y + 1) %% 6 + 1)) return(1)
    if (x == ((y + 2) %% 6 + 1)) return(0)
  }
  
  C_index <- calc_pair_match(r1[1], r2[1]) * 3 +
    calc_pair_match(r1[2], r2[2]) * 2 +
    calc_pair_match(r1[3], r2[3]) * 1
  
  return(C_index)
}


## C-индекс: C = 3(X1, Y1) + 2(X2, Y2) + 1(X3, Y3), где
##   (Xi, Yi) = 3 (Xi == Yi); 2 (Xi, Yi -- соседи); 1 (Xi, Yi -- через 1 позицию); 0 (противоположные)
calc_C_index <- function(row1, row2) {
  r1 <- row1 %>% get_max_indx()
  r2 <- row2 %>% get_max_indx()
  return(calc_C_index_threes(r1, r2))
}

# чтобы минимильное значение было лучшим (ибо чем больше С-индекс, тем предсказание точнее)
c_index_dist <- \(row1, row2) 18 - calc_C_index(row1, row2)


# sapply(1:nrow(pred_test), \(i) smart_integer_round(pred_test[i, ])) %>% colSums()
smart_integer_round <- function(six_vals) {
  modif <- six_vals * 42 / sum(six_vals) + 10*.Machine$double.eps
  resid <- 42 - sum(round(modif))
  
  ind_to_change <- order(abs(modif - round(modif)), decreasing = TRUE)[1:abs(resid)]
  modif[ind_to_change] <- modif[ind_to_change] + sign(resid) * 1
  
  int_rnd_values <- round(modif)
  if (sum(int_rnd_values) != 42) warning(str_glue("Sum of {toString(int_rnd_values)} != 42. Input: {toString(six_vals)}. \n"))
  return(int_rnd_values)
}


prediction_correction <- function(preds) {
  correct_preds <- preds %>%
    as.data.table() %>% 
    apply(., 1, smart_integer_round) %>% 
    t()
  return(correct_preds)
}


df_metric <- function(pred_df, Y_test_, func = my_rmse) {
  return(sapply(1:nrow(Y_test_), \(i) func(pred_df[i, ], Y_test_[i, ])) %>% mean())
}


show_custom_metrics <- function(my_pred, case_name, Y_test_ = Y_test, need_to_correct = FALSE) {
  if (need_to_correct) my_pred <- my_pred %>% prediction_correction()
  
  aRMSE_ <- df_metric(my_pred, Y_test_, my_rmse)
  # aCosDist_ <- df_metric(my_pred, Y_test_, cosine_dist)
  aCindex_ <- df_metric(my_pred, Y_test_, calc_C_index)
  # msg <- str_glue("{case_name}. aRMSE: {round(aRMSE_, 3)}; aCosDist: {round(aCosDist_, 3)}; aCindex: {round(aCindex_, 3)}")
  msg <- str_glue("{case_name}. aRMSE: {round(aRMSE_, 3)}; aCindex: {round(aCindex_, 3)}")
  return(msg)
}


# Распределение значений метрики (аналогично df_metric, но в без усреднения)
distr_metric <- function(pred_df, Y_test_, func = calc_C_index) {
  values <- sapply(1:nrow(Y_test_), \(i) func(pred_df[i, ], Y_test_[i, ]))
  p <- plot_cindex_hist(values)
  return(p)
}


plot_cindex_hist <- function(values) {
  values <- as.numeric(na.omit(values))
  mean_val   <- mean(values)
  median_val <- median(values)
  
  dens <- density(values, from = 0, to = 18)
  dens_counts <- dens$y * 100
  
  p <- plot_ly() %>%
    add_histogram(x = values, xbins = list(start = 0, end = 18, size = 1), histnorm = "percent",
                  marker = list(color = "lightsteelblue", line  = list(color = "white", width = 1)),
                  showlegend = FALSE) %>%
    add_lines(x = dens$x, y = dens_counts, mode = "lines", line = list(color = "slategray", width = 2), 
              showlegend = FALSE) %>%
    layout(title = "Распределение C-индекса", 
          xaxis = list(title = "Значения C-индекса", range = c(0, 18), 
                       zeroline = TRUE, showline = TRUE, linecolor = "black",
                       zerolinecolor = "black", zerolinewidth = 1),
          yaxis = list(title = "Доля ответов, %", ticksuffix = "", rangemode = "tozero", 
                       zeroline = TRUE, showline = TRUE, linecolor = "black", 
                       zerolinecolor = "black", zerolinewidth = 1),
          bargap = 0.1,
          shapes = list(
            list(type = "line", x0 = mean_val,   x1 = mean_val,   y0 = 0, y1 = 1, yref = "paper", 
                 line = list(color = "red", dash = "dash")),
            list(type = "line", x0 = median_val, x1 = median_val, y0 = 0, y1 = 1, yref = "paper",
                 line = list(color = "blue", dash = "dash"))
          ),
          annotations = list(
            list(
              x         = median_val, y = 0.95, xref = "x", yref = "paper",
              text      = paste0("Медиана = ", round(median_val, 2)),
              xanchor   = "left", showarrow = FALSE,
              font      = list(family = "Times New Roman, serif", size = 12, color = "blue")
            ),
            list(
              x         = mean_val, y = 0.95, xref = "x", yref = "paper",
              text      = paste0("Среднее = ", round(mean_val, 2)),
              xanchor   = "right", showarrow = FALSE,
              font      = list(family = "Times New Roman, serif", size = 12, color = "red")
            )
          )
        )
  return(p)
}


# 2.2 Преобразования данных                               ####

pca_scaler <- function(X_df, pca_model_, k = 32) {
  res <- scale(X_df, center = pca_model_$center, scale = pca_model_$scale) %*% 
    pca_model_$rotation %>% 
    .[, 1:k]
  return(res)
}


pca_data_preparation <- function(X_train_, X_test_, k_dim = NULL, limit_ssq = 0.9) {
  pca_model <- prcomp(X_train_, center = TRUE, scale. = TRUE)
  
  if (is.null(k_dim)) {
    k_dim <- data.table(k = 1:length(pca_model$sdev),
                        ssq = pca_model$sdev^2) %>%
      .[, cumsum := cumsum(ssq / sum(ssq))] %>%
      .[, limit_b := cumsum >= limit_ssq] %>%
      .[limit_b == TRUE, min(k)]
  }
  X_train_ <- pca_scaler(X_train_, pca_model, k = k_dim)
  X_test_  <- pca_scaler(X_test_, pca_model, k = k_dim)
  return(list(X_train_, X_test_))
}


scalar_matrix_production <- function(w_vec, mat_list) {
  return((map2(w_vec, mat_list, `*`) %>% reduce(`+`)) / sum(w_vec))
}


prepare_psytest_dataset <- function(wide_data_, wide_data2_, 
                                    psytest_label = c("BF", "EY", "CT", "LN", "SC"),
                                    use_PCA_b = FALSE) {
  psytest_label <- match.arg(psytest_label)
  
  wide_data_ <- wide_data_ %>% copy() %>% 
    .[, .SD, .SDcols = patterns(paste0(psytest_label, "|HL"))] %>% 
    drop_na()
  
  wide_data2_ <- wide_data2_ %>% copy() %>% 
    .[, .SD, .SDcols = patterns(paste0(psytest_label, "|HL"))] %>% 
    drop_na()
  
  X_train_unsc <- wide_data2_ %>% .[, .SD, .SDcols = patterns(psytest_label)]
  X_test_unsc  <- wide_data_  %>% .[, .SD, .SDcols = patterns(psytest_label)]
  
  Y_train <- wide_data2_ %>% .[, .SD, .SDcols = patterns("HL_")] %>% as.matrix()
  Y_test  <- wide_data_  %>% .[, .SD, .SDcols = patterns("HL_")] %>% as.matrix()
  
  mean_train <- apply(X_train_unsc, 2, mean, na.rm = TRUE)
  sd_train <- apply(X_train_unsc, 2, sd, na.rm = TRUE)
  
  X_train <- scale(X_train_unsc, center = mean_train, scale = sd_train)
  X_test  <- scale(X_test_unsc, center = mean_train, scale = sd_train)
  
  if (use_PCA_b) {
    pca_model <- prcomp(X_train, center = TRUE, scale. = TRUE)
    opt_k <- data.table(k = 1:length(pca_model$sdev),
                        ssq = pca_model$sdev^2) %>% 
      .[, cumsum := cumsum(ssq / sum(ssq))] %>% 
      .[, limit_b := cumsum >= 0.85] %>% 
      .[limit_b == TRUE, min(k)]
    
    X_train <- pca_scaler(X_train, pca_model, opt_k)
    X_test <- pca_scaler(X_test, pca_model, opt_k)
  }
  
  n_train <- nrow(X_train)
  n_test <- nrow(X_test)
  
  print(str_glue("Train: {n_train} rows; test: {n_test} rows ({round(n_test / (n_train + n_test) * 100, 2)}%)."))
  if (nrow(X_train) == 0 | nrow(X_test) == 0) error("X_train or X_test are empty!")
  
  return(list(X_train = X_train, Y_train = Y_train, X_test = X_test, Y_test = Y_test))
}


check_k_pca_for_model <- function(k, model, X_train_, X_test_, metric_f = my_rmse) {
  X_train_pca <- pca_scaler(X_train_, pca_model, k)
  X_test_pca  <- pca_scaler(X_test_,  pca_model, k)
  
  tmp_preds <- perform_stack_MO_regression(model, X_train_pca, Y_train, X_test_pca, Y_test, 
                                           print_metric = FALSE, print_model_name = FALSE)
  
  avg_metric <- df_metric(tmp_preds, Y_test, func = metric_f)
  return(c(k = k, avg_metric = avg_metric))
}


get_k_PCA_model_table <- function(model, by = 5, X_train_ = X_train, X_test_ = X_test, 
                                  plot_b = TRUE, metric_f = my_rmse) {
  
  res_k_model <- sapply(seq(2, ncol(X_train_), by = by), 
                        check_k_pca_for_model, 
                        model = model, X_train_ = X_train_, X_test_ = X_test_, metric_f = metric_f) %>% 
    t() %>% 
    as.data.table()
  
  if (plot_b) plot_ly(res_k_model, x = ~k, y = ~avg_metric, type = 'scatter', mode = 'lines+markers') %>% show()
  return(res_k_model)
}


# 2.3 MO -- multioutput        ####
perform_MO_regression <- function(model_class, type = c("stack", "chain"), X_train_ = X_train, 
                                  Y_train_ = Y_train, X_test_ = X_test, Y_test_ = Y_test, ...) {
  type <- match.arg(type)
  method_f <- if (type == "stack") perform_stack_MO_regression else perform_chain_MO_regression
  pred <- method_f(model_class, X_train_, Y_train_, X_test_, Y_test_, print_metric = FALSE, ...)
  return(pred)
}


perform_stack_MO_regression <- function(model_class, X_train_, Y_train_, X_test_, Y_test_,
                                        print_metric = FALSE, print_model_name = TRUE, 
                                        need_to_correct = FALSE, ...) {
  if (print_model_name) message(paste0("Stack - ", model_class$classname))
  models <- vector("list", 6)
  for (col_i in 1:6) {
    models[[col_i]] <- model_class$new(X_train_, Y_train_[, col_i], X_test_, Y_test_[, col_i], ...)
  }
  
  stack_pred <- sapply(models, \(x) x$pred_test)
  if (need_to_correct == TRUE) stack_pred <- stack_pred %>% prediction_correction()
  
  if (print_metric) print(show_custom_metrics(stack_pred, paste0(model_class$classname, " stack"), Y_test_ = Y_test_))
  return(invisible(stack_pred))
}


perform_chain_MO_regression <- function(model_class, X_train_, Y_train_, X_test_, Y_test_,
                                        print_metric = FALSE, print_model_name = TRUE, 
                                        need_to_correct = FALSE, ...) {
  if (print_model_name) message(paste0("Chain - ", model_class$classname))
  models <- vector("list", 6)
  
  cbind_X_train <- X_train_
  cbind_X_test <- X_test_
  
  for (col_i in 1:6) {
    if (col_i != 1) {
      cbind_X_train <- cbind(cbind_X_train, col_i = models[[col_i-1]]$pred_train)
      cbind_X_test <- cbind(cbind_X_test, col_i = models[[col_i-1]]$pred_test)
      colnames(cbind_X_train)[ncol(cbind_X_train)] <- paste0("output_", col_i)
      colnames(cbind_X_test)[ncol(cbind_X_test)] <- paste0("output_", col_i)
    }
    models[[col_i]] <- model_class$new(cbind_X_train, Y_train_[, col_i], cbind_X_test, Y_test_[, col_i], ...)
  }
  
  chain_pred <- sapply(models, \(x) x$pred_test)
  if (need_to_correct == TRUE) chain_pred <- chain_pred %>% prediction_correction()
  
  if (print_metric) print(show_custom_metrics(chain_pred, paste0(model_class$classname, " Chained"), Y_test_ = Y_test_))
  return(invisible(chain_pred))
}


calc_regression_models <- function(regr_models_df, MO_strategy = c("stack", "chain"),
                                   X_train_, Y_train_, X_test_, Y_test_) {
  MO_strategy <- match.arg(MO_strategy)
  
  MO_res <- regr_models_df %>% 
    copy() %>% 
    .[need_MO == T, pred := pmap(list(model, params), \(mdl, par) do.call(perform_MO_regression, 
        c(list(mdl, MO_strategy, X_train_, Y_train_, X_test_, Y_test_, print_model_name = F), par)))] %>%
    .[need_MO == F, pred := pmap(list(model, params), \(mdl, par) 
        do.call(mdl, c(list(X_train_, Y_train_, X_test_, Y_test_), par)))] %>% 
    .[, rmse := map_dbl(pred, \(x) df_metric(x, Y_test_, my_rmse) %>% round(3))] %>% 
    .[map_lgl(pred, \(item) !is.null(item)), 
      C_index := map_dbl(pred, \(x) df_metric(x, Y_test_, calc_C_index) %>% round(3))] %>% 
    .[, .(name, pred, rmse, C_index)]
  
  return(MO_res)
}


extract_matrices <- function(pred_res_df) return(pred_res_df[, pred] %>% `names<-`(pred_res_df[, name]))
