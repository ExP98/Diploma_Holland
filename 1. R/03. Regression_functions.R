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
    layout(title = "Распределение C-индекса предсказаний PSO-ансамбля", 
          xaxis = list(title = "Значения C-индекса предсказаний", range = c(0, 18), 
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


prepare_psytest_data <- function(wide_data_, wide_data2_, 
                                 psytest_label = c("BF", "CT", "EY", "LN", "SC")) {
  psytest_label <- match.arg(psytest_label)
  
  .[psytest_fts, psytest_tgs] <- bind_rows(wide_data_, wide_data2_) %>% copy() %>% 
    .[, .SD, .SDcols = patterns(paste0(psytest_label, "|HL"))] %>%
    separate_X_y()
  
  wd_split_idx <- c(rep(FALSE, nrow(wide_data_)), rep(TRUE, nrow(wide_data2_)))
  .[X_tr, X_ts, Y_tr, Y_ts] <- train_test_split(psytest_fts, psytest_tgs, split_idx = wd_split_idx)
  
  X_tr <- X_tr %>% na.omit()
  Y_tr <- Y_tr[-attr(X_tr, "na.action"),]
  
  n_train <- nrow(X_tr)
  n_test  <- nrow(X_ts)
  
  print(str_glue("Train: {n_train} rows; test: {n_test} rows ({round(n_test / (n_train + n_test) * 100, 2)}%)."))
  return(list(X_tr, X_ts, Y_tr, Y_ts))
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
stack_MO_regr <- function(model_class, X_train_, Y_train_, X_test_, Y_test_, ...) {
  models <- vector("list", 6)
  for (col_i in 1:6) {
    models[[col_i]] <- model_class$new(X_train_, Y_train_[, col_i], X_test_, Y_test_[, col_i], ...)
  }
  return(models)
}


chain_MO_regr <- function(model_class, X_train_, Y_train_, X_test_, Y_test_, ...) {
  models <- vector("list", 6)
  cbind_X_train <- X_train_
  cbind_X_test  <- X_test_
  for (col_i in 1:6) {
    if (col_i != 1) {
      cbind_X_train <- cbind(cbind_X_train, col_i = models[[col_i-1]]$pred_train)
      cbind_X_test  <- cbind(cbind_X_test, col_i = models[[col_i-1]]$pred_test)
      colnames(cbind_X_train)[ncol(cbind_X_train)] <- paste0("output_", col_i)
      colnames(cbind_X_test)[ncol(cbind_X_test)]   <- paste0("output_", col_i)
    }
    models[[col_i]] <- model_class$new(cbind_X_train, Y_train_[, col_i], cbind_X_test, Y_test_[, col_i], ...)
  }
  return(models)
}


no_MO_regr <- function(model_class, X_train_, Y_train_, X_test_, Y_test_, ...) {
  model <- model_class$new(X_train_, Y_train_, X_test_, Y_test_, ...)
  return(list(model))
}


perform_MO_regression <- function(model_class, MO_type = c("stack", "chain", "none"), 
                                  X_train_, Y_train_, X_test_, Y_test_, 
                                  print_metric = FALSE, print_model_name = TRUE, ...) {
  MO_type <- match.arg(MO_type)
  method_f <- if (MO_type == "stack") stack_MO_regr else if (MO_type == "chain") chain_MO_regr else no_MO_regr
  if (print_model_name) message(paste0(MO_type, " - ", model_class$classname))
  
  models <- method_f(model_class, X_train_, Y_train_, X_test_, Y_test_, ...)
  pred <- lapply(models, \(x) x$pred_test) %>% do.call(cbind, .)
  
  if (print_metric) print(show_custom_metrics(pred, paste(model_class$classname, MO_type), Y_test_))
  return(invisible(pred))
}


# Для обратной совместимости
perform_stack_MO_regression <- function(model_class, X_train_, Y_train_, X_test_, Y_test_, 
                                        print_metric = FALSE, print_model_name = TRUE, ...) {
  stack_pred <- perform_MO_regression(model_class, "stack", X_train_, Y_train_, X_test_, Y_test_,
                                      print_metric, print_model_name, ...)
  return(invisible(stack_pred))
}


perform_chain_MO_regression <- function(model_class, X_train_, Y_train_, X_test_, Y_test_, 
                                        print_metric = FALSE, print_model_name = TRUE, ...) {
  chain_pred <- perform_MO_regression(model_class, "chain", X_train_, Y_train_, X_test_, Y_test_,
                                      print_metric, print_model_name, ...)
  return(invisible(chain_pred))
}


calc_regression_models <- function(regr_models_df, X_train_, Y_train_, X_test_, Y_test_) {
  MO_res <- regr_models_df %>% 
    copy() %>% 
    .[, pred := pmap(list(model, params, MO_type), \(mdl, par, mo_type) do.call(perform_MO_regression, 
        c(list(mdl, mo_type, X_train_, Y_train_, X_test_, Y_test_, print_model_name = F), par)))] %>% 
    .[, rmse := map_dbl(pred, \(x) df_metric(x, Y_test_, my_rmse) %>% round(3))] %>% 
    .[map_lgl(pred, \(item) !is.null(item)),
      C_index := map_dbl(pred, \(x) df_metric(x, Y_test_, calc_C_index) %>% round(3))] %>% 
    .[, .(name, pred, rmse, C_index)]
  return(MO_res)
}


extract_matrices <- function(pred_res_df) return(pred_res_df[, pred] %>% `names<-`(pred_res_df[, name]))


# 2.4 Важность признаков                                    ####

get_feature_importance <- function(features_df) {
  # features_df: {6 code rows x 55 feature cols} 
  importance_df <- data.table(
    feature = colnames(features_df),
    avg_imp = features_df %>% colMeans(na.rm = TRUE)
  ) %>% 
    .[, avg_imp := abs(avg_imp)] %>% 
    .[, avg_imp := (avg_imp / sum(avg_imp))] %>% 
    .[order(-avg_imp)] %>% 
    .[, cumsum_imp := cumsum(avg_imp)] %>% 
    .[, names(.SD) := round(.SD, 3), .SDcols = c("avg_imp", "cumsum_imp")]
  return(importance_df)
}


get_model_importance <- function(model_class, X_train_ = X_train, Y_train_ = Y_train,
                                 X_test_ = X_test, Y_test_ = Y_test) {
  imp <- vector("list", 6)
  for (col_i in 1:6) {
    model <- model_class$new(X_train_, Y_train_[, col_i], X_test_, Y_test_[, col_i])
    imp[[col_i]] <- model$calc_importance()
  }
  imp_df <- bind_rows(imp)
  imp_df[is.na(imp_df)] <- 0
  return(imp_df)
}


glm_importance <- function(X_train_, Y_train_, alpha = 1) {
  glm_model <- cv.glmnet(X_train_, Y_train_, family = "mgaussian", alpha = alpha)
  cf <- coef(glm_model, s = "lambda.min")
  imp_df <- cf %>% 
    sapply(\(x) x %>% unlist() %>% as.matrix()) %>%
    t() %>% rbind() %>% 
    as.data.table() %>% 
    setnames(new = pluck(cf, 1) %>% rownames()) %>% 
    .[, -"(Intercept)"]
  return(imp_df)
}


# 2.5 Методы восстановления и обучения на неполных данных   ####

# не подходит для восстановления новых данных: обрабатывает только разом исторические данные
mice_imputation <- function(df_with_na) {
  df <- df_with_na %>% copy() %>% as.data.table()
  to_imp <- setdiff(colnames(df), c("id", paste0("HL_", 1:6)))
  imp <- mice::mice(df[, ..to_imp], m = 1, method  = "pmm", maxit = 5, printFlag = FALSE)
  df[, (to_imp) := mice::complete(imp, 1)]
  return(df)
}


train_matrix_completion <- function(DT, rank.max = 20) {
  aux_feats <- setdiff(colnames(DT), c("id", paste0("HL_", 1:6)))
  M         <- as.matrix(DT)[, aux_feats]
  fit       <- softImpute::softImpute(M, rank.max = rank.max, lambda = 0, type = "als")
  return(list(fit = fit, aux = aux_feats))
}


transform_matrix_completion <- function(fit_obj, DT_new) {
  # DT_new <- DT_new %>%
  #   as.matrix() %>%
  #   softImpute::complete(fit_obj$fit) %>%
  #   {DT_new[, fit_obj$aux] <- .; DT_new}
  
  DT_new    <- as.matrix(DT_new)
  aux_feats <- fit_obj$aux
  M_imp     <- softImpute::complete(DT_new[, aux_feats, drop = FALSE], fit_obj$fit)
  DT_new[, aux_feats] <- M_imp
  return(DT_new)
}


# Добавление масок и нулей вместо пропусков
prepare_mask_data <- function(DT, y_cols = paste0("HL_", 1:6)) {
  df <- DT %>% as.data.table() %>% copy()
  x_cols <- setdiff(colnames(df), c("id", y_cols))
  
  for (x in x_cols) {
    mcol <- paste0(x, "_mask")
    df <- df %>% 
      .[, (mcol) := as.integer(!is.na(get(x)))] %>% 
      .[is.na(get(x)), (x) := 0L]
  }
  
  feats <- c(x_cols, paste0(x_cols, "_mask"))
  return(as.matrix(df[, ..feats]))
}
