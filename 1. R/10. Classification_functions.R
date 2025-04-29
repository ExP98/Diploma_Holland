#@ Diploma_Holland
#@ Функции моделей классификации
#@ Дата: 14.04.2025
#@ Разработчик: Глушков Егор
#@ Изменения: -


# 1. Библиотеки                                                  ####


# 2. Функции                                                     ####
# 2.1 Общие функции                                              ####
bool_mask_row <- function(matrix_row) {
  ind <- matrix_row %>% order(decreasing = TRUE) %>% .[1:3]
  b_vec <- rep(FALSE, 6)
  b_vec[ind] <- TRUE
  return(b_vec)
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


make_multiclass_df <- function(X_train_, Y_train_, k = 3) {
  class_label <- Y_train_ %>% 
    as.data.table() %>% 
    .[, id := 1:.N] %>% 
    melt(id.vars = "id") %>% 
    .[order(id, -value), head(.SD, k), by = "id"] %>% 
    .[, variable]
  
  df <- data.frame(X_train_[rep(1:nrow(X_train_), each = k),], Class = class_label)
  return(df)
}


# 2.2 Метрики                                                    ####

# доля неправильных ответов. От 0 до 1
hamming_loss <- function(y_true, y_pred) {
  mean(y_true != y_pred)
}


# Top-3 Accuracy аналогично Subset Accuracy (Exact Match) - Доля объектов с полностью правильными метками
subset_accuracy <- function(y_true, y_pred) {
  mean(apply(y_true == y_pred, 1, all))
}


calc_class_metrics <- function(y_true, y_pred) {
  answ <- data.table(y_true = y_true, y_pred = y_pred)
  TP <- answ[y_true == TRUE & y_pred == TRUE, .N]
  FP <- answ[y_true == FALSE & y_pred == TRUE, .N]
  FN <- answ[y_true == TRUE & y_pred == FALSE, .N]
  TN <- answ[y_true == FALSE & y_pred == FALSE, .N]
  
  return(list(
    recall = TP / (TP + FN),
    precision = TP / (TP + FP),
    acc = (TP + TN) / (TP + FP + FN + TN),
    F1_score = (2 * TP) / (2 * TP + FP + FN)
  ))
}

get_confus_elems <- function(y_true, y_pred) {
  answ <- data.table(y_true = y_true, y_pred = y_pred)
  TP <- answ[y_true == TRUE & y_pred == TRUE, .N]
  FP <- answ[y_true == FALSE & y_pred == TRUE, .N]
  FN <- answ[y_true == TRUE & y_pred == FALSE, .N]
  TN <- answ[y_true == FALSE & y_pred == FALSE, .N]
  return(list(TP = TP, FP = FP, FN = FN, TN = TN))   
}

get_total_confus_dt <- function(Y_true, Y_pred) {
  conf_dt <- sapply(1:6, \(i) get_confus_elems(Y_true[, i], Y_pred[, i])) %>% 
    t() %>% 
    as.data.table() %>% 
    unnest(cols = c(TP, FP, FN, TN))
  return(conf_dt)
}

recall_    <- function(conf_row) conf_row[["TP"]] / (conf_row[["TP"]] + conf_row[["FN"]])
precision_ <- function(conf_row) conf_row[["TP"]] / (conf_row[["TP"]] + conf_row[["FP"]])
F1_score_  <- function(conf_row) {
  return((2 * conf_row[["TP"]]) / (2 * conf_row[["TP"]] + conf_row[["FP"]] + conf_row[["FN"]]))
}

micro_metric <- \(total_conf_dt, metric_f) colSums(total_conf_dt) %>% metric_f()
macro_metric <- \(total_conf_dt, metric_f) apply(total_conf_dt, 1, metric_f) %>% mean()

calc_multiclass_metrics <- function(Y_true, Y_pred) {
  total_confus_dt <- get_total_confus_dt(Y_true, Y_pred)
  return(list(
    micro_precision = micro_metric(total_confus_dt, precision_),
    micro_recall    = micro_metric(total_confus_dt, recall_),
    micro_F1_score  = micro_metric(total_confus_dt, F1_score_),
    
    macro_precision = macro_metric(total_confus_dt, precision_),
    macro_recall    = macro_metric(total_confus_dt, recall_),
    macro_F1_score  = macro_metric(total_confus_dt, F1_score_)
  ))
}

jaccard_score <- function(y_true, y_pred) {
  intersection <- rowSums(y_true * y_pred)
  union <- rowSums((y_true + y_pred) > 0)
  return(mean(intersection / union))
}


# Мета-функция
calc_classification_metrics <- function(Y_pred_, Y_b_test_, label = "") {
  res <- list(
    label = label,
    hamming_loss = hamming_loss(Y_b_test_, Y_pred_),
    top1_acc = mean(rowSums(Y_pred_ * Y_b_test_) >= 1), # top-k accuracy -- хотя бы k меток верные
    top2_acc = mean(rowSums(Y_pred_ * Y_b_test_) >= 2),
    top3_acc = mean(rowSums(Y_pred_ * Y_b_test_) >= 3),
    calc_multiclass_metrics(Y_b_test_, Y_pred_),
    jaccard = jaccard_score(Y_b_test_, Y_pred_)
  ) %>% 
    purrr::list_flatten() %>% 
    as.data.table()
  return(res)
}


# 3. Модели                                                     ####

multilabel_ind_clsf <- function(X_train, Y_b_train, X_test) {
  ind_pred <- vector("list", 6)
  for (col_i in 1:6) {
    model <- svm(x = X_train, y = as.factor(Y_b_train[, col_i]), kernel = "sigmoid", probability = TRUE)
    ind_pred[[col_i]] <- predict(model, X_test, probability = TRUE) %>% 
      attr("probabilities") %>% 
      .[, "TRUE"]
  }
  return(as.data.table(ind_pred))
}


# X_train_ + Y_train_ OR multiclass_df
pred_by_SVM <- function(X_test_, X_train_ = NULL, Y_train_ = NULL, multiclass_df = NULL,
                        kernel = "sigmoid", k = 3) {
  if (is.null(multiclass_df)) multiclass_df <- make_multiclass_df(X_train_, Y_train_, k = k)
  svm_model <- svm(
    Class ~ .,
    data        = multiclass_df,
    kernel      = kernel, # linear, polynomial, radial, sigmoid
    probability = TRUE
  )
  
  pred <- predict(svm_model, X_test_, probability = TRUE) %>% 
    attr("probabilities") %>% 
    .[, paste0("HL_", 1:6)] %>% 
    as.data.table()
  return(pred)
}


# Обертка для того, чтобы можно было использовать classification_test_framework
multiclass_clsf <- function(X_train = NULL, Y_b_train = NULL, X_test, ...) {
  return(pred_by_SVM(X_test_ = X_test, ...))
}


classification_test_framework <- function(X_train, Y_b_train, 
                                          X_test, Y_test, Y_b_test,
                                          ind_clsf_func = multilabel_ind_clsf,
                                          n_retry = 1, label = "", ...) {
  metric_values <- lapply(1:n_retry, \(i) {
    ind_pred <- ind_clsf_func(X_train, Y_b_train, X_test, ...)
    
    classif_ind_pred <- ind_pred %>% as.data.table() %>% as.matrix()
    cindex = df_metric(classif_ind_pred, Y_test, func = calc_C_index)
    
    Y_pred <- ind_pred %>% as.data.table() %>% apply(., 1, bool_mask_row) %>% t()
    classif_metrics <- calc_classification_metrics(Y_pred, Y_b_test, label)
    return(list(cindex = cindex, classif_metrics = classif_metrics))
  })
  
  cindex_med <- sapply(metric_values, \(item) item[["cindex"]]) %>% median()
  clsf_df <- lapply(metric_values, \(item) item[["classif_metrics"]]) %>% bind_rows() 
  
  clsf_df <- clsf_df %>% copy() %>% 
    .[, lapply(.SD, \(col) mean(col, na.rm = TRUE)), .SDcols = is.numeric] %>%
    .[, label := clsf_df[1, label]] %>% 
    .[, cindex := cindex_med] %>% 
    relocate(label, cindex)
  
  return(clsf_df)
}
