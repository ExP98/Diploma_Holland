#@ Diploma_Holland
#@ Функции моделей классификации
#@ Дата: 14.04.2025
#@ Разработчик: Глушков Егор
#@ Изменения: -


# 1. Библиотеки и константы                                      ####
# source_python(here("2. Python/MLP_classificator.py"))

riasec_codes <- c("R", "I", "A", "S", "E", "C")

all_combos <- combn(riasec_codes, 3, simplify = FALSE) %>% 
  sapply(\(item) item %>% sort() %>% paste0(collapse = ""))


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
  
  return(list(X = X_train_[rep(1:nrow(X_train_), each = k),], y = class_label))
}


# для Label Powerset
get_three_letter_code <- function(Y_b) {
  Y_3label <- Y_b %>% 
    apply(1, \(x) riasec_codes[x]) %>% t() %>% 
    apply(1, \(x) x %>% sort() %>% paste0(collapse = "")) %>% 
    factor(levels = all_combos)
  return(Y_3label)
}


three_letters_to_bool_matrix <- function(three_lttr_vec) {
  bool_mtrx <- three_lttr_vec %>% 
    as.character() %>%  
    str_split("", n = 3) %>% 
    sapply(\(pr) 1:6 %in% match(pr, riasec_codes)) %>% 
    t()
  return(bool_mtrx)
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

classification_test_framework <- function(Y_test, Y_b_test,
                                          ind_clsf_func = multilabel_svm_clsf,
                                          n_retry = 1, label = "", ...) {
  metric_values <- lapply(1:n_retry, \(i) {
    # ind_pred <- ind_clsf_func(X_train, Y_b_train, X_test, ...)
    ind_pred <- ind_clsf_func(...)
    
    classif_ind_pred <- ind_pred %>% as.data.table() %>% as.matrix()
    cindex <- df_metric(classif_ind_pred, Y_test, func = calc_C_index)
    
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


## 3.1 Multiclass                   ####

multiclass_pred_by_SVM <- function(X_train_, Y_train_, X_test_, kernel = "sigmoid", k = 3) {
  .[X, y] <- make_multiclass_df(X_train_, Y_train_, k = k)
  
  # kernel: linear, polynomial, radial, sigmoid
  svm_model <- svm(X, y, kernel = kernel, probability = TRUE)
  
  pred <- predict(svm_model, X_test_, probability = TRUE) %>% 
    attr("probabilities") %>% 
    .[, paste0("HL_", 1:6)] %>% 
    as.data.table()
  return(pred)
}


multiclass_pred_by_RF <- function(X_train_, Y_train_, X_test_, ntree = 1000, k = 3) {
  .[X, y] <- make_multiclass_df(X_train_, Y_train_, k = k)
  
  rf_model <- randomForest(X, y, ntree = ntree) 
  mlt_cls_pred <- predict(rf_model, X_test_, type = "prob") %>% as.data.table()
  return(mlt_cls_pred)
}


multiclass_pred_by_Catboost <- function(X_train_, Y_train_, X_test_, k = 3) {
  .[X, y] <- make_multiclass_df(X_train_, Y_train_, k = k)
  
  train_pool <- catboost.load_pool(data = X, label = as.integer(y))
  test_pool  <- catboost.load_pool(data = X_test_ %>% as.data.frame())
  
  model <- catboost.train(
    learn_pool = train_pool,
    params = list(loss_function = 'MultiClass', iterations = 500, logging_level = "Silent", allow_writing_files = FALSE) 
  )
  
  mlt_cls_pred <- catboost.predict(model, test_pool, prediction_type = "Probability") %>% as.data.table()
  return(mlt_cls_pred)
}


multiclass_pred_by_Logregr <- function(X_train_, Y_train_, X_test_, k = 3, alpha = 1) {
  .[X, y] <- make_multiclass_df(X_train_, Y_train_, k = k)
  
  model <- glmnet(X, y, family = "multinomial", alpha = alpha)
  mlt_cls_pred <- predict(model, X_test_, s = last(model$lambda), type = "response")[,,1]
  return(as.data.table(mlt_cls_pred))
}


multiclass_pred_by_NB <- function(X_train_, Y_train_, X_test_, k = 3) {
  .[X, y] <- make_multiclass_df(X_train_, Y_train_, k = k)
  nb_model <- naiveBayes(X, y)
  pred <- predict(nb_model, X_test_, type = "raw") %>% as.data.table()
  return(pred)
}


multiclass_pred_by_kNN <- function(X_train_, Y_train_, X_test_, k_neighb = 25) {
  .[X, y] <- make_multiclass_df(X_train_, Y_train_, k = 3)
  model <- knn3(X, y, k = k_neighb)
  pred <- predict(model, X_test_) %>% as.data.table()
  return(pred)
}


multiclass_pred_by_ExtraTree <- function(X_train_, Y_train_, X_test_, ntree = 1000) {
  .[X, y] <- make_multiclass_df(X_train_, Y_train_, k = 3)
  et_model <- ranger(formula = y ~ .,  data = data.frame(X, y = y), num.trees = ntree,
                     splitrule = "extratrees", probability = TRUE)
  pred <- predict(et_model, X_test_) %>% as.data.table()
  return(pred)
}


multiclass_pred_by_XGBoost <- function(X_train_, Y_train_, X_test_, nrounds = 500) {
  .[X, y] <- make_multiclass_df(X_train_, Y_train_, k = 3)
  dtrain <- xgboost::xgb.DMatrix(data = X, label = as.numeric(y) - 1)
  model <- xgboost::xgb.train(
    # softprob, если нужно получить вероятности (а не только финальный класс)
    params = list(objective = "multi:softprob", num_class = 6),
    data = dtrain,
    nrounds = nrounds,
    verbose = 0
  )
  pred <- predict(model, xgboost::xgb.DMatrix(X_test_)) %>% 
    matrix(ncol = 6, byrow = TRUE) %>% 
    as.data.table()
  return(pred)
}


multiclass_pred_by_LightGBM <- function(X_train_, Y_train_, X_test_, nrounds = 100) {
  .[X, y] <- make_multiclass_df(X_train_, Y_train_, k = 3)
  dtrain <- lightgbm::lgb.Dataset(data = X, label = as.integer(y) - 1)
  
  model <- lightgbm::lgb.train(
    params = list(objective = "multiclass", num_class = 6, metric = "multi_logloss", verbose = -1),
    data = dtrain,
    nrounds = nrounds
  )
  
  prob_matrix <- predict(model, X_test_) %>% matrix(ncol = 6, byrow = TRUE)
  return(as.data.table(prob_matrix))
}


# mlp_pred <- py_eval("multiclass_pred_by_MLP(r.X, r.y_num, r.X_test)")
multiclass_pred_by_MLP <- function(X_train_, Y_train_, X_test_) {
  .[X, y] <- make_multiclass_df(X_train_, Y_train_, k = 3)
  return(PY_multiclass_pred_by_MLP(X, as.integer(y) - 1, X_test_))
}


## 3.2 Multilabel                   ####
multilabel_svm_clsf <- function(X_train, Y_b_train, X_test) {
  ind_pred <- vector("list", 6)
  for (col_i in 1:6) {
    model <- svm(x = X_train, y = as.factor(Y_b_train[, col_i]), kernel = "sigmoid", probability = TRUE)
    ind_pred[[col_i]] <- predict(model, X_test, probability = TRUE) %>% 
      attr("probabilities") %>% 
      .[, "TRUE"]
  }
  return(as.data.table(ind_pred))
}


multilabel_rf_clsf <- function(X_train, Y_b_train, X_test, ntree = 1000) {
  mltlabel_pred <- vector("list", 6)
  for (col_i in 1:6) {
    model <- randomForest(x = X_train, y = as.factor(Y_b_train[, col_i]), ntree = ntree) 
    mltlabel_pred[[col_i]] <- predict(model, X_test, type = "prob") %>% .[, "TRUE"]
  }
  return(as.data.table(mltlabel_pred))
}


multilabel_catboost_clsf <- function(X_train, Y_b_train, X_test, nrounds = 500) {
  mltlabel_pred <- vector("list", 6)
  for (col_i in 1:6) {
    train_pool <- catboost.load_pool(data = X_train, label = as.integer(Y_b_train[, col_i]))
    test_pool  <- catboost.load_pool(data = X_test %>% as.data.frame())
    
    model <- catboost.train(
      learn_pool = train_pool,
      params = list(loss_function = 'CrossEntropy', iterations = nrounds, logging_level = "Silent",
                    allow_writing_files = FALSE) 
    )
    mltlabel_pred[[col_i]] <- catboost.predict(model, test_pool, prediction_type = "Probability")
  }
  return(as.data.table(mltlabel_pred))
}


# alpha - параметр регуляризации (1 = lasso, 0 = ridge)
multilabel_logit_clsf <- function(X_train, Y_b_train, X_test, alpha = 1) {
  mltlabel_pred <- vector("list", 6)
  for (col_i in 1:6) {
    model <- glmnet(X_train, as.numeric(Y_b_train[, col_i]), family = "binomial", alpha = alpha)
    mltlabel_pred[[col_i]] <- predict(model, newx = X_test, s = last(model$lambda), type = "response")
  }
  return(as.data.table(mltlabel_pred))
}


multilabel_nb_clsf <- function(X_train, Y_b_train, X_test) {
  ind_pred <- vector("list", 6)
  for (col_i in 1:6) {
    model <- naiveBayes(x = X_train, y = as.factor(Y_b_train[, col_i]))
    ind_pred[[col_i]] <- predict(model, X_test, type = "raw")[, "TRUE"]
  }
  return(as.data.table(ind_pred))
}


multilabel_knn_clsf <- function(X_train, Y_b_train, X_test, k_neighb = 5) {
  ind_pred <- vector("list", 6)
  for (col_i in 1:6) {
    model <- knn3(X_train, as.factor(Y_b_train[, col_i]), k = k_neighb)
    ind_pred[[col_i]] <- predict(model, X_test)[, "TRUE"]
  }
  return(as.data.table(ind_pred))
}


multilabel_extratree_clsf <- function(X_train, Y_b_train, X_test, ntree = 1000) {
  ind_pred <- vector("list", 6)
  for (col_i in 1:6) {
    model <- ranger(formula = y ~ .,  data = data.frame(X_train, y = as.factor(Y_b_train[, col_i])), 
                    num.trees = ntree, splitrule = "extratrees", probability = TRUE)
    ind_pred[[col_i]] <- predict(model, X_test) %>% as.data.table() %>% .[, "TRUE."]
  }
  return(as.data.table(ind_pred))
}


multilabel_xgb_clsf <- function(X_train, Y_b_train, X_test, nrounds = 500) {
  ind_pred <- vector("list", 6)
  for (col_i in 1:6) {
    dtrain <- xgboost::xgb.DMatrix(data = X_train, label = as.numeric(Y_b_train[, col_i]))
    model <- xgboost::xgb.train(
      params = list(objective = "binary:logistic"),
      data = dtrain,
      nrounds = nrounds,
      verbose = 0
    )
    ind_pred[[col_i]] <- predict(model, xgboost::xgb.DMatrix(X_test))
  }
  return(as.data.table(ind_pred))
}


multilabel_lightgbm_clsf <- function(X_train, Y_b_train, X_test, nrounds = 100) {
  ind_pred <- vector("list", 6)
  for (col_i in 1:6) {
    dtrain <- lightgbm::lgb.Dataset(data = X_train, label = as.numeric(Y_b_train[, col_i]))
    model <- lightgbm::lgb.train(
      params = list(objective = "binary", metric = "binary_logloss", verbose = -1),
      data = dtrain,
      nrounds = nrounds
    )
    ind_pred[[col_i]] <- predict(model, X_test)
  }
  return(as.data.table(ind_pred))
}


## 3.3 Label Powerset                           ####

evaluate_label_powerset <- function(model_fn, X_train, Y_b_train, X_test, Y_b_test, label = "", ...) {
  Y_pred <- model_fn(X_train, Y_b_train, X_test, ...) %>% three_letters_to_bool_matrix()
  cl_metrics_df <- calc_classification_metrics(Y_b_test, Y_pred, label)
  return(cl_metrics_df)
}


run_label_powerset_experiments <- function(experiments_df, X_train, Y_b_train, X_test, Y_b_test) {
  # experiments_df (tibble): model_fn, label, params
  evaluate_LP <- function(model_fn, label = "", ...) {
    return(evaluate_label_powerset(model_fn, X_train, Y_b_train, X_test, Y_b_test, label, ...))
  }
  
  res <- experiments_df %>%
    as.data.table() %>% 
    .[, metrics := pmap(list(model_fn, label, params), \(model_fn, label, extra_args) 
                        do.call(evaluate_LP, c(list(model_fn = model_fn, label = label), extra_args)))] %>% 
    .[, .(label, metrics, id = 1:.N)] %>% 
    .[, metrics[[1]], by = id]
  return(res)
}


lp_svm_letters <- function(X_train, Y_b_train, X_test, kernel = "sigmoid") {
  model <- svm(x = X_train, y = get_three_letter_code(Y_b_train), kernel = kernel, probability = TRUE)
  Y_pred <- predict(model, X_test, probability = TRUE)
  return(Y_pred)
}


lp_rf_letters <- function(X_train, Y_b_train, X_test, ntree = 1000) {
  model <- randomForest(x = X_train, y = get_three_letter_code(Y_b_train), ntree = ntree)
  Y_pred <- predict(model, X_test)
  return(Y_pred)
}


lp_logit_letters <- function(X_train, Y_b_train, X_test, alpha = 1) {
  # alpha: 0 -- ridge, 1 -- lasso
  model <- suppressWarnings({
    glmnet(x = X_train, y = get_three_letter_code(Y_b_train), family = "multinomial", alpha = alpha)  
  })
  Y_pred <- predict(model, newx = X_test, s = last(model$lambda), type = "class")
  return(Y_pred)
}


lp_nb_letters <- function(X_train, Y_b_train, X_test) {
  model <- naiveBayes(x = X_train, y = get_three_letter_code(Y_b_train))
  Y_pred <- predict(model, X_test)
  return(Y_pred)
}


lp_knn_letters <- function(X_train, Y_b_train, X_test, k_neighb = 25) {
  model <- knn3(x = X_train, y = get_three_letter_code(Y_b_train), k = k_neighb)
  Y_pred <- predict(model, X_test, type = "class")
  return(Y_pred)
}


lp_et_letters <- function(X_train, Y_b_train, X_test, ntree = 1000) {
  model <- ranger(formula = y ~ .,  data = data.frame(X_train, y = get_three_letter_code(Y_b_train)),
                  num.trees = ntree, splitrule = "extratrees")
  Y_pred <- predict(model, X_test)$predictions
  return(Y_pred)
}


lp_catboost_letters <- function(X_train, Y_b_train, X_test, nrounds = 500) {
  labels <- (get_three_letter_code(Y_b_train) %>% as.integer()) - 1
  train_pool <- catboost.load_pool(data = X_train, label = labels)
  test_pool  <- catboost.load_pool(data = X_test)
  
  model <- catboost.train(
    learn_pool = train_pool,
    params = list(loss_function = 'MultiClass', iterations = nrounds, logging_level = "Silent",
                  allow_writing_files = FALSE) 
  )
  
  Y_pred <- catboost.predict(model, test_pool, prediction_type = "Class") %>% `+`(1) %>% all_combos[.]
  return(Y_pred)
}


lp_xgboost_letters <- function(X_train, Y_b_train, X_test, nrounds = 200) {
  labels <- get_three_letter_code(Y_b_train) %>% as.integer() - 1
  dtrain <- xgboost::xgb.DMatrix(data = X_train, label = labels)
  dtest  <- xgboost::xgb.DMatrix(data = X_test)
  
  model <- xgboost::xgb.train(
    params = list(objective = "multi:softprob", num_class = length(all_combos), eval_metric = "mlogloss"),
    data = dtrain,
    nrounds = nrounds,
    verbose = 0
  )
  
  Y_pred <- predict(model, dtest) %>%
    matrix(ncol = length(all_combos), byrow = TRUE) %>% 
    max.col() %>% 
    all_combos[.]
  return(Y_pred)
}


lp_lightgbm_letters <- function(X_train, Y_b_train, X_test, nrounds = 500) {
  labels <- get_three_letter_code(Y_b_train) %>% as.integer() - 1
  dtrain <- lightgbm::lgb.Dataset(data = X_train, label = labels)
  model  <- lightgbm::lgb.train(
    params = list(objective = "multiclass", num_class = length(all_combos),
                  metric = "multi_logloss", verbose = -1),
    data = dtrain,
    nrounds = nrounds
  )
  
  Y_pred <- predict(model, X_test) %>% 
    matrix(ncol = length(all_combos), byrow = TRUE) %>% 
    max.col() %>% 
    all_combos[.]
  return(Y_pred)
}
