#@ Diploma_Holland
#@ Функции и классы моделей регрессии
#@ Дата: 14.03.2025
#@ Разработчик: Глушков Егор
#@ Изменения: март 2025


# 1. Библиотеки                                                  ####
library(R6)
library(xgboost)
library(lightgbm)
library(randomForest)
library(catboost)


# 2. Функции                                                     ####
# 2.1 Метрики, функции потерь, корректировка предсказания        ####

my_rmse     <- function(y1, y2) sqrt(sum((y1 - y2)^2) / length(y1))
cosine_sim  <- function(y1, y2) (sum(y1 * y2)) / (sqrt(sum(y1 ^ 2)) * sqrt(sum(y2 ^ 2)))
cosine_dist <- function(y1, y2) sqrt(2 * (1 - cosine_sim(y1, y2)))

## C-индекс: C = 3(X1, Y1) + 2(X2, Y2) + 1(X3, Y3), где
##   (Xi, Yi) = 3 (Xi == Yi); 2 (Xi, Yi -- соседи); 1 (Xi, Yi -- через 1 позицию); 0 (противоположные)
calc_C_index <- function(row1, row2) {
  get_max_indx <- \(row) row %>% order(decreasing = TRUE) %>% .[1:3]
  calc_pair_match <- function(x, y) {
    if (x == y) return(3)
    if (x == ((y - 2) %% 6 + 1) | x == (y %% 6 + 1)) return(2)
    if (x == ((y - 3) %% 6 + 1) | x == ((y + 1) %% 6 + 1)) return(1)
    if (x == ((y + 2) %% 6 + 1)) return(0)
  }
  
  r1 <- row1 %>% get_max_indx()
  r2 <- row2 %>% get_max_indx()
  
  C_index <- calc_pair_match(r1[1], r2[1]) * 3 +
    calc_pair_match(r1[2], r2[2]) * 2 +
    calc_pair_match(r1[3], r2[3]) * 1
  return(C_index)
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


show_custom_metrics <- function(my_pred, case_name, Y_test_ = Y_test) {
  aRMSE_ <- df_metric(my_pred, Y_test_, my_rmse)
  aCosDist_ <- df_metric(my_pred, Y_test_, cosine_dist)
  aCindex_ <- df_metric(my_pred, Y_test_, calc_C_index)
  msg <- str_glue("{case_name}. aRMSE: {round(aRMSE_, 3)}; aCosDist: {round(aCosDist_, 3)}; aCindex: {round(aCindex_, 3)}")
  return(msg)
}


# 2.2 Преобразования данных                               ####

pca_scaler <- function(X_df, pca_model_, k = 32) {
  res <- scale(X_df, center = pca_model_$center, scale = pca_model_$scale) %*% 
    pca_model_$rotation %>% 
    .[, 1:k]
  return(res)
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
perform_MO_regression <- function(model_class, type = c("stack", "chain")) {
  type <- match.arg(type)
  pred <- if (type == "stack") {
    perform_stack_MO_regression(model_class, X_train, Y_train, X_test, Y_test, print_metric = FALSE)
  } else {
    perform_chain_MO_regression(model_class, X_train, Y_train, X_test, Y_test, print_metric = FALSE)
  }
  return(pred)
}


perform_stack_MO_regression <- function(model_class, X_train_, Y_train_, X_test_, Y_test_,
                                        print_metric = FALSE, print_model_name = TRUE) {
  if (print_model_name) message(paste0("Stack - ", model_class$classname))
  models <- vector("list", 6)
  for (col_i in 1:6) {
    models[[col_i]] <- model_class$new(X_train_, Y_train_[, col_i], X_test_, Y_test_[, col_i])
    # print(models[[col_i]]$rmse_test)
  }
  
  stack_pred <- sapply(models, \(x) x$pred_test) %>% prediction_correction()
  if (print_metric) print(show_custom_metrics(stack_pred, paste0(model_class$classname, " stack"), Y_test_ = Y_test_))
  return(invisible(stack_pred))
}


perform_chain_MO_regression <- function(model_class, X_train_, Y_train_, X_test_, Y_test_,
                                        print_metric = FALSE, print_model_name = TRUE) {
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
    models[[col_i]] <- model_class$new(cbind_X_train, Y_train_[, col_i], cbind_X_test, Y_test_[, col_i])
    # print(models[[col_i]]$rmse_test)
  }
  
  chain_pred <- sapply(models, \(x) x$pred_test) %>% prediction_correction()
  if (print_metric) print(show_custom_metrics(chain_pred, paste0(model_class$classname, " Chained"), Y_test_ = Y_test_))
  return(invisible(chain_pred))
}


# 3. R6-Классы моделей                                           ####
# 3.1 my_template_model                                          ####
my_template_model <- R6Class(
  classname = "my_template_model",
  public = list(
    model = NULL,
    importance = NULL,
    pred_train = NULL,
    pred_test = NULL,
    rmse_test = NA,
    
    calc_importance = function() {
      self$importance <- NULL
      return(self$importance)
    },
    
    initialize = function(X_train_, y_train_, X_test_ = NULL, y_test_ = NULL, ...) {
      private$fit(X_train_, y_train_, ...)
      self$pred_train <- private$predict(X_train_)
      
      if (!is.null(X_test_)) self$pred_test <- private$predict(X_test_, is_test = TRUE)
      if (!is.null(y_test_) && !is.null(self$pred_test)) private$calc_rmse(y_test_, self$pred_test)
      
      return(invisible(self))
    }
  ),
  
  private = list(
    calc_rmse = function(y_test_, y_preds = self$pred_test) {
      self$rmse_test <- my_rmse(y_test_, self$pred_test)
      return(invisible(self))
    },
    
    fit = function(X_train_, y_train_, ...) {
      self$model <- NULL
      return(invisible(self))
    },
    
    predict = function(X_, is_test = FALSE) {
      preds <- predict(self$model, X_)
      if (is_test) self$pred_test <- preds
      return(preds)
    }
  )
)


# 3.2 my_XGBoost_model                                          ####
my_XGBoost_model <- R6Class(
  classname = "my_XGBoost_model",
  inherit = my_template_model,
  
  public = list(
    calc_importance = function() {
      self$importance <- xgb.importance(model = self$model)
      return(self$importance)
    }
  ),
  
  private = list(
    fit = function(X_train_, y_train_, ...) {
      self$model <- xgboost(data = X_train_, label = y_train_, nrounds = 15, verbose = 0,
                            objective = "reg:squarederror", eval_metric = "rmse", ...)
      return(invisible(self))
    }
  )
)


# 3.3 my_LightGBM_model                                          ####
my_LightGBM_model <- R6Class(
  classname = "my_LightGBM_model",
  inherit = my_template_model,
  
  public = list(
    initialize = function(X_train_, y_train_, X_test_, y_test_) {
      X_train_mat_ <- data.matrix(X_train_)
      X_test_mat_ <- data.matrix(X_test_)
      
      dtrain <- lgb.Dataset(data = X_train_mat_, label = y_train_, free_raw_data = FALSE)
      dtest <- lgb.Dataset.create.valid(dtrain, data = X_test_mat_, label = y_test_)
      
      private$fit(dtrain, dtest)
      self$pred_train <- private$predict(X_train_mat_)
      
      if (!is.null(X_test_)) self$pred_test <- private$predict(X_test_mat_, is_test = TRUE)
      if (!is.null(y_test_) && !is.null(self$pred_test)) private$calc_rmse(y_test_, self$pred_test)
      
      return(invisible(self))
    }
  ),
  
  private = list(
    fit = function(dtrain, dtest) {
      self$model <- lgb.train(
        params = list(objective = "regression", metric = "rmse", num_iterations = 250),
        data = dtrain,
        valids = list(train = dtrain, eval = dtest),
        verbose = -1
      )
      return(invisible(self))
    }
  )
)


# 3.4 my_LightGBM_CV_model                                          ####
my_LightGBM_CV_model <- R6Class(
  classname = "my_LightGBM_CV_model",
  inherit = my_template_model,
  
  public = list(
    initialize = function(X_train_, y_train_, X_test_, y_test_) {
      X_train_mat_ <- data.matrix(X_train_)
      X_test_mat_ <- data.matrix(X_test_)
      
      dtrain <- lgb.Dataset(data = X_train_mat_, label = y_train_, free_raw_data = FALSE)
      private$fit(dtrain)
      self$pred_train <- private$predict(X_train_mat_)
      
      if (!is.null(X_test_)) self$pred_test <- private$predict(X_test_mat_, is_test = TRUE)
      if (!is.null(y_test_) && !is.null(self$pred_test)) private$calc_rmse(y_test_, self$pred_test)
      
      return(invisible(self))
    }
  ),
  
  private = list(
    fit = function(dtrain) {
      self$model <- lgb.cv(
        params = list(objective = "regression", metric = "rmse"), # , seed = 43, deterministic = TRUE
        data = dtrain,
        nfold = 5L,
        verbose = -1
      )
      return(invisible(self))
    },
    
    predict = function(X_, is_test = FALSE) {
      calc_mean_matrix <- function(mat_list) Reduce('+', mat_list) / length(mat_list)
      
      preds <- lapply(self$model$boosters, \(cv_mdl) predict(cv_mdl, X_)$booster) %>% calc_mean_matrix()
      if (is_test) self$pred_test <- preds
      return(preds)
    }
  )
)


# 3.5 my_RandomForest_model                                          ####
my_RandomForest_model <- R6Class(
  classname = "my_RandomForest_model",
  inherit = my_template_model,
  
  public = list(
    calc_importance = function() {
      self$importance <- self$model$importance
      return(self$importance)
    }
  ),
  
  private = list(
    fit = function(X_train_, y_train_) {
      # na.action = na.omit,
      self$model <- randomForest(x = X_train_, y = y_train_, ntree = 1000, importance = TRUE) 
      return(invisible(self))
    }
  )
)


# 3.6 my_lm_model                                          ####
my_lm_model <- R6Class(
  classname = "my_lm_model",
  inherit = my_template_model,
  
  public = list(
    initialize = function(X_train_, y_train_, X_test_ = NULL, y_test_ = NULL, ...) {
      X_train_ <- as.data.frame(X_train_)
      X_test_  <- as.data.frame(X_test_)
      
      private$fit(X_train_, y_train_, ...)
      self$pred_train <- private$predict(X_train_)
      
      if (!is.null(X_test_)) self$pred_test <- private$predict(X_test_, is_test = TRUE)
      if (!is.null(y_test_) && !is.null(self$pred_test)) private$calc_rmse(y_test_, self$pred_test)
      
      return(invisible(self))
    }
  ),
  
  private = list(
    fit = function(X_train_, y_train_) {
      self$model  <- lm(target ~ ., data = data.frame(X_train_, target = y_train_))
      return(invisible(self))
    }
  )
)


# 3.7 my_Catboost_model                                          ####
my_Catboost_model <- R6Class(
  classname = "my_Catboost_model",
  inherit = my_template_model,
  
  public = list(
    initialize = function(X_train_, y_train_, X_test_, y_test_) {
      train_pool <- catboost.load_pool(data = X_train_, label = y_train_)
      test_pool  <- catboost.load_pool(data = X_test_,  label = y_test_)
      
      private$fit(train_pool, test_pool)
      self$pred_train <- private$predict(train_pool)
      
      if (!is.null(X_test_)) self$pred_test <- private$predict(test_pool, is_test = TRUE)
      if (!is.null(y_test_) && !is.null(self$pred_test)) private$calc_rmse(y_test_, self$pred_test)
      
      return(invisible(self))
    }
  ),
  
  private = list(
    fit = function(train_pool, test_pool) {
      self$model <- catboost.train(
        learn_pool = train_pool,
        test_pool = test_pool,
        params = list(loss_function = 'RMSE', logging_level = "Silent")
      )
      return(invisible(self))
    },
    
    predict = function(X_, is_test = FALSE) {
      preds <- catboost.predict(self$model, X_)
      if (is_test) self$pred_test <- preds
      return(preds)
    }
  )
)
