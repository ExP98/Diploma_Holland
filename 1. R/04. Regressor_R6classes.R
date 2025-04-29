#@ Diploma_Holland
#@ R6-классы моделей регрессии
#@ Дата: 14.04.2025
#@ Разработчик: Глушков Егор


# 1. Библиотеки                                                  ####
# library(R6)
# library(xgboost)
# library(lightgbm)
# library(randomForest)
# library(catboost)
# library(e1071)
# library(FNN)
# library(glmnet)                         # Для многомерной регрессии
library(MASS, include.only = "stepAIC") # Для stepwise регрессии
# library(pls)                            # Для многомерных методов


# 2. Функции                                                     ####


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


my_stepwise_lm_model <- R6Class(
  classname = "my_stepwise_lm_model",
  inherit = my_lm_model,
  
  private = list(
    fit = function(X_train_, y_train_) {
      just_lm  <- lm(target ~ ., data = data.frame(X_train_, target = y_train_))
      self$model <- stepAIC(just_lm, direction = "both", trace = FALSE)
      return(invisible(self))
    }
  )
)


# Обучение модели с кросс-валидацией
get_L1_L2_glmnet_preds <- function(X_train_ = X_train, Y_train_ = Y_train, 
                                   X_test_ = X_test, Y_test_ = Y_test,
                                   alpha = 1, print_metric = TRUE, need_to_correct = FALSE) {
  cv_glm <- cv.glmnet(X_train_, Y_train_, family = "mgaussian", alpha = alpha)
  glm_pred <- predict(cv_glm, newx = X_test_, s = "lambda.min")[,,1]
  if (need_to_correct == TRUE) glm_pred <- glm_pred %>% prediction_correction()
  
  lbl <- if (alpha == 1) "Lasso" else if (alpha == 0) "Ridge" else "Wrong"
  if (print_metric) print(show_custom_metrics(glm_pred, paste0("GLM ", lbl), Y_test_ = Y_test_))
  return(invisible(glm_pred))
}


my_L1_L2_glmnet_model <- R6Class(
  classname = "my_L1_L2_glmnet_model",
  inherit = my_template_model,
  
  public = list(
    initialize = function(X_train_, y_train_, X_test_, y_test_, ...) {
      alpha <- list(...) %>% pluck("alpha")
      if (is.null(alpha)) alpha <- 1
      
      private$fit(X_train_, y_train_, alpha_ = alpha)
      self$pred_train <- private$predict(X_train_)
      
      if (!is.null(X_test_)) self$pred_test <- private$predict(X_test_, is_test = TRUE)
      if (!is.null(y_test_) && !is.null(self$pred_test)) private$calc_rmse(y_test_, self$pred_test)
      
      return(invisible(self))
    }
  ),
  
  private = list(
    fit = function(X_train_, y_train_, alpha_) {
      self$model <- cv.glmnet(X_train_, y_train_, family = "gaussian", alpha = alpha_)
      return(invisible(self))
    },
    
    predict = function(X_, is_test = FALSE) {
      preds <- predict(self$model, newx = X_, s = "lambda.min")
      if (is_test) self$pred_test <- preds
      return(preds)
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


# 3.8 my_knn_model                                          ####
my_knn_model <- R6Class(
  classname = "my_knn_model",
  inherit = my_template_model,
  
  public = list(
    initialize = function(X_train_, y_train_, X_test_, y_test_ = NULL, ...) {
      k <- list(...) %>% pluck("k")
      if (is.null(k)) k <- 10
      
      private$fit(X_train_, y_train_, X_test_, k = k)
      self$pred_test <- private$predict(X_test_, is_test = TRUE)
      if (!is.null(y_test_) && !is.null(self$pred_test)) private$calc_rmse(y_test_, self$pred_test)
      return(invisible(self))
    }
  ),
  
  private = list(
    fit = function(X_train_, y_train_, X_test_, k) {
      self$model <- knn.reg(X_train_, test = X_test_, y = y_train_, k = k)
      return(invisible(self))
    },
    
    predict = function(X_, is_test = FALSE) {
      preds <- self$model$pred
      if (is_test) self$pred_test <- preds
      return(preds)
    }
  )
)


# 3.9 my_SVR_model                                          ####
my_SVR_model <- R6Class(
  classname = "my_SVR_model",
  inherit = my_template_model,
  
  private = list(
    fit = function(X_train_, y_train_, ...) {
      self$model <- svm(x = X_train_, y = y_train_, ...)
      return(invisible(self))
    }
  )
)