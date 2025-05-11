#@ Diploma_Holland
#@ Функции подбора весов моделей
#@ Дата: 04.05.2025
#@ Разработчик: Глушков Егор
#@ Изменения: -


# 1. Библиотеки и константы                                      ####
libs <- c("Rcpp", "RcppArmadillo", "quadprog", "GA", "pso", "rBayesianOptimization")
install_pkgs(libs)

# sourceCpp(paste0(here::here(), "/1. R/05._Rcpp_functions.cpp")) # rcpp_rmse, approx_shapley_cpp


# 2. Функции                                                     ####

## 2.1 Общие функции                                             ####
cut_w <- function(w, threshold = 9.5, qnt_prob = NULL) {
  if (!is.null(qnt_prob)) threshold <- quantile(w, qnt_prob)
  w[w < threshold] <- 0
  return(w / sum(w))
}


cut_matr <- function(matr, qnt_prob = 0.67, Y_true = Y_test) {
  w <- sapply(matr, \(probs) df_metric(probs, Y_true, func = calc_C_index))
  w <- cut_w(w, qnt_prob = qnt_prob)
  sngnf_ind <- which(w > 1e-3)
  filtered_matr <- matr[sngnf_ind]
  return(filtered_matr)
}


matr_cind <- function(prob_matrix, Y_true) {
  cindex <- prob_matrix %>% 
    as.data.table() %>% as.matrix() %>% 
    df_metric(., Y_true, func = calc_C_index)
  return(cindex)
}


weighted_cindex <- function(w_, matr_list, Y_true = Y_test, label = NULL) {
  res <- scalar_matrix_production(w_, matr_list) %>%
    df_metric(., Y_true, func = calc_C_index)
  if (!is.null(label)) {
    cat(label, ":\t", round(res, 3), "\n", sep = "")
    return(invisible(res))
  } else {
    return(res)
  }
}


# Возвращает только значение, без print
weighted_cindex_value <- function(w, matr_list, Y_true) {
  v <- scalar_matrix_production(w, matr_list) %>% 
    df_metric(., Y_true, func = calc_C_index)
  return(v)
}


weighted_rmse_value <- function(w, matr_list, Y_true) {
  v <- scalar_matrix_production(w, matr_list) %>% 
    df_metric(., Y_true, func = my_rmse)
  return(v)
}


run_weights_search_experiments <- function(experiments_df, models_probs, Y_true) {
  evaluate_weights <- function(method_fun, models_probs, Y_true, ...) {
    w <- do.call(method_fun, c(list(models_probs = models_probs, Y_true = Y_true), ...))
    cidx <- weighted_cindex_value(w, models_probs, Y_true)
    list(cindex = cidx, w = list(w))
  }
  
  res <- experiments_df %>%
    copy() %>% 
    .[, metrics := pmap(list(method, params), \(fn, extra_args)
                        do.call(evaluate_weights, c(list(fn, models_probs, Y_true), extra_args))
    )] %>% 
    as.data.table() %>% 
    .[, .(label, cindex = metrics[[1]]$cindex, w = metrics[[1]]$w), by = .I]
  return(res)
}


get_regr_w <- function(weight_config, matrices, Y_true) {
  regr_w <- run_weights_search_experiments(weight_config, matrices, Y_true) %>% 
    .[, rmse := sapply(w, \(w_vec) weighted_rmse_value(w_vec, matrices, Y_true))]
  return(regr_w)
}


write_w_search_to_xlsx <- function(w_srch_res, matr, sheetname, 
                                   filename = here("4. Output/10. Classification_ensembles_weights.xlsx")) {
  lbls <- names(matr)
  res <- w_srch_res %>% 
    copy() %>% 
    .[, .(w_ = w[[1]]), by = .I] %>% 
    .[, w_ := round(w_, 3)] %>% 
    .[, model_i := 1:.N, by = I] %>% 
    dcast(I ~ model_i, value.var = "w_") %>% 
    merge(w_srch_res[, -"w"], by = "I", all.x = TRUE) %>% 
    setnames(old = as.character(1:length(lbls)), new = lbls) %>% 
    relocate(I, label, cindex)
  
  xlsx::write.xlsx(res, filename, sheetname, row.names = FALSE, append = TRUE)
  return(invisible(res))
}


## 2.2 Функции поиска наилучшего набора весов моделей               ####

# Шэпли + Монте-Карло
# также можно использовать approx_shapley_cpp
approx_shapley <- function(models_probs, Y_true, R = 500) {
  M <- length(models_probs)
  N <- nrow(models_probs[[1]])
  K <- ncol(models_probs[[1]])
  model_names <- names(models_probs) %||% as.character(seq_len(M))
  
  phi <- numeric(M)
  names(phi) <- model_names
  prob_array <- array(unlist(models_probs), dim = c(N, K, M))
  
  for (r in seq_len(R)) {
    perm <- sample(M)
    weights <- 1:M
    cum_sum <- matrix(0, nrow = N, ncol = K)
    
    for (k in seq_len(M)) {
      idx <- perm[k]
      prob_k <- prob_array[,,idx]
      cum_avg <- (cum_sum + prob_k) / k
      v_prev <- if (k == 1) 0 else matr_cind(cum_sum / (k - 1), Y_true)
      v_curr <- matr_cind(cum_avg, Y_true)
      phi[idx] <- phi[idx] + (v_curr - v_prev)
      cum_sum <- cum_sum + prob_k
    }
  }
  
  w <- phi / R
  return(w / sum(w))
}


# Перебор по сетке, Random Search (при больших N, если быть точным)
grid_search_weights <- function(models_probs, Y_true, step = 1.0) {
  M <- length(models_probs)
  ws_list <- lapply(seq_len(M), function(i) seq(0, 1, by = step))
  names(ws_list) <- paste0("w", seq_len(M))
  
  weight_cols <- names(ws_list)
  dt <- do.call(CJ, ws_list) %>% 
    .[, sumw := rowSums(.SD), .SDcols = weight_cols] %>% 
    .[sumw != 0] %>% 
    .[, .SD / sumw] %>% 
    distinct() %>% 
    .[, sumw := NULL]
  
  if (dt[, .N] > 1e4) dt <- dt[sample(.N, 1e4), ]
  
  dt <- dt %>% copy() %>% 
    .[, score := weighted_cindex_value(as.numeric(.SD), models_probs, Y_true), 
      by = .I, .SDcols = weight_cols]
  w <- dt[which.max(score), ..weight_cols] %>% as.numeric()
  return(w / sum(w))
}


# Стэкинг моделей, решение с помощью квадратичного линейного программирования
stacking_qp_weights <- function(models_probs, Y_true, ...) {
  Y_true_onehot <- Y_true %>% apply(1, bool_mask_row) %>% t() 
  
  M <- length(models_probs)
  N <- nrow(Y_true_onehot); K <- ncol(Y_true_onehot)
  
  X <- do.call(cbind, lapply(models_probs, \(P) as.vector(P)))
  y <- as.vector(Y_true_onehot)
  
  # QP: min ½ w' D w – d' w, при w ≥ 0, sum(w)=1
  Dmat <- crossprod(X)         # M×M
  dvec <- crossprod(X, y)      # M
  
  # матрица ограничений: сначала sum(w)=1, потом w_i ≥ 0
  Amat <- cbind(rep(1, M), diag(M))
  bvec <- c(1, rep(0, M))
  meq  <- 1
  
  eigenvalues <- eigen(Dmat, only.values = TRUE)$values
  eigenvalues[abs(eigenvalues) < 1e-8] <- 0
  if (any(eigenvalues <= 0)) Dmat <- Matrix::nearPD(Dmat)$mat %>% as.matrix()
  
  w <- quadprog::solve.QP(Dmat, dvec, Amat, bvec, meq)$solution
  w_rnd <- (w / sum(w)) %>% round(8)
  return(w_rnd)
}


# Генетический алгоритм (мутации и скрещивания поколений в популяции)
genetic_algorithm_weights <- function(models_probs, Y_true, popSize = 50, maxiter = 150, run = 50) {
  M <- length(models_probs)
  fitness <- function(x) weighted_cindex_value(x / sum(x), models_probs, Y_true)
  GAres <- GA::ga(
    type     = "real-valued",
    fitness  = fitness,
    lower    = rep(0, M),
    upper    = rep(1, M),
    popSize  = popSize,
    maxiter  = maxiter,
    run      = run,
    monitor  = FALSE
  )
  w <- GAres@solution[1, ] %>% unname()
  return(w / sum(w))
}


# Метод роя частиц (Particle Swarm Opt)
particle_swarm_weights <- function(models_probs, Y_true, swarm_size = 50, maxit = 100) {
  M <- length(models_probs)
  fn_pso <- function(x) {-weighted_cindex_value(x / sum(x), models_probs, Y_true)}
  PSOres <- pso::psoptim(
    par       = rep(1/M, M),
    fn        = fn_pso,
    lower     = rep(.Machine$double.eps, M),
    upper     = rep(1, M),
    control   = list(s = swarm_size, maxit = maxit, trace = FALSE, vectorize = TRUE, maxit.stagnate = 10)
  )
  w <- PSOres$par
  w[w < 1e-6] <- 0
  return(w / sum(w))
}


# Байесовская оптимизация
# it runs (init_points + n_iter) times
bayes_optimize_weights <- function(models_probs, Y_true, init_points = 15, n_iter = 15) {
  M <- length(models_probs)
  # задаём границы x1…xM ∈ [0,1]
  bounds <- setNames(rep(list(c(0, 1)), M), paste0("x", seq_len(M)))
  
  FUN <- function(...) {
    w <- c(...)
    list(Score = weighted_cindex_value(w / sum(w), models_probs, Y_true), Pred = 0)
  }
  
  BO_res <- rBayesianOptimization::BayesianOptimization(
    FUN         = FUN,
    bounds      = bounds,
    init_points = init_points,
    n_iter      = n_iter,
    verbose     = FALSE
  )
  w <- unlist(BO_res$Best_Par)
  return(w / sum(w))
}


# Координатный спуск (Coordinate Ascent) на симплексе w_i ≥0, ∑w_i=1
coordinate_optimize_weights <- function(models_probs, Y_true, tol = 1e-4, max_iter = 20) {
  M <- length(models_probs)
  w <- rep(1/M, M)
  
  for (iter in seq_len(max_iter)) {
    w_old <- w
    for (i in seq_len(M)) {
      old_wi <- w[i]
      if (old_wi < 1) {
        # функция от нового w_i (alpha)
        f_alpha <- function(alpha) {
          # обновляем w_i = alpha, остальные пропорционально уменьшаем
          w_new   <- w
          w_new[i] <- alpha
          w_new[-i] <- w[-i] * (1 - alpha) / (1 - old_wi)
          weighted_cindex_value(w_new, models_probs, Y_true)
        }
        # находим оптимальное alpha ∈ [0,1]
        opt <- stats::optimize(f_alpha, lower = 0, upper = 1, maximum = TRUE)
        # применяем обновление
        w[i]  <- opt$maximum
        w[-i] <- w[-i] * (1 - w[i]) / (1 - old_wi)
      }
    }
    # проверяем сходимость
    if (max(abs(w - w_old)) < tol) break
  }
  return(w)
}


equal_weights <- function(models_probs, Y_true = NULL) {
  w <- rep(1/length(models_probs), length(models_probs))
  return(w / sum(w))
}


simple_weights <- function(models_probs, Y_true) {
  w <- sapply(models_probs, \(probs) df_metric(probs, Y_true, func = calc_C_index))
  return(w / sum(w))
}


filtered_weights <- function(models_probs, Y_true, qnt_prob = 0.67) {
  w <- simple_weights(models_probs, Y_true)
  w_norm <- cut_w(w, qnt_prob = 0.67)
  return(w_norm)
}


## 2.3 Ансамбли для потестовых данных                ####

best_fit_bytests_ensemble <- function(X_train_, Y_train_, X_test_, Y_test_) {
  fit_by_tests_ensemble(model_class = my_regularized_lm_model, 
                        X_train_, Y_train_, X_test_, Y_test_,
                        psytests = c("BF", "CT", "EY", "LN", "SC"), 
                        MO_regr = no_MO_regr, 
                        weight_model = particle_swarm_weights, 
                        weight_model_params = list(swarm_size = NA, maxit = 75),
                        alpha = 0)
}


fit_by_tests_ensemble <- function(model_class, 
                                  X_train_, Y_train_, X_test_, Y_test_,
                                  psytests = c("BF", "CT", "EY", "LN", "SC"), 
                                  MO_regr = stack_MO_regr, 
                                  weight_model = particle_swarm_weights, 
                                  weight_model_params = list(),
                                  ...) {
  models_by_psytests <- NULL
  pred_valid_by_psytests <- NULL
  psytests <- psytests %>% sort()
  
  for (psytest in psytests) {
    models <- MO_regr(
      model_class = model_class, 
      X_train_ = X_train_[, str_subset(colnames(X_train_), psytest), drop = FALSE], 
      Y_train_ = Y_train_,
      X_test_ = X_test_[, str_subset(colnames(X_test_), psytest), drop = FALSE], 
      Y_test_ = Y_test_,
      ...
    )
    models_by_psytests[[psytest]]     <- models
    pred_valid_by_psytests[[psytest]] <- lapply(models, \(x) x$pred_test) %>% do.call(cbind, .)
  }
  
  # 31 комбинация
  psy_combos <- sapply(1:length(psytests), \(i) combn(psytests, i, simplify = FALSE)) %>%
    purrr::list_flatten() %>% 
    lapply(sort)
  
  # веса для каждой из комбинаций (и их cindex)
  w_combos <- data.table(comb = psy_combos) %>% 
    .[, comb_str := sapply(comb, toString)] %>% 
    .[, w_comb := lapply(comb, \(cmb) do.call(
      weight_model, 
      args = c(list(pred_valid_by_psytests[cmb], Y_test), weight_model_params)
    ))] %>% 
    .[, cindex := map2_dbl(w_comb, comb, \(w, cm)
                           weighted_cindex_value(w, pred_valid_by_psytests[cm], Y_test))]
  
  return(list(models_by_psytests = models_by_psytests, w_combos = w_combos))
}


predict_by_tests_ensemble <- function(X_na, Y_na, models_by_psytests, w_combos, 
                                      psytests = c("BF", "CT", "EY", "LN", "SC")) {
  psytests <- psytests %>% sort()
  psytest_preds_by_id <- X_na[, .(id)]
  
  for (psytest in psytests) {
    X_psytest <- X_na[, .SD, .SDcols = patterns(paste0(psytest, "|id"))] %>% drop_na()
    
    pred <- lapply(models_by_psytests[[psytest]], \(mdl) mdl$predict(as.matrix(X_psytest[, -"id"]))) %>% 
      do.call(cbind, .)
    pred_id <- X_psytest[, .(id)] %>% 
      cbind(pred) %>% 
      setnames(new = c("id", paste0("HL_", 1:6))) %>% 
      .[, .(test = list(as.numeric(.SD))), by = id, .SDcols = paste0("HL_", 1:6)] %>% 
      setnames(old = "test", new = psytest)
    
    psytest_preds_by_id <- psytest_preds_by_id %>% merge(pred_id, all.x = TRUE, by = "id")
  }
  
  # какие тесты для каждого id были пройдены
  psytests_by_id <- psytest_preds_by_id %>% 
    copy() %>% 
    melt(id.vars = "id", variable.name = "psytest", variable.factor = FALSE) %>% 
    .[, val_len := sapply(value, \(v) length(v) > 0)] %>% 
    .[val_len == TRUE] %>%
    .[, .(comb_str = psytest %>% sort() %>% toString()), by = "id"]
  
  # для каждого id собраны предсказания по каждому тесту, его комбинация тестов, их веса, сделанное на их основе общее предсказание
  psytest_preds_by_id <- psytest_preds_by_id %>% 
    merge(psytests_by_id, all.x = TRUE, by = "id") %>% 
    merge(w_combos[, -c("cindex")], all.x = TRUE, by = "comb_str") %>%
    .[order(id)] %>%
    .[, pred := pmap(.SD, function(...) {
        args <- list(...)                        # named list: w_comb, BF, EY, …
        # args[psytests] -- идут строго в том же порядке, что и веса, для их 1-1 соответствия
        vecs <- purrr::compact(args[psytests])   # compact удаляет NULL-элементы
        scalar_matrix_production(w_vec = args[["w_comb"]], mat_list = vecs)
      },
      .SDcols = c("w_comb", psytests)
    )]
  
  # Y_pred <- psytest_preds_by_id %>% copy() %>% 
  #   .[order(id), pred] %>%
  #   do.call(rbind, .)
  # distr_metric(Y_pred, Y_na, func = calc_C_index)
  
  # предсказание + настоящая метка + оценка C-index для каждой строки
  pred_true_df <- psytest_preds_by_id %>% copy() %>% 
    .[order(id), .(id, pred)] %>% 
    cbind(Y_na) %>% 
    .[, .(pred, y_true = list(as.numeric(.SD))), by = id, .SDcols = paste0("HL_", 1:6)] %>% 
    .[, cindex := map2_dbl(pred, y_true, calc_C_index)]
  
  # cindex = pred_true_df$cindex %>% mean() %>% round(3)
  # p = pred_true_df$cindex %>% plot_cindex_hist()
  return(pred_true_df)
}


# 3. Данные и константы                                          ####
w_config <- tribble(
  ~method,                     ~label,           ~params,
  equal_weights,               "Equal w",        list(list()),
  simple_weights,              "Simple w",       list(list()),
  filtered_weights,            "Filtered w",     list(list(qnt_prob = 0.67)),
  approx_shapley,              "Approx Shap",    list(list(R = 500)),
  grid_search_weights,         "Grid Search",    list(list(step = 1.0)),
  stacking_qp_weights,         "QP",             list(list()),
  genetic_algorithm_weights,   "GA",             list(list(popSize = 25, maxiter = 150)),
  particle_swarm_weights,      "PSO",            list(list(swarm_size = 50, maxit = 100)),
  # bayes_optimize_weights,      "Bayes Opt",      list(list(init_points = 15, n_iter = 15)),
  coordinate_optimize_weights, "Coord. Asc",     list(list(tol = 1e-4, max_iter = 20))
) %>% 
  as.data.table()

w_bytest_comb_config <- tribble(
  ~method,                     ~label,           ~params,
  equal_weights,               "Equal w",        list(),
  simple_weights,              "Simple w",       list(),
  grid_search_weights,         "Grid Search",    list(step = 0.5),
  stacking_qp_weights,         "QP",             list(),
  genetic_algorithm_weights,   "GA",             list(popSize = 10, maxiter = 50),
  particle_swarm_weights,      "PSO",            list(swarm_size = NA, maxit = 75)
  #, coordinate_optimize_weights, "Coord. Asc",     list(tol = 1e-3, max_iter = 15)
) %>% 
  as.data.table()
