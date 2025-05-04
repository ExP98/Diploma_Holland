#@ Diploma_Holland
#@ Функции подбора весов моделей
#@ Дата: 04.05.2025
#@ Разработчик: Глушков Егор
#@ Изменения: -


# 1. Библиотеки и константы                                      ####
libs <- c("Rcpp", "RcppArmadillo", "quadprog", "GA", "pso", "rBayesianOptimization")
install_pkgs(libs)

sourceCpp(paste0(here::here(), "/1. R/05._Rcpp_functions.cpp")) # rcpp_rmse, approx_shapley_cpp


# 2. Функции                                                     ####

## 2.1 Общие функции                                             ####
cut_w <- function(w, threshold = 9.5, qnt_prob = NULL) {
  if (!is.null(qnt_prob)) threshold <- quantile(w, qnt_prob)
  w[w < threshold] <- 0
  return(w / sum(w))
}


cut_matr <- function(matr, qnt_prob = 0.67) {
  w <- sapply(matr, \(probs) df_metric(probs, Y_test, func = calc_C_index))
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


write_w_search_to_xlsx <- function(w_srch_res, matr, sheetname) {
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
  
  xlsx::write.xlsx(res, here("0. Data/1. Output/clsf_ensembles/clsf_ensembles_weights.xlsx"), 
                   sheetname, row.names = FALSE, append = TRUE)
  return(invisible(res))
}


## 2.2 Функции поиска наилучшего набора весов моделей               ####

# Шэпли + Монте-Карло
# также можно использовать approx_shapley_cpp
approx_shapley <- function(models_probs, Y_true, metric_fn = matr_cind, R = 500) {
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
      v_prev <- if (k == 1) 0 else metric_fn(cum_sum / (k - 1), Y_true)
      v_curr <- metric_fn(cum_avg, Y_true)
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
  w <- as.numeric(best[, weight_cols, with = FALSE])
  return(w / sum(w))
}


# Стэкинг моделей, решение с помощью квадратичного линейного программирования
stacking_qp_weights <- function(models_probs, Y_true_onehot, ...) {
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
    lower     = rep(0, M),
    upper     = rep(1, M),
    control   = list(s = swarm_size, maxit = maxit, trace = FALSE)
  )
  w <- PSOres$par
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


simple_weights <- function(models_probs, Y_true) {
  w <- sapply(models_probs, \(probs) df_metric(probs, Y_true, func = calc_C_index))
  return(w / sum(w))
}


filtered_weights <- function(models_probs, Y_true, qnt_prob = 0.67) {
  w <- simple_weights(models_probs, Y_true)
  w_norm <- cut_w(w, qnt_prob = 0.67)
  return(w_norm)
}
