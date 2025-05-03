#include <RcppArmadillo.h>
#include <Rcpp.h>
using namespace Rcpp;
using arma::mat;

//' @title calculate RMSE
//' @keywords internal 
//' @return root-mean-square error value
// [[Rcpp::export]]
double rcpp_rmse(Rcpp::NumericVector& y, Rcpp::NumericVector& y_hat) {
 Rcpp::NumericVector diff = y - y_hat;
 return sqrt( Rcpp::sum(diff*diff) / y.size() );
}


// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
NumericVector approx_shapley_cpp(List prob_list,
                                 NumericMatrix Y_true,
                                 Function metric_fn,
                                 int R = 1000) {
  int M = prob_list.size();
  int N = Y_true.nrow(), K = Y_true.ncol();
  std::vector<mat> P(M);
  for (int i = 0; i < M; ++i) P[i] = as<mat>(prob_list[i]);
  arma::vec phi(M, arma::fill::zeros);
  std::vector<int> perm(M);
  std::iota(perm.begin(), perm.end(), 0);
  std::mt19937_64 rng((unsigned) std::time(nullptr));
  mat cum_sum(N, K, arma::fill::zeros);
  NumericMatrix avg(N, K);
  
  for (int r = 0; r < R; ++r) {
    std::shuffle(perm.begin(), perm.end(), rng);
    cum_sum.zeros();
    for (int k = 0; k < M; ++k) {
      int idx = perm[k];
      double v_prev = 0.0;
      if (k > 0) {
        for (int i = 0; i < N; ++i)
          for (int j = 0; j < K; ++j)
            avg(i,j) = cum_sum(i,j) / k;
        v_prev = as<double>( metric_fn(avg, Y_true) );
      }
      cum_sum += P[idx];
      for (int i = 0; i < N; ++i)
        for (int j = 0; j < K; ++j)
          avg(i,j) = cum_sum(i,j) / (k + 1);
      double v_curr = as<double>( metric_fn(avg, Y_true) );
      phi[idx] += (v_curr - v_prev);
    }
  }
  
  double sum_phi = arma::sum(phi);
  NumericVector w(M);
  for (int i = 0; i < M; ++i) w[i] = phi[i] / sum_phi;
  return w;
}
