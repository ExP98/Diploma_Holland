#include <Rcpp.h>
using namespace Rcpp;

//' @title calculate RMSE
//' @keywords internal 
//' @return root-mean-square error value
// [[Rcpp::export]]
double rcpp_rmse(Rcpp::NumericVector& y, Rcpp::NumericVector& y_hat) {
 Rcpp::NumericVector diff = y - y_hat;
 return sqrt( Rcpp::sum(diff*diff) / y.size() );
}
