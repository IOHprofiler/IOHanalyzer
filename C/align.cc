#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector align_by_target_inner_loop(double t, int idxEvals, int idxTarget, 
  List data, NumericVector index, NumericMatrix next_lines, NumericVector curr_eval) {

  int N = data.size();
  NumericVector out = clone(curr_eval);

  for (int k = 0; k < N; k++) {
    NumericMatrix d = as<NumericMatrix>(data[k]);
    int n_row = d.nrow();
    int n_col = d.ncol();
    int iter = index[k];

    while (true) {
      if (next_lines(k, idxTarget) >= t) {
        out[k] = next_lines(k, idxEvals);
        break;
      }

      if (iter < (n_row - 1)) {
        iter++;
        for (int j = 0; j < n_col; j++) {
          next_lines(k, j) = d(iter, j);
        }
      } else {
        break;
      }
    }
    index[k] = iter;
  }
  return out;
}

// [[Rcpp::export]]
void align_by_runtime_inner_loop(int r, int idxEvals, int idxTarget, 
  List data, NumericVector n_rows, NumericVector index, NumericMatrix next_lines,
  NumericVector curr_fvalues) {

  int N = data.size();
  for (int k = 0; k < N; k++) {
    NumericMatrix d = as<NumericMatrix>(data[k]);
    int n_row = n_rows[k];
    int iter = index[k];
    int n_col = d.ncol();

    while (!NumericVector::is_na(next_lines(k, idxEvals))) {
      if (next_lines(k, idxEvals) >= r) {
        curr_fvalues[k] = next_lines(k, idxTarget);
        break;
      }

      if (iter < (n_row - 1)) {
        iter++;
        for (int j = 0; j < n_col; j++) {
          next_lines(k, j) = d(iter, j);
        }
      } else {
        curr_fvalues[k] = next_lines(k, idxTarget);
        next_lines(k, idxEvals) = NA_REAL;
      }
    }
    index[k] = iter;
  }
}

// [[Rcpp::export]]
NumericVector c_impute(NumericVector x, NumericVector y, NumericVector rowname) {
  int N = rowname.size();
  NumericVector res(N, NA_REAL);

  int j = 0;
  for (int i = 0; i < N; ++i) {
    if (rowname[i] == y[j]) {
      res[i] = x[j];
      ++j;
    } else {
      res[i] = x[j - 1];
    }
  }
  return res;
}
