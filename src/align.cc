#include <Rcpp.h>
#include <iostream>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector align_by_target_inner_loop(double t, int idxEvals, int idxTarget,
List data, NumericVector index, NumericMatrix next_lines, NumericVector curr_eval, bool maximization) {

    bool condition;
    int n_row, n_col, iter, N;
    N = data.size();
    NumericVector out = clone(curr_eval);

    for (int k = 0; k < N; k++) {
      NumericMatrix d = as<NumericMatrix>(data[k]);
      n_row = d.nrow();
      n_col = d.ncol();
      iter = index[k];

      while (true) {
        if (maximization) {
          condition = next_lines(k, idxTarget) >= t;
        } else {
          condition = next_lines(k, idxTarget) <= t;
        }

        if (condition) {
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
NumericVector c_impute(NumericVector x, NumericVector y, NumericVector rowname) {
  int N = rowname.size();
  int L = x.size();
  NumericVector res(N, NA_REAL);

  int i = 0;
  int j = 0;
  while (i < N) {
    if (j < (L - 1) && rowname[i] >= y[j]) {
      if (rowname[i] < y[j + 1]) {
        res[i] = x[j];
        ++j;
      } else {
        ++j;
        continue;
      }
    // if the query value is larger than the last runtime value `y[L - 1]`
    } else if (j == L - 1 && rowname[i] >= y[j]) {
      res[i] = x[j]; 
    // take the previous function value if the next runtime in y[j] is not reached
    } else if (rowname[i] < y[j] && j > 0) {
      res[i] = x[j - 1];
    }
    ++i;
  }
  return res;
}

// [[Rcpp::export]]
NumericMatrix c_impute_running_time(NumericVector index, // index value recorded
                                    NumericMatrix value, // value to align: runtime + parameters
                                    NumericVector FV,    // function values to match
                                    bool maximization    // the data are collected from a maximization problem?
                                    ) {
  int N = FV.size();
  int L = index.size();
  int M = value.ncol();
  NumericMatrix res(N, M);
  std::fill(res.begin(), res.end(), NA_REAL);

  bool condition;
  int j = 0;
  for (int i = 0; i < N; ++i) {
    while (j < L) {
      if (maximization) {
        condition = FV[i] > index[j];
      } else {
        condition = FV[i] < index[j];
      }

      if (condition) {
        ++j;
      } else {
        res(i, _) = value(j, _);
        break;
      }
    }
  }
  return res;
}

// TODO: Better comments
//' Align a list of data set by function values
//'
//' @param data the data
//' @param FV Function values
//' @param idxValue index of the function values
//' @param maximization Boolean
//' @param idxTarget index of the target
//' @noRd
// [[Rcpp::export]]
List c_align_running_time(List data, NumericVector FV, NumericVector idxValue, bool maximization, int idxTarget) {
  int NC = data.size();
  int NR = FV.size();
  int M = idxValue.size();

  List res(M);
  for (int i = 0; i < M; i++) {
    NumericMatrix aux(NR, NC);
    rownames(aux) = FV;
    res[i] = clone(aux);
  }

  for (int i = 0; i < NC; i++) {
    NumericMatrix d = data[i];
    NumericMatrix value(d.nrow(), M);

    for (int j = 0; j < M; j++) {
      value(_, j) = d(_, idxValue[j]);
    }

    NumericMatrix tmp = c_impute_running_time(d(_, idxTarget), value, FV, maximization);

    for (int k = 0; k < M; k++) {
      NumericMatrix aux = res[k];
      aux(_, i) = tmp(_, k);
    }
  }
  return res;
}
