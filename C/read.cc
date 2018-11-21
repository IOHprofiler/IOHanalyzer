#include <Rcpp.h>
#include <fstream>
#include <iostream>
#include <string>
#include <sstream>

using namespace Rcpp;
using namespace std;

Rcpp::NumericMatrix make_mat(Rcpp::List input_list){

  unsigned int n = input_list.length();

  if(n == 0) { 
    Rcpp::stop("Must supply a list with more than 1 element.");
  }

  Rcpp::NumericVector testvals = input_list[0];
  unsigned int elems = testvals.length();

  Rcpp::NumericMatrix result_mat = Rcpp::no_init(n, elems);

  for(unsigned int i = 0; i < n; i++) {
    Rcpp::NumericVector row_val = input_list[i];

    if(elems != row_val.length()) {
      Rcpp::stop("Length of row does not match matrix requirements"); 
    }

    result_mat(i, Rcpp::_) = row_val;
  }

  return result_mat;
}

// [[Rcpp::export]]
List c_read_dat(std::string dat, int NC, std::string leading) {
    int i;
    std::string line, val;
    std::ifstream fp(dat.c_str());

    List dataSets, one_run;
    NumericVector row(NC);

    while (std::getline(fp, line)) {
        if (line.find(leading) == 0) {
            if (one_run.size() != 0) {
                
                dataSets.push_back(make_mat(one_run));
            }
            one_run = Rcpp::List::create();
            continue;
        }
           
        std::stringstream ss(line);
        i = 0;
        for (i = 0; i < NC; ++i) {
            std::getline(ss, val, ' ');
            row[i] = std::stod(val);
            ++i;
        }
        one_run.push_back(Rcpp::clone(row));
    }
    dataSets.push_back(make_mat(one_run));
    return dataSets;
}
