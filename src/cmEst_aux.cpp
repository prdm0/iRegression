#include <RcppArmadillo.h>
#include <string>
#include <vector>


using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
Rcpp::List cmEst_cpp(StringVector string_formula1, StringVector string_formula2, DataFrame data){
  
  int size_string_formula1 = string_formula1.size();
  int size_string_formula2 = string_formula2.size();
  
  String a = string_formula1[0];
  String b = string_formula2[0];
  arma::vec y1 = data[a];
  arma::vec y2 = data[b];
  
  string_formula1.erase(0);
  string_formula2.erase(0);
  
  DataFrame data_1 = data[string_formula1]; /* Explanatory variables of the model 1. */
  DataFrame data_2 = data[string_formula2]; /* Explanatory variables of the model 2. */

  arma::mat x1 = arma::ones(data_1.nrow(),1);
  arma::mat x2 = arma::ones(data_2.nrow(),1);
  
  for(int c = 0; c < data_1.ncol(); c++){
    arma::colvec data_acess_1 = data_1[c];
    x1.insert_cols(c + 1, data_acess_1);
    
    arma::colvec data_acess_2 = data_2[c];
    x2.insert_cols(c + 1, data_acess_2);
  }
  
  arma::mat xpm = (x1 + x2)/2.;
  arma::mat xh = x2 - x1;
  arma::vec ypm = (y1 + y2)/2.;
  arma::vec yh = y2 - y1;
  
  arma::mat Q, R;
  arma::qr_econ(Q, R, xpm);
  
  arma::vec coef = arma::inv(R) * Q.t() * ypm;
  
  int df = xpm.n_rows - xpm.n_cols;
  double sigma2 = sum(pow(ypm - xpm * coef, 2))/df;
  
  //Function chol2inv_r("chol2inv");  
  // return  sigma2 * chol2inv_r(R);
  arma::mat vcov = sigma2 * arma::inv(xpm.t() * xpm);
  
  arma::vec fitted_values_l = x1 * coef;
  arma::vec fitted_values_u = x2 * coef;
  
  return List::create(_["coefficients"] = as<std::vector<double>>(wrap(coef.t())),
                      _["vcov"] = vcov,
                      _["sigma"] = sqrt(sigma2),
                      _["df"] = df,
                      _["fitted.values.l"] = as<std::vector<double>>(wrap(fitted_values_l.t())),
                      _["fitted.values.u"] = as<std::vector<double>>(wrap(fitted_values_u.t())),
                      _["residuals.l"] = as<std::vector<double>>(wrap(y1.t() - fitted_values_l.t())),
                      _["residuals.u"] = as<std::vector<double>>(wrap(y2.t() - fitted_values_u.t()))); 
}
