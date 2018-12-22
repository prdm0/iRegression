#include <RcppArmadillo.h>
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
List modmEst_cpp(StringVector variables1, StringVector variables2, DataFrame data) {
  
  //int size_string_formula1 = variables1.size();
  //int size_string_formula2 = variables2.size();
  
  String a = variables1[0];
  String b = variables2[0];
  
  arma::vec ymin = data[a];
  arma::vec yup = data[b];
  
  String c = variables1[1];
  String d = variables2[1];
  
  arma::vec xmin = data[c];
  arma::vec xup = data[d];
  
  arma::vec x_range = (xup - xmin)/2.;
  arma::vec y_range = (yup - ymin)/2.;

  double s0 = min(y_range/x_range);

  arma::vec x_center = (xup + xmin)/2.;
  arma::vec y_center = (yup + ymin)/2.;

  double alpha = as_scalar(cov(x_center, y_center)/var(x_center));

  double beta = as_scalar(cov(x_range, y_range)/var(x_range));

  if(beta < 0.) beta = 0.;
  if(beta < s0) beta = beta;

  double Bmin = (mean(y_center) - alpha * mean(x_center)) - (mean(y_range) - beta * mean(x_range));
  double Bmax = (mean(y_center) - alpha * mean(x_center)) + (mean(y_range) - beta * mean(x_range));

  double gamma = (Bmax + Bmin)/2.;
  double delta = (Bmax - Bmin)/2.;

  arma::vec Ymin_est = alpha * x_center + beta * x_range - (gamma - delta);
  arma::vec Ymax_est = alpha * x_center + beta * x_range + (gamma - delta);

  return List::create(_["Ymin_est"] = as<std::vector<double>>(wrap(Ymin_est.t())),
                      _["Ymax_est"] = as<std::vector<double>>(wrap(Ymax_est.t())),
                      _["alpha"] = alpha,
                      _["beta"] = beta,
                      _["Bmin"] = Bmin,
                      _["Bmax"] = Bmax,
                      _["gamma"] = gamma,
                      _["delta"] = delta);
}

