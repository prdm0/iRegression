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
List modm2(arma::vec xmin, arma::vec xup, arma::vec ymin, arma::vec yup) {
  arma::vec x_range = (xup - xmin)/2;
  arma::vec y_range = (yup - ymin)/2;

  double s0 = min(y_range/x_range);

  arma::vec x_center = (xup + xmin)/2;
  arma::vec y_center = (yup + ymin)/2;

  double alpha = as_scalar(cov(x_center, y_center)/var(x_center));

  double beta = as_scalar(cov(x_range, y_range)/var(x_range));

  if(beta < 0.) beta = 0.;
  if(beta < s0) beta = beta;

  double Bmin = (mean(y_center) - alpha*mean(x_center)) - (mean(y_range) - beta*mean(x_range));
  double Bmax = (mean(y_center) - alpha*mean(x_center)) + (mean(y_range) - beta*mean(x_range));

  double gamma = (Bmax + Bmin)/2.;
  double delta = (Bmax - Bmin)/2.;

  arma::vec Ymin_est = alpha * x_center + beta * x_range - (gamma - delta);
  arma::vec Ymax_est = alpha * x_center + beta * x_range + (gamma - delta);

  return List::create(Named("Ymin_est") = Ymin_est.t(),
                        Named("Ymax_est") = Ymax_est.t(),
                        Named("alpha") = alpha,
                        Named("beta") = beta,
                        Named("Bmin") = Bmin,
                        Named("Bmax") = Bmax,
                        Named("gamma") = gamma,
                        Named("delta") = delta);
}

