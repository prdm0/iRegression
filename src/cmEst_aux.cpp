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
List cmEst_cpp(arma::mat x1, arma::mat x2, arma::vec y1, arma::vec y2) {
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
 
 return List::create(_["coefficients"] = as<std::vector<double>>(wrap(coef.t())),
                     _["vcov"] = vcov,
                     _["sigma"] = sqrt(sigma2),
                     _["df"] = df);
}