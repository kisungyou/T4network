#include "RcppArmadillo.h"

// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;
using namespace arma;


/* ROUTINES
 * (01) cpp_effective : partial routine to compute effective resistance
 */

 
// (01) cpp_effective : partial routine to compute effective resistance ========
// [[Rcpp::export]]
arma::mat cpp_effective(arma::mat &X){
  // parameter and output
  int N = X.n_rows;
  arma::mat output(N,N,fill::zeros);
  
  // iteration
  double theval = 0.0;
  for (int k=0;k<N;k++){
    for (int j=(k+1);j<N;j++){
      theval = X(k,k)+X(j,j)-2.0*X(k,j);
      output(k,j) = theval;
      output(j,k) = theval;
    }
  }
  
  // return
  return(output);
}