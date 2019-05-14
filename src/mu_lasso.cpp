// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

// we only include RcppArmadillo.h which pulls Rcpp.h in for us
#include "RcppArmadillo.h"

// via the depends attribute we tell Rcpp to create hooks for
// RcppArmadillo so that the build process will know what to do
//
// [[Rcpp::depends(RcppArmadillo)]]

double softThres(double a, double b);

// [[Rcpp::export]]
arma::vec mu_lasso(arma::vec omega, double gamma, arma::mat W, arma::vec z, arma::vec betaInit, bool activeSet){
  // We assume the first column of W is ones, which represent the intercept
  arma::vec beta = betaInit;
  arma::vec betaOld = betaInit;
  int p = W.n_cols;
  int n = W.n_rows;
  double epsilon = 1e-7, error = 10;
  double a,b, denom, numerator;
  int maxit = 1000;

  int i=0, j;
  while((i < maxit) & (error > epsilon)){
    j = 0;
    denom = 1.0/n*arma::sum(W.col(j) % (z - W*beta + W.col(j)*beta(j)));
    numerator = 1.0/n*arma::as_scalar(arma::sum(arma::pow(W.col(j),2)));
    beta(j) = denom/numerator;


    for(j=1; j<p; ++j){
      if(!activeSet || (beta(j) != 0) || (i % 30 == 0)){
        a = 1.0/n*arma::sum(W.col(j) % (z - W*beta + W.col(j)*beta(j)));
        b = omega(j);

        denom = softThres(a,b);

        numerator = 1.0/n*arma::as_scalar(sum(pow(W.col(j),2))) + 2*gamma;
        beta(j) = denom/numerator;
      }
    }
    // Check if user has pressed STOP or CTRL+C
    Rcpp::checkUserInterrupt();

    error = arma::norm(beta - betaOld);
    betaOld = beta;
    ++i;
  }
  if(i >= maxit){
    Rcpp::Rcout << "Coordinate descent did not converge.\n";
  }

  return beta;
}


double softThres(double a, double b){
  if( (a > 0) & (b < std::abs(a)) ){
    return a-b;
  } else if( (a < 0) & (b < std::abs(a)) ){
    return a+b;
  } else {
    return 0;
  }
}
