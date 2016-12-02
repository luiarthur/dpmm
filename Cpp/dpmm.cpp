#include <Rcpp.h>

using namespace Rcpp;

//[[Rcpp::export]]
double logit(double p) {
  return log(p / (1-p));
}

//[[Rcpp::export]]
double invLogit(double x) {
  return 1 / (1 + exp(-x));
}

double metropolis(double curr, double(*ll)(double), double(*lp)(double), double stepSig)
{
  double cand = R::rnorm(curr,stepSig);
  double u = R::runif(0,1);
  double out;

  if (ll(cand) + lp(cand) - ll(curr) - lp(curr) > log(u)) {
    out = cand;
  } else {
    out = curr;
  }

  return out;
}
