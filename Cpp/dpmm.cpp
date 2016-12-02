#include<Rcpp.h>
#include<functional> // std::function

using namespace Rcpp;

// Enable C++11 via this plugin (Rcpp 0.10.3 or later)
// [[Rcpp::plugins("cpp11")]]


//[[Rcpp::export]]
double logit(double p) {
  return log(p / (1-p));
}

//[[Rcpp::export]]
double invLogit(double x) {
  return 1 / (1 + exp(-x));
}

/* This is the function pointer way. Not ideal because I can't write metLogit this way.
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
*/

// uses lambdas
double metropolis(double curr, std::function<double(double)> ll, std::function<double(double)> lp, double stepSig)
{
  const auto cand = R::rnorm(curr, stepSig);
  const auto u = R::runif(0,1);
  double out;

  if (ll(cand) + lp(cand) - ll(curr) - lp(curr) > log(u)) {
    out = cand;
  } else {
    out = curr;
  }

  return out;
}

double metLogit(double curr, std::function<double(double)> ll, std::function<double(double)> lp, double stepSig)
{
  auto logLogitPrior = [lp](double logit_p) { // capture lp in []
    double p = invLogit(logit_p);
    double log_J = -logit_p + 2 * log(p);
    return lp(p) + log_J;
  };
  return invLogit(metropolis(logit(curr), ll, logLogitPrior, stepSig));
}


