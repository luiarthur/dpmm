#include<Rcpp.h>
#include<functional> // std::function

using namespace Rcpp;

// Enable C++11 via this plugin (Rcpp 0.10.3 or later)
// [[Rcpp::plugins("cpp11")]]

double metropolis(double curr, std::function<double(double)> ll, 
                  std::function<double(double)> lp, double stepSig) {
  const double cand = R::rnorm(curr,stepSig);
  const double u = R::runif(0,1);
  double out;

  if (ll(cand) + lp(cand) - ll(curr) - lp(curr) > log(u)) {
    out = cand;
  } else {
    out = curr;
  }

  return out;
}

double metLogit(double curr, std::function<double(double)> ll, 
                std::function<double(double)> lp, double stepSig) {
  auto logit = [](double p) { return log(p/(1-p)); }
  auto invLogit = [](double x) { return 1 / (1 + exp(-x)); }
  auto lp_logit = [lp](double logit_p) { // capture lp in []
    const double p = invLogit(logit_p);
    const double log_J = -logit_p + 2 * log(p);
    return lp(p) + log_J;
  };
  auto ll_logit = [ll](double logit_p) { 
    return ll(invLogit(logit_p)); 
  };
  return invLogit(metropolis(logit(curr),ll_logit,lp_logit,stepSig));
}
