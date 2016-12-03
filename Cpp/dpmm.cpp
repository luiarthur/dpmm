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
  auto lp_logit = [lp](double logit_p) { // capture lp in []
    double p = invLogit(logit_p);
    double log_J = -logit_p + 2 * log(p);
    return lp(p) + log_J;
  };
  auto ll_logit = [ll](double logit_p) { return ll(invLogit(logit_p)); };
  return invLogit(metropolis(logit(curr), ll_logit, lp_logit, stepSig));
}

int wsample_index(double *p, int n) {
  int sum_of_weight = 0;
  for(int i=0; i<n; i++) {
     sum_of_weight += p[i];
  }
  int rnd = R::runif(0,sum_of_weight);
  for(int i=0; i<n; i++) {
    if(rnd < p[i])
      return i;
    rnd -= p[i];
  }
}

double wsample(double *x, double *p, int n) {
  return x[wsample_index(p,n)];
}

