#include "mcmc.h"
// want to do all this most efficiently without NumericVector's. 
// Just vectors. then make wrapper for R.

struct State { std::vector<double> v; };

//[[Rcpp::export]]
NumericMatrix fit(NumericVector y, NumericVector m, double alpha, 
                  double cs, int B, int burn, int printEvery) {

  auto update = [alpha, cs, y, m] (State& s_old, State& s_new) {
    auto lf = [y,m](double p, int i) {
      return y[i]*log(p)+(m[i]-y[i])*log(1-p);
    };
    auto lg0 = [](double p){return 0.0;};
    auto rg0 = [](){return R::runif(0,1);};

    algo8(alpha, s_old.v, s_new.v, cs, lf, lg0, rg0, metLogit);
  };

  std::vector<double> init_v(y.size(), 0.5);
  auto init = State{ init_v };

  NumericMatrix ret_out(y.size(),B);
  auto samps = gibbs<State>(init, update, B, burn, printEvery);

  for (int i=0; i<B; i++) {
    NumericVector col( samps[i].v.begin(), samps[i].v.end() );
    ret_out(_,i) = col;
  }

  return ret_out;
}
