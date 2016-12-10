#include "algo8.h"
#include "gibbs.h"

using namespace Rcpp;

//[[Rcpp::export]]
NumericMatrix fit(NumericVector y, NumericVector m, double alpha, double cs, int B, int burn, int printEvery) {

  NumericMatrix out(y.size(),B);
  out(_,0) = NumericVector(y.size(),0.5);

  class State {
    public:
      NumericVector v;
      State(NumericVector v_in) {v = v_in;};

      std::function<double(double,int)> lf = [y,m](double p, int i) {
        return y[i]*log(p)+(m[i]-y[i])*log(1-p);
      };
      std::function<double(double)> lg0 = [](double p){return 0.0;};
      std::function<double()> rg0 = [](){return R::runif(0,1);};

      // this won't work...
      //virtual State* update() const {
      //  return new State(algo8(alpha, v, lf, lg0, rg0, metLogit, cs));
      //};
  };

  //std::vector<double> v(y.size(), 0.5);
  //auto init = new State(v);
  //auto gibbs_out = gibbs(init, B, burn, printEvery);

  // need to change gibbs_out to out

  return out;
}

/*
#include<functional>
double f(double x) {
  class State {
    public:
      double y;
      std::function<double(double)> f1 = [y](double x)->double { return y; };
  };

  State s;
  s.y = 1.0;

  return s.f1();
}
 */
