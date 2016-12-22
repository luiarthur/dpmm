// want to do all this most efficiently without NumericVector's. Just vectors.
// then make wrapper for R.

struct State { double* v; }

//[[Rcpp::export]]
NumericMatrix fit(NumericVector y, NumericVector m, double alpha, double cs, int B, int burn, int printEvery) {


  auto update = [y,m] (State& s_old, State& s_new) {
    auto lf = [y,m](double p, int i) {return y[i]*log(p)+(m[i]-y[i])*log(1-p);};
    auto lg0 = [](double p){return 0.0;};
    auto rg0 = [](){return R::runif(0,1);};
    int n = y.size();

    algo8(alpha,s_old.v,s_new.v,n,lf,lg0,rg0,metLogit,cs);
  }

  State init = new State{ vector(y.size(), 0.5) };
  auto out = gibbs(init, update, B, burn, printEvery);

  return out;
}

/*
#include<vector>
#include<iostream>
#include<functional>


struct State { std::vector<double> v; }
State init;

const int N = 10;
std::vector<double> init_v(N);
init.v = init_v;

State update(State s) { return s; }


std::vector<State> update2(State s) {
  std::vector<State> out(10);
  return out;
}



*/
