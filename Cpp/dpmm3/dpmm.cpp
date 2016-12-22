#include<Rcpp.h>
#include<functional> // std::function
#include<ctime>
#include<map>

using namespace Rcpp;

// Enable C++11 via this plugin (Rcpp 0.10.3 or later)
// [[Rcpp::plugins("cpp11")]]


double logit(double p) {
  return log(p / (1-p));
}

double invLogit(double x) {
  return 1 / (1 + exp(-x));
}

double metropolis(double curr, std::function<double(double)> ll, std::function<double(double)> lp, double stepSig)
{
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

double metLogit(double curr, std::function<double(double)> ll, std::function<double(double)> lp, double stepSig)
{
  auto lp_logit = [lp](double logit_p) { // capture lp in []
    const double p = invLogit(logit_p);
    const double log_J = -logit_p + 2 * log(p);
    return lp(p) + log_J;
  };
  auto ll_logit = [ll](double logit_p) { return ll(invLogit(logit_p)); };
  return invLogit(metropolis(logit(curr), ll_logit, lp_logit, stepSig));
}


int wsample_index(double p[], int n) { // GOOD
  const double p_sum = std::accumulate(p, p+n, 0.0);
  const double u = R::runif(0,p_sum);

  int i = 0;
  double cumsum = 0;

  do {
    cumsum += p[i];
    i++;
  } while (cumsum < u);

  return i-1;
}

double wsample(double x[], double p[], int n) { // GOOD
  return x[wsample_index(p,n)];
}


void algo8(double alpha, double* t_old, double* t_new, int n, double cs,
           std::function<double(double,int)> lf,
           std::function<double(double)> lg0,
           std::function<double()> rg0,
           std::function<double(double,
                                std::function<double(double)>,
                                std::function<double(double)>,
                                double)> mh) {

  auto f = [lf](double x, int i){ return exp(lf(x,i)); };
  const int n = t.size();

  // create a map of unique t's
  std::map<double,int> map_t_count;
  for (int i=0; i<n; i++) {
    if (map_t_count.find( t_old[i] ) != map_t_count.end()) 
    { // if key exists
      map_t_count[t_old[i]]++;
    } else {
      map_t_count[t_old[i]] = 1;
    }
    t_new[i] = t_old[i];
  }

  // update each element in t
  for (int i=0; i<n; i++) {
    map_t_count[t_new[i]]--;

    double aux;
    if (map_t_count[t_new[i]] > 0) {
      aux = rg0();
    } else {
      aux = t_new[i];
      map_t_count.erase(t_new[i]);
    }

    double probAux = alpha * f(aux,i);

    const int K = map_t_count.size();
    double prob[K+1];
    double unique_t[K+1];
    
    prob[0] = probAux;
    unique_t[0] = aux;

    int k=1;
    for (auto const& ut : map_t_count) {
      prob[k] = ut.second * f(ut.first,i);
      unique_t[k] = ut.first;
      k++;
    }

    t_new[i] = unique_t[wsample_index(prob,K)];
    if (map_t_count.find( t_new[i] ) != map_t_count.end()) {
      map_t_count[t_new[i]]++;
    } else {
      map_t_count[t_new[i]] = 1;
    }
  }

  // update by cluster
  std::map<double,std::vector<int>> map_t_idx;
  for (int i=0; i<n; i++) {
    if (map_t_idx.find( t_new[i] ) != map_t_idx.end()) { // if key exists
      map_t_idx[new_t[i]].push_back(i);
    } else {
      map_t_idx[new_t[i]] = {i};
    }
  }
  for (auto const& ut : map_t_idx) {
    auto idx = ut.second;
    auto ll = [idx,lf](double tj) {
      double out = 0;
      for (int i=0; i<idx.size(); i++) { out += lf(tj,idx[i]); }
      return out;
    };
    auto new_tj = mh(ut.first, ll, lg0, cs);
    for (int i=0; i<idx.size(); i++) {
      t_new[idx[i]] = new_tj;
    }
  }
}

// want to do all this most efficiently without NumericVector's. Just vectors.
// then make wrapper for R.

struct State { double* v; }

std::vector<State> gibbs(State init, 
                         update std::function<void(State*, State*)>, 
                         int B, int burn, int burn, int every) {

  std::vector<State> out(B);
  out[0] = init;

  for (int i=0; i<B+burn; i++) {
    if (i <= burn) {
      update(&out[0], &out[0]);
    } else {
      update(&out[i-burn], &out[i-burn-1]);
    }

    if (every > 0 && (i+1) % every == 0) {
      Rcout << "\rProgress:  " << i+1 << "/" << B+burn << "\t";
    }
  }

  if (every >0) { Rcout << std::endl; }

  return out
}

//[[Rcpp::export]]
NumericMatrix fit(NumericVector y, NumericVector m, double alpha, double cs, int B, int burn, int printEvery) {


  auto update = [y,m] (State* s_old, State* s_new) {
    auto lf = [y,m](double p, int i) {return y[i]*log(p)+(m[i]-y[i])*log(1-p);};
    auto lg0 = [](double p){return 0.0;};
    auto rg0 = [](){return R::runif(0,1);};
    int n = y.size();

    algo8(alpha,s_old.v,s_new.v,n,lf,lg0,rg0,metLogit,cs);
  }

  State init = new State{ vector(y.size(),0.5) };

  // gibbs

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

std::vector<State*> gibbs(State* init, 
                         std::function<void(State*, State*)> update, 
                         int B, int burn) {

  std::vector<State*> out(B);
  out[0] = *int;

  for (int i=0; i<B+burn; i++) {
    if (i <= burn) {
      update(&out[0], &out[0]);
    } else {
      update(&out[i-burn], &out[i-burn-1]);
    }
  }

  return out;
}



std::vector<double> x(10)
*/
