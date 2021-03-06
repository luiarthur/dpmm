#include<Rcpp.h>
#include<functional> // std::function
#include<ctime>
#include<map>

using namespace Rcpp;

// Enable C++11 via this plugin (Rcpp 0.10.3 or later)
// [[Rcpp::plugins("cpp11")]]

inline void printIt(int i, int every, int total) {
  if (every > 0 && (i+1) % every == 0) { \
    Rcout << "\rProgress:  " << i+1 << "/" << total << "\t";\
  } 
}

double logit(double p) {
  return log(p / (1-p));
}

double invLogit(double x) {
  return 1 / (1 + exp(-x));
}

double metropolis(double curr, std::function<double(double)> ll, std::function<double(double)> lp, double stepSig)
{
  double const cand = R::rnorm(curr,stepSig);
  double const u = R::runif(0,1);
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
    double const p = invLogit(logit_p);
    //const double log_J = -logit_p + 2*log(p); //???
    double const log_J = -logit_p + 2*log(1-p); //???
    return lp(p) + log_J;
  };
  auto ll_logit = [ll](double logit_p) { return ll(invLogit(logit_p)); };
  return invLogit(metropolis(logit(curr), ll_logit, lp_logit, stepSig));
}


int wsample_index(double p[], int n) { // GOOD
  double const p_sum = std::accumulate(p, p+n, 0.0);
  double const u = R::runif(0,p_sum);

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


NumericVector algo8(double alpha, NumericVector t, 
                    std::function<double(double,int)> lf,
                    std::function<double(double)> lg0,
                    std::function<double()> rg0,
                    std::function<double(double,
                                         std::function<double(double)>,
                                         std::function<double(double)>,
                                         double)> mh,
                    double cs) {
  auto f = [lf](double x, int i){ return exp(lf(x,i)); };
  auto const n = t.size();
  //std::vector<double> newT(t.begin(), t.end());
  auto newT = t;

  // create a map of unique t's
  std::map<double,int> map_t_count;
  for (auto const& ti : t) {
    map_t_count[ti]++;
  }

  // update each element in t
  for (int i=0; i<n; i++) {
    map_t_count[newT[i]]--;

    double aux;
    if (map_t_count[newT[i]] > 0) {
      aux = rg0();
    } else {
      aux = newT[i];
      map_t_count.erase(newT[i]);
    }

    double probAux = alpha * f(aux,i);

    int const K = map_t_count.size() + 1;
    double prob[K];
    double unique_t[K];
    
    prob[0] = probAux;
    unique_t[0] = aux;

    int k=1;
    for (auto const& ut : map_t_count) {
      prob[k] = ut.second * f(ut.first,i);
      unique_t[k] = ut.first;
      k++;
    }

    newT[i] = unique_t[wsample_index(prob,K)];
    map_t_count[ newT[i] ]++;
  }

  // update by cluster
  std::map<double,std::vector<int>> map_t_idx;
  for (int i=0; i<n; i++) {
    if (map_t_idx.find( newT[i] ) != map_t_idx.end()) { // if key exists
      map_t_idx[newT[i]].push_back(i);
    } else {
      map_t_idx[newT[i]] = {i};
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
      newT[idx[i]] = new_tj;
    }
  }

  return newT;//NumericVector(newT.begin(), newT.end());
}

//[[Rcpp::export]]
NumericMatrix fit(NumericVector y, NumericVector m, double alpha, double cs, int B, int burn, int printEvery) {

  NumericMatrix out(y.size(),B);
  out(_,0) = NumericVector(y.size(),0.5);

  auto lf = [&y,&m](double p, int i) {
    return y[i]*log(p)+(m[i]-y[i])*log(1-p);
  };
  auto lg0 = [](double p){return 0.0;};
  auto rg0 = [](){return R::runif(0,1);};

  // gibbs loop
  for (int i=0; i<B+burn; i++) {
    if (i <= burn) {
      out(_, 0) = 
        algo8(alpha, out(_, 0), lf, lg0, rg0, metLogit, cs);
    } else {
      out(_, i-burn) = 
        algo8(alpha, out(_, i-burn-1), lf, lg0, rg0, metLogit, cs);
    }

    printIt(i, printEvery, B+burn);
  }

  Rcout << std::endl;
  return out;
}
