#include<Rcpp.h>
#include<functional> // std::function
#include<map>

using namespace Rcpp;

// Enable C++11 via this plugin (Rcpp 0.10.3 or later)
// [[Rcpp::plugins("cpp11")]]


// Generic Gibbs Sampler
template <typename S>
void gibbs(S state, std::function<void(S&)> update, 
          std::function<void(const S&, int)> ass,
          int B, int burn, int print_every) {

  for (int i=0; i<B+burn; i++) {
    update(state);
    if (i > burn) {
      ass(state, i-burn-1);
    }

    if (print_every > 0 && (i+1) % print_every == 0) {
      Rcout << "\rProgress:  " << i+1 << "/" << B+burn << "\t";
    }
  }

  if (print_every > 0) { Rcout << std::endl; }
}

// Uniariate Metropolis step with Normal proposal
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

// Univariate Metropolis step with logit transform
double metLogit(double curr, std::function<double(double)> ll, 
                std::function<double(double)> lp, double stepSig) {

  auto logit = [](double p) { return log(p/(1-p)); };
  auto invLogit = [](double x) { return 1 / (1 + exp(-x)); };

  // capture invLogit,lp in []
  auto lp_logit = [invLogit,lp](double logit_p) {
    const double p = invLogit(logit_p);
    const double log_J = -logit_p + 2 * log(p);
    return lp(p) + log_J;
  };

  auto ll_logit = [invLogit,ll](double logit_p) { 
    return ll(invLogit(logit_p)); 
  };

  return invLogit(metropolis(logit(curr),ll_logit,lp_logit,stepSig));
}

// Weighted sampling: takes prob. array and size; returns index.
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

void algo8(double alpha, 
           std::vector<double> &t,
           double cs,
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
    map_t_count[t[i]]++;
  }

  // update each element in t
  for (int i=0; i<n; i++) {
    map_t_count[t[i]]--;

    double aux;
    if (map_t_count[t[i]] > 0) {
      aux = rg0();
    } else {
      aux = t[i];
      map_t_count.erase(t[i]);
    }

    double probAux = alpha * f(aux, i);

    const int K = map_t_count.size() + 1;
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

    t[i] = unique_t[wsample_index(prob,K)];
    map_t_count[t[i]]++;
  }

  // update by cluster
  // Create a map of unique values to indices
  std::map<double, std::vector<int>> map_t_idx;
  for (int i=0; i<n; i++) {
    map_t_idx[t[i]].push_back(i);
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
      t[idx[i]] = new_tj;
    }
  }
}
