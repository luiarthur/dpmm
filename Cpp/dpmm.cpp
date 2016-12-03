#include<Rcpp.h>
#include<functional> // std::function
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
  const int n = t.size();
  std::vector<double> newT(t.begin(), t.end());

  // update each element in t
  for (int i=0; i<n; i++) {
    auto tMinus = newT; 
    tMinus.erase(tMinus.begin()+i);

    // create a map of unique t's
    std::map<double,int> map_t_count;
    for (int j=0; j<(n-1); j++) {
      if (map_t_count.find( tMinus[j] ) != map_t_count.end()) 
      { // if key exists
        map_t_count[tMinus[j]]++;
        std::cout << map_t_count[tMinus[j]] <<std::endl;
      } else {
        map_t_count[tMinus[j]] = 1;
      }
    }

    //for (int j=0; j<v.size(); j++) {
    //  if (m.find( v[j] ) != m.end()) {
    //    m[v[j]] = m[v[j]] + 1;
    //  } else {
    //    m[v[j]] = 1;
    //  }
    //}
    
    double aux;
    if (map_t_count.find(newT[i]) != map_t_count.end()) {
      aux = rg0();
    } else {
      aux = newT[i];
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

    newT[i] = unique_t[wsample_index(prob,K)];
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

  return NumericVector(newT.begin(), newT.end());
}

// need a wrapper for R. Then, test speed.
//[[Rcpp::export]]
NumericMatrix fit(NumericVector y, NumericVector m, double alpha, double cs, int B, int burn, int printEvery) {

  NumericMatrix out(y.size(),B);
  out(_,0) = NumericVector(y.size(),0.5);

  auto lf = [y,m](double p, int i) {return y[i]*log(p), (m[i]-y[i])*log(1-p);};
  auto lg0 = [](double p){return 0.0;};
  auto rg0 = [](){return R::runif(0,1);};

  for (int i=0; i<B+burn; i++) {
    if (i <= burn) {
      out(_,0) = algo8(alpha,out(_,0),lf,lg0,rg0,metLogit,cs);
    } else {
      out(_,i-burn) = algo8(alpha,out(_,i-burn-1),lf,lg0,rg0,metLogit,cs);
    }
  }

  return out;
}
