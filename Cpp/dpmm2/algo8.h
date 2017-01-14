#include "metropolis.h"

using namespace Rcpp;

int wsample_index(double p[], int n) {
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

  // create a map of unique t's
  std::map<double,int> map_t_count;
  for (int i=0; i<n; i++) {
    if (map_t_count.find( t[i] ) != map_t_count.end()) 
    { // if key exists
      map_t_count[t[i]]++;
    } else {
      map_t_count[t[i]] = 1;
    }
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

    newT[i] = unique_t[wsample_index(prob,K)];
    if (map_t_count.find( newT[i] ) != map_t_count.end()) {
      map_t_count[newT[i]]++;
    } else {
      map_t_count[newT[i]] = 1;
    }
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


