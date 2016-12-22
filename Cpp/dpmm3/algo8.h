#include<Rcpp.h>
#include<functional> // std::function
#include<map>

using namespace Rcpp;

// Enable C++11 via this plugin (Rcpp 0.10.3 or later)
// [[Rcpp::plugins("cpp11")]]

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
