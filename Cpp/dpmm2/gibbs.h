#include<Rcpp.h>
#include<iostream>
#include<vector>

template <typename S>
std::vector<S*> gibbs(S* init, int B, int burn, int printEvery) {
  std::vector<S*> out(B);
  out[0] = init;

  for (int i=0; i<B+burn; i++) {
    if (i <= burn) {
      out[0] = out[0]->update();
    } else {
      out[i-burn] = out[i-burn-1]->update();
    }

    if (printEvery > 0 && (i+1) % printEvery == 0) {
      Rcout << "\rProgress:  " << i+1 << "/" << B+burn << "\t";
    }
  }

  Rcout << std::endl;
  return out;
}
