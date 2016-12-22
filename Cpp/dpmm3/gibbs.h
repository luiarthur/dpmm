#include<vector>
#include<Rcpp.h>
#include<functional>

using namespace Rcpp;

// Enable C++11 via this plugin (Rcpp 0.10.3 or later)
// [[Rcpp::plugins("cpp11")]]

template <typename S>
std::vector<S> gibbs(S init, std::function<void(S&,S&)> update, 
                     int B, int burn, int print_every) {

  std::vector<S> out(B);
  out[0] = init;

  for (int i=0; i<B+burn; i++) {
    if (i <= burn) {
      update(out[0], out[0]);
    } else {
      update(out[i-burn-1], out[i-burn]);
    }

    if (print_every > 0 && (i+1) % print_every == 0) {
      Rcout << "\rProgress:  " << i+1 << "/" << B+burn << "\t";
    }
  }

  if (print_every > 0) { Rcout << std::endl; }

  return out;
}

/* test:
#include "gibbs.h"

struct State { std::vector<double> v; }
State init;

const int N = 10;
std::vector<double> init_v(N);
init.v = init_v;

void update(State& s_old, State& s_new) {
  std::vector<double> v_new(N);
  for (int i=0; i<N; i++) {
    v_new[i] = s_old.v[i] + 1;
  }
  s_new = State{v_new};
}

auto out = gibbs<State>(init, update, 100, 10, 0);
auto out2 = gibbs<State>(init, update, 100, 0, 0);

 */
