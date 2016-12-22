#include<vector>
#include<iostream>
#include<functional>

template <typename S>
std::vector<S> gibbs(S init, std::function<S(S)> update, 
                     int B, int burn, int print_every) {

  std::vector<S> out(B);
  out[0] = init;

  for (int i=0; i<B+burn; i++) {
    if (i <= burn) {
      out[0] = update(out[0]);
    } else {
      out[i-burn] = update(out[i-burn-1]);
    }

    if (print_every > 0 && (i+1) % print_every == 0) {
      std::cout << "\rProgress:  " << i+1 << "/" << B+burn << "\t";
    }
  }

  if (print_every > 0) { std::cout << std::endl; }

  return out;
}

/* test:
#include "gibbs.h"

struct State { std::vector<double> v; }
State init;

const int N = 10;
std::vector<double> init_v(N);
init.v = init_v;

State update (State s_old) {
  std::vector<double> v_new(N);

  for (int i=0; i<N; i++) {
    v_new[i] = s_old.v[i] + 1;
  }

  return State{v_new};
}

auto out = gibbs<State>(init, update, 100, 10, 0);
auto out2 = gibbs<State>(init, update, 100, 0, 0);

 */
