#include<iostream>
#include<vector>
#include<algorithm>
#include "map.h"

class GibbsState {
  public:
    virtual GibbsState* update() const = 0;

    virtual std::vector<GibbsState*> sample(int B, int burn, int printEvery) {
      std::vector<GibbsState*> out(B);
      out[0] = this;

      for (int i=0; i<B+burn; i++) {
        if (i <= burn) {
          out[0] = out[0]->update();
        } else {
          out[i-burn] = out[i-burn-1]->update();
        }

        if (printEvery > 0 && i % printEvery == 0) {
          std::cout << "\rProgress:  " << i << "/" << B+burn << "\t";
        }
      }
      return out;
    };
}

class State : public GibbsState {
  public:

    double mu;

    State(double m) {mu = m;};

    virtual State* update() const {
      return new State(mu + 1);
    };
}


auto s = new State(10);
s->update()->mu
auto x = s->sample(10,5,1)
static_cast<State*>(x[0]) -> mu

auto y = static_cast<State*>(x[0])

std::vector<State*> v(x.size())
for (int i=0; i<x.size(); i++) { v[i] = static_cast<State*>(x[i]); }


auto z = map(x, [](GibbsState* xi) { return static_cast<State*>(xi); })


