#include<iostream>
#include<vector>
#include "gibbs.h"

class State {
  public:

    double mu;

    State(double m) {mu = m;};

    virtual State* update() const {
      return new State(mu + 1);
    };
}

auto s = new State(10);
s->update()->mu
auto x = gibbs(s, 10, 4, 1)

