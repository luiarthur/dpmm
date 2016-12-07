#include<vector>
#include<iostream>

class State {
  public:
    std::vector<State*> sample(int B, int burn, int printEvery) {
      std::vector<State*> out;
      out.reserve(B);
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
    }
    // implement the following:
    double mu;
    State(double m) {mu = m;};
    State* update() { return new State(mu + 1); }
}

auto s = State(1)
auto x = s.sample(100,10,10)
