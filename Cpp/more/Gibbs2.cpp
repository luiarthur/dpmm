#include<vector>
#include<iostream>

class GibbsState { public: virtual GibbsState* update() const = 0; }
class State : public GibbsState {
  public:
    double p;
    State(int p_in) {p = p_in;};
    virtual State* update() const { return new State(p+2); };
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
}

auto s1 = new State(10);
s1->p
auto s2 = s1->update();
s2->p = 20
s1->p
s2->p

auto x = s1->sample(10,3,1)
x[0] -> p
x[9] -> p
