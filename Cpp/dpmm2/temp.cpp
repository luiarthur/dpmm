#include <functional>
#include <vector>
#include <iostream>

template <class D>
class State {
  D data;
  State update() {return this;};
  std::vector<State> sample(int B, int burn, int printEvery) {
    std::vector<State> out(B);
    out[0] = this;

    for (int i=0; i<B+burn; i++) {
      if (i <= burn) {
        out[0] = out[0]->update();
      } else {
        out[i-burn] = out[i-burn-1]->update();
      }

      if (printEvery > 0 && (i+1) % printEvery == 0) {
        std::cout << "\rProgress:  " << i+1 << "/" << B+burn << "\t";
      }
    }

    std::cout << std::endl;
    return out;
  };
};

/*
#include "temp.cpp"
 */
