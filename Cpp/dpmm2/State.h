#include "algo8.h"

class State {
  public:

    std::vector<double> v;
    State(std::vector<double> v_in) {v = v_in;};

    virtual State* update() const {
      algo8(alpha, v, )
    };

  private:
    static auto alpha = 1.0;
    static auto lf = [y,m](double p, int i) {
      return y[i]*log(p)+(m[i]-y[i])*log(1-p);
    };
    static auto lg0 = [](double p){ return 0.0; };
    static auto rg0 = [](){ return R::runif(0,1); };
};
