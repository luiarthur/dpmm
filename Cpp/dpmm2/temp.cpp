#include <functional>

template <class D>
class GibbsState {
  D data;
  virtual GibbsState update() = 0;
  virtual GibbsState sample(int B, int burn, int printEvery) = 0;
};
