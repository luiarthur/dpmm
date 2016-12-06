//class GibbsState {
//  public: 
//    virtual GibbsState* update() const = 0;
//}
//
//class State : public GibbsState {
//  public:
//    virtual State* update() const;
//    State(int i_in);
//    int i;
//}
//
//State::State(int i_in) {
//  i = i_in;
//}
//
//State* State::update() const {
//  return new State(i+1);
//}
//
//auto s1 = new State(10);
//auto *s2 = new State(10);
//auto &s3 = *(new State(10));
//auto s4 = *(new State(10));
//
//s1->i
//s2->i
//s3.i
//s4.i
//s3.update()->i
//(*(s4.update())).i


#include<vector>

class GibbsState {
  public: 
    virtual GibbsState* update() const = 0;
    double cool() { return 100; };
    virtual std::vector<GibbsState*> sample(int B, int burn, int printEvery) {
      std::vector<GibbsState*> out;
      out.reserve(B);

      for (int i=0; i<B; i++) {
        out[i] = this;
      }

      return out;
    };
}

class State : public GibbsState {
  public:
    double p;
    State(int p_in) {p = p_in;};
    virtual State* update() const {
      return new State(p+2);
    };
}

auto s1 = new State(10);
s1->p
auto s2 = s1->update();
s2->p = 20
s1->p
s2->p

auto x = s1->sample(10,3,0)
