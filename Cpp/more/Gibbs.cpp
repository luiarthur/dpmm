class GibbsState {
  public: 
    virtual GibbsState* update() const = 0;
}

class State : public GibbsState {
  public:
    virtual State* update() const;
    State(int i_in);
    int i;
}

State::State(int i_in) {
  i = i_in;
}

State* State::update() const {
  return new State(i+1);
}

auto s1 = new State(10);
auto *s2 = new State(10);
auto &s3 = *(new State(10));
auto s4 = *(new State(10));

s1->i
s2->i
s3.i
s4.i
s3.update()->i
(*(s4.update())).i
