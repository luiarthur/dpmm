#include<algorithm>
template <typename Collection,typename unop>
  Collection map(Collection col,unop op) {
  std::transform(col.begin(),col.end(),col.begin(),op);
  return col;
}

