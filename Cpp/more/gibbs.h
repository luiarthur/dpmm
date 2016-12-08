template <typename S>
std::vector<S*> gibbs(S* init, int B, int burn, int printEvery) {
  std::vector<S*> out(B);
  out[0] = init;

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
