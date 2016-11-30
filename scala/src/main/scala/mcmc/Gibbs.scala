package dpmm.mcmc

object Gibbs {
  abstract class State{ def update: State }

  // For large B, print out results instead of storing a list.
  def sample[T <: State](init: T, B: Int, burn: Int, printEvery: Int = 10) = {
    def loop(S: List[T], i: Int): List[T] = {
      if (i % printEvery == 0) print("\rProgress: " + i +"/"+ (B+burn) + "\t")
      if (i < B + burn) {
        val newState = S.head.update.asInstanceOf[T] :: S
        loop(newState, i+1)
      } else S
    }
    loop(List(init),0).take(B)
  }

}
