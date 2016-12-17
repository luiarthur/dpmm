package dpmm.mcmc

object Gibbs {

  abstract class State { 
    def update(): State
    def sample(B:Int, burn:Int, printEvery:Int=0) = {
      def loop(S:List[State], i:Int): List[State] = {
        if (printEvery > 0 && i % printEvery == 0) 
          print("\rProgress: " + i +"/"+ (B+burn) + "\t")

        if (i < B + burn) {
          val newState = if (i <= burn) 
            List(S.head.update)
          else
            S.head.update :: S

          loop(newState, i+1)
        } else S
      }
      loop(List(this),0).asInstanceOf[List[this.type]]
    }
  }

}
