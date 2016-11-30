import org.scalatest.FunSuite
class TestSuite extends FunSuite {

  // start R
  val R = org.ddahl.rscala.callback.RClient()
  R eval """
  require('devtools')
  if ( !("rcommon" %in% installed.packages()) ) {
    devtools::install_github('luiarthur/rcommon')
  }
  library(rcommon)
  """

  import dpmm.util._
  test("MH for Binomial data") {
    import math.{log,pow}

    Rand.reSeed(1)
    val pTruth = 0.6
    val N = 100
    val M = 100
    val x = Vector.fill(N)(Rand.nextBinomial(M,pTruth).toInt)

    class State(val p:Double) extends dpmm.mcmc.Gibbs.State {
      val cs = .1
      def logPrior(p:Double) = 0.0
      def logLike(p:Double) = 
        Vector.tabulate(N)(i=>x(i)*log(p)+(M-x(i))*log(1-p)).sum

      def update() = 
        new State(dpmm.mcmc.MH.metLogit(p, logLike, logPrior, cs))
    }

    val init = new State(.5)
    val samples = timer{dpmm.mcmc.Gibbs.sample(init,10000,1000,1000)}

    val pSim = samples.map(_.p)
    //R.p = pSim.toArray
    //R eval " plotPost(p) "
    println("\nOutput for Binomial Simulation")
    println("Acceptance:     " + pSim.distinct.length/pSim.length.toDouble)
    println("Truth:          " + pTruth)
    println("Posterior Mean: " + round(pSim.sum / pSim.length,4)+"\n")
    assert(math.abs(pSim.sum/pSim.length - pTruth) < 1E-2)
  }
}
