import org.scalatest.FunSuite
class TestSuite extends FunSuite {

  import dpmm.util._
  import dpmm.mcmc._
  import math.{log,pow}

  // start R
  val R = org.ddahl.rscala.callback.RClient()
  R eval """
  require('devtools')
  if ( !("rcommon" %in% installed.packages()) ) {
    devtools::install_github('luiarthur/rcommon')
  }
  library(rcommon)
  """

  test("MH for Binomial data") {
    Rand.reSeed(1)
    val pTruth = 0.6
    val N = 100
    val M = 100
    val x = Vector.fill(N)(Rand.nextBinomial(M,pTruth).toInt)

    case class State(val p:Double) extends Gibbs.State {
      val cs = .1
      def logPrior(p:Double) = 0.0
      def logLike(p:Double) = 
        Vector.tabulate(N)(i=>x(i)*log(p)+(M-x(i))*log(1-p)).sum

      def update() = 
        new State(MH.metLogit(p, logLike, logPrior, cs))
    }

    val init = new State(.5)
    val samples = timer{ init.sample(B=10000,burn=1000,printEvery=1000) }
    val pSim = samples.map(_.p)

    println("\nOutput for Binomial Simulation")
    println("Acceptance:     " + pSim.distinct.length/pSim.length.toDouble)
    println("Truth:          " + pTruth)
    println("Posterior Mean: " + round(pSim.sum / pSim.length,4)+"\n")

    //R.p = pSim.toArray
    //R eval " plotPost(p) "

    assert(math.abs(pSim.sum/pSim.length - pTruth) < 1E-2)
  }

  test("DP Test") {
    def simV(setV: Set[Double], n: Int): Vector[Double] = {
      setV.foreach{ uv => require(uv>0 && uv < 1) }
      val J = setV.size
      val uv = setV.toVector
      val out = Vector.fill(n)(uv(Rand.nextInt(0,J-1)))
      if (out.toSet == setV) out else simV(setV, n)
    }

    Rand.reSeed(1)
    val N = 100
    val M = 100
    val vTruth = simV(Set(.1,.5,.9), N)
    val x = Vector.tabulate(N)(i => Rand.nextBinomial(M,vTruth(i)))

    class State(val v:Vector[Double]) extends Gibbs.State {
      def update() = {
        def logf(vi:Double,i:Int) = log(x(i)) * vi + log(M-x(i)) * (1-vi)
        def logg0(vi:Double) = 0.0
        def rg0() = Rand.nextUniform(0,1)

        new State(Neal.algo8(alpha=0.001, v, logf, logg0, rg0, cs=0.1, 
                             clusterUpdates=10))
      }
    }

    val init = new State(Vector.fill(N)(.5))
    val out = timer {init.sample(B=2000,burn=10000,printEvery=100)}
    val v = out.map(_.v.toArray).toArray

    R.v = v
    R.vTruth = vTruth.toArray
    R.numClus = v.map( vt => vt.distinct.length )
    R eval """
    pdf("src/test/output/plots.pdf")
    ord <- order(vTruth)

    par(mfrow=c(1,3))
    plot(numClus, main="Number of Clusters")

    plot(vTruth[ord],pch=20,ylim=c(0,1),main='v',
         col='grey30',fg='grey',ylab='')
    points(apply(v,2,mean)[ord],lwd=2,col='blue',cex=1.3)
    add.errbar(t(apply(v,2,quantile,c(.025,.975)))[ord,],co=rgb(0,0,1,.2))

    plot(v[,ord[ncol(v)]],col=rgb(.5,.5,.5,.3),type='l',ylim=c(0,1),fg='grey',main='trace plot for v_100')
    par(mfrow=c(1,1))
    dev.off()
    """
  }

}
