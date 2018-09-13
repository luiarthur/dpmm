import org.scalatest.FunSuite
class TestSuite extends FunSuite {

  import dpmm.util._
  import dpmm.mcmc._
  import math.{log,pow}

  // start R
  //val R = org.ddahl.rscala.callback.RClient()
  //R eval """
  //require('devtools')
  //if ( !("rcommon" %in% installed.packages()) ) {
  //  devtools::install_github('luiarthur/rcommon')
  //}
  //library(rcommon)
  //"""

  Rand.reSeed(1)
  test("MH for Binomial data") {
    val pTruth = 0.6
    val N = 100
    val M = 100
    val x = Vector.fill(N)(Rand.nextBinomial(M,pTruth).toInt)

    case class State(val p:Double) extends Gibbs.State {
      def logPrior(p:Double) = 0.0
      def logLike(p:Double) = 
        Vector.tabulate(N)(i=>x(i)*log(p)+(M-x(i))*log(1-p)).sum

      def update() = 
        new State(MH.metLogit(p, logLike, logPrior, candSig=.1))
        //new State(MH.metropolis(p, logLike, logPrior, candSig=0.03))
    }

    val init = new State(.5)
    val samples = timer{ init.sample(B=2000,burn=10000,printEvery=1000) }
    val pSim = samples.map(_.p)

    println("\nOutput for Binomial Simulation")
    println("Acceptance:     " + pSim.distinct.length/pSim.length.toDouble)
    println("Truth:          " + pTruth)
    println("Posterior Mean: " + round(pSim.sum / pSim.length,4)+"\n")

    //R.p = pSim.toArray
    //R eval " pdf('src/test/output/binom.pdf') "
    //R eval " plotPost(p) "
    //R eval " dev.off() "

    assert(math.abs(pSim.sum/pSim.length - pTruth) < 1E-1)
  }

  def simParam(setParam: Set[Double], n: Int): Vector[Double] = {
    val J = setParam.size
    val uParam = setParam.toVector
    val out = Vector.fill(n)(uParam(Rand.nextInt(0,J-1))) 
    if (out.toSet == setParam) out else simParam(setParam, n)
  }
  test("DP Binomial Test") {
    println("Doing DP Binomial Test")
    val N = 100
    val M = 100
    val vTruth = simParam(Set(.1,.5,.9), N).sorted
    val x = Vector.tabulate(N)(i => Rand.nextBinomial(M,vTruth(i)))

    class State(val v:Vector[Double]) extends Gibbs.State {
      def update() = {
        def logf(vi:Double,i:Int) = {
            x(i) * log(vi) + (M-x(i)) * log(1-vi)
        }
        def logg0(vi:Double) = 0.0
        def rg0() = Rand.nextUniform(0,1)

        new State(Neal.algo8(alpha=1, v, logf, logg0, rg0, cs=.3, 
                             mh=MH.metLogit))
      }
    }

    val init = new State(Vector.fill(N)(.5))
    val out = timer {init.sample(B=2000,burn=10000,printEvery=1000)}
    val v = out.map(_.v.toArray).toArray
    println("acc v1: "+v.map(_.head).distinct.length.toDouble / out.length)

    //R.v = v
    //R.vTruth = vTruth.toArray
    //R.numClus = v.map( vt => vt.distinct.length )
    //R.x = x.toArray
    //R.M = M.toInt
    //R eval """
    //pdf("src/test/output/plots.pdf")

    //#par(mfrow=c(1,3))
    //#plot(numClus, main="Number of Clusters")

    //plot(vTruth,pch=20,ylim=c(0,1),main='v',
    //     col='grey30',fg='grey',ylab='')
    //points(apply(v,2,mean),col='blue',pch=20)
    //add.errbar(t(apply(v,2,quantile,c(.025,.975))),col='blue',lwd=.5)
    //points(x/M, col='red',pch=20,cex=.5)

    //minor <- function() plot(v[,1],col='grey',type='l',bty='n',axes=F,xlab='',ylab='')
    //plotInPlot(minor,'topleft')

    //#plot(v[,ncol(v)],col=rgb(.5,.5,.5,.3),type='l',
    //#     ylim=c(0,1),fg='grey',main='trace plot for v_100')

    //#par(mfrow=c(1,1))
    //dev.off()
    //"""
  }

  test("DP Normal Test") {
    val N = 30
    val muTruth = simParam(Set(.5,5.0), N).sorted
    val x = Vector.tabulate(N)(i => Rand.nextGaussian(muTruth(i),.5))

    class State(val mu:Vector[Double]) extends Gibbs.State {
      val sd = 10.0
      def update() = {
        def logf(mui:Double,i:Int) = 
          -pow(x(i) - mui,2) / 2.0
        def logg0(mui:Double) = -mui*mui / (2.0 * sd*sd)
        def rg0() = Rand.nextGaussian(0,sd)

        new State(Neal.algo8(alpha=1, mu, logf, logg0, rg0, cs=1, 
                             mh=MH.metropolis))
      }
    }

    val init = new State(Vector.fill(N)(0))
    val out = timer {init.sample(B=2000,burn=10000,printEvery=100)}
    val mu = out.map(_.mu.toArray).toArray
    println("acc mu: "+mu.map(_.head).distinct.length.toDouble / out.length)
    //println(mu.map(_.head).distinct.toVector)

    //R.x = x.toArray
    //R.mu = mu
    //R.muTruth = muTruth.toArray
    //R.numClus = mu.map( mut => mut.distinct.length )
    //R eval """
    //pdf("src/test/output/plotMu.pdf")

    //#par(mfrow=c(1,2))
    //#plot(numClus, main="Number of Clusters")

    //plot(x)
    //points(muTruth,pch=20, col='grey30',fg='grey',ylab='')
    //points(apply(mu,2,mean),lwd=2,col='blue',cex=1.3)
    //add.errbar(t(apply(mu,2,quantile,c(.025,.975))),co=rgb(0,0,1,.2))

    //#plot(mu[,ncol(mu)],col=rgb(.5,.5,.5,.3),type='l',
    //#     fg='grey',main='trace plot for mu')

    //#par(mfrow=c(1,1))
    //dev.off()
    //"""
  }


}
