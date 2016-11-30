package dpmm.mcmc

import dpmm.util.Rand

object MH {
  // metropolis step with normal random walk
  def metropolis(curr:Double,logLikePlusLogPrior:Double=>Double,candSig:Double) = {
    val cand = Rand.nextGaussian(curr,candSig)
    val u = math.log(Rand.nextUniform(0,1))
    val p = logLikePlusLogPrior(cand) - logLikePlusLogPrior(curr)
    if (p > u) cand else curr
  }

  // metropolis step on logit-transformed var with normal random walk
  def metLogit(curr:Double,ll:Double=>Double,lp:Double=>Double,candSig:Double) = {
    // curr should be between 0 and 1
    
    def logit(p:Double) = math.log(p / (1-p))
    def invLogit(x:Double) = 1.0 / (1.0 + math.exp(-x))

    def logLikePlusLogLogitPrior(logitP: Double) = {
      val p = invLogit(logitP)
      val logJ = -logitP + 2*math.log(p)
      val logPriorLogitP = lp(p) + logJ 

      ll(p) + logPriorLogitP
    }

    invLogit(metropolis(logit(curr), logLikePlusLogLogitPrior, candSig))
  }
}
