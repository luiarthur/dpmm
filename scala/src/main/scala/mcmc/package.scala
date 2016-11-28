package dpmm

package object mcmc {

  def metropolis(curr:Double, loglike_plus_prior:Double=>Double, 
                 candSig:Double) = {
    val cand = Rand.nextGaussian(curr,candSig)
    val u = math.log(Rand.nextUniform(0,1))
    val p = loglike_plus_prior(cand) - loglike_plus_prior(curr)
    if (p > u) cand else curr
  }

  def gibbs(init: State, prior: Prior, obs: Obs,
            B: Int, burn: Int, printEvery: Int = 10) = {

    def loop(S: List[State], i: Int): List[State] = {
      if (i % printEvery == 0) 
        print("\rProgress: " + i +"/"+ (B+burn) + "\t" + 
          java.util.Calendar.getInstance.getTime() + "\t")

      if (i < B + burn) {
        val newState = if (i <= burn) 
          List( S.head.update(prior,obs) )
        else 
          S.head.update(prior,obs) :: S
        
        loop(newState, i+1)
      } else S
    }

    lazy val out = loop(List(init),0)
    println()
    out
  }
 
  def neal8Update(alpha: Double, t: Vector[Double],
    logf: (Double,Int)=>Double, 
    logg0: Double=>Double, rg0: ()=>Double,
    cs: Double, clusterUpdates:Int) = { // assumes m = 1

    def f(x:Double,i:Int) = math.exp(logf(x,i))
    val n = t.length
    def removeAt(i: Int, x: Vector[Double]) = {
      val sa = x.splitAt(i)
      sa._1 ++ sa._2.tail
    }

    def updateAt(i: Int, t: Vector[Double]): Vector[Double] = {
      if (i == n) t else {
        val tMinus = removeAt(i,t)
        val mapUT = tMinus.groupBy(identity).mapValues(_.length).toVector
        val aux = rg0()
        val probExisting = mapUT.map(ut => ut._2 * f(ut._1,i) / (alpha + n -1))
        val pAux = alpha * f(aux, i) / (alpha + n - 1)
        val uT = mapUT.map(_._1)
        val newTi = wsample(uT :+ aux, probExisting :+ pAux)
        updateAt(i+1, t.updated(i,newTi))
      }
    }

    def updateClusters(t:Vector[Double]): Vector[Double] = {
      val out = Array.ofDim[Double](n)
      val tWithIndex = t.zipWithIndex

      t.distinct.foreach { curr =>
        val idx = tWithIndex.filter(_._1 == curr).map(_._2)

        def logLikePlusLogPriorLogitV(logitV: Double) = {
          val v = invLogit(logitV)
          val logJ = -logitV + 2 * math.log(v)
          val logPriorLogitV = logJ // + logg0(v)
          val ll = idx.map(i => logf(v,i)).sum
          ll + logPriorLogitV
        }

        def loop(j:Int, logitV:Double): Double = 
          if (j==0) 
            logitV 
          else
            loop(j-1,metropolis(logitV,logLikePlusLogPriorLogitV,cs))

        val newVal = invLogit(loop(clusterUpdates, logit(curr)))

        idx.foreach { i => out(i) = newVal }
      }

      out.toVector 
    }
  
    updateClusters(updateAt(0,t))
  }

}
