package dpmm.mcmc

import dpmm.util.wsample

object Neal {
  def algo8(alpha: Double, t: Vector[Double],
    logf: (Double,Int)=>Double, 
    logg0: Double=>Double, rg0: ()=>Double,
    mh:(Double,Double=>Double,Double=>Double,Double)=>Double=MH.metropolis, 
    cs: Double, clusterUpdates:Int) = { // assumes m = 1

    def f(x:Double,i:Int) = math.exp(logf(x,i))
    val n = t.length
    def removeAt(i: Int, x: Vector[Double]) = {
      val sa = x.splitAt(i)
      sa._1 ++ sa._2.tail
    }

    // check this FIXME
    def updateAt(i: Int, t: Vector[Double]): Vector[Double] = {
      if (i == n) t else {
        val tMinus = removeAt(i,t)
        val mapUT = tMinus.groupBy(identity).mapValues(_.length).toVector
        val aux = if ( tMinus.contains(t(i)) ) rg0() else t(i)
        val probExisting = mapUT.map(ut => ut._2 * f(ut._1,i))
        val pAux = alpha * f(aux, i)
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

        def ll(v:Double) = idx.map( logf(v,_) ).sum
        def lp(v:Double) = 0.0

        def loop(v:Double,it:Int):Double = 
          if (it==0) v else MH.metropolis(v,ll,lp,cs)
          //if (it==0) v else MH.metLogit(v,ll,lp,cs) 

        val newVal = loop(curr,clusterUpdates)

        idx.foreach { i => out(i) = newVal }
      }

      out.toVector 
    }
  
    updateClusters(updateAt(0,t))
  }
}
