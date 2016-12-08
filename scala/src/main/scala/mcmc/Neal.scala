package dpmm.mcmc

import dpmm.util.wsample

object Neal {
  def algo8(alpha: Double, t: Vector[Double],
    logf: (Double,Int)=>Double, 
    logg0: Double=>Double, rg0: ()=>Double,
    mh:(Double,Double=>Double,Double=>Double,Double)=>Double=MH.metropolis, 
    cs: Double) = { // assumes m = 1

    def f(x:Double,i:Int) = math.exp(logf(x,i))
    val n = t.length

    val mapUT = collection.mutable.Map[Double,Int]()
    t foreach { ti => if (mapUT.contains(ti)) mapUT(ti) += 1 else mapUT(ti) = 1 }

    def updateAt(i: Int, t: Vector[Double]): Vector[Double] = {
      if (i == n) t else {
        mapUT(t(i)) -= 1
        val aux = if (mapUT( t(i) ) > 0) rg0() else {
          mapUT.remove( t(i) )
          t(i)
        }
        //val probExisting = mapUT.map(ut => ut._2 * f(ut._1,i)).toVector
        val probExisting = mapUT.map(ut => ut._2 * f(ut._1,i)).toVector
        val pAux = alpha * f(aux, i)
        val uT = mapUT.keys.toVector
        val newTi = wsample(uT :+ aux, probExisting :+ pAux)

        if (mapUT.contains(newTi)) mapUT(newTi) += 1 else mapUT(newTi) = 1

        updateAt(i+1, t.updated(i,newTi))
      }
    }

    def updateClusters(t:Vector[Double]): Vector[Double] = {
      val out = Array.ofDim[Double](n)
      val tWithIndex = t.zipWithIndex

      //mapUT.keys.foreach { curr =>
      t.distinct.foreach { curr =>
        val idx = tWithIndex.filter(_._1 == curr).map(_._2)
        def ll(v:Double) = idx.map( logf(v,_) ).sum
        val newVal = mh(curr, ll, logg0, cs)

        idx.foreach { i => out(i) = newVal }
      }

      out.toVector 
    }
  
    updateClusters(updateAt(0,t))
  }
}
