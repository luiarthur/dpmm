package dpmm

package object util {
  val Rand = new org.apache.commons.math3.random.RandomDataGenerator()
  def reSeed(s: Long) = Rand.reSeed(s)
  def rig(shp: Double, rate: Double) = 1.0 / Rand.nextGamma(shp, 1.0/rate)

  def timer[R](block: => R) = {
    val t0 = System.nanoTime()
    val result = block
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) / 1E9 + "s")
    result
  }

  // weighted sampling
  def wsample(x: Vector[Double], p: Vector[Double]) = {
    require(p.min>=0)
    require(p.length == x.length)
    val sump = p.sum
    val rescaledP = if (sump == 1) p else p.map(pi => pi / sump)
    val u = Rand.nextUniform(0,1)
    val cumP = rescaledP.scanLeft(0.0)(_+_).tail
    x.view.zip(cumP).dropWhile(_._2<u).head._1
  }

  def round(x: Double, d: Int=4) = {
    val s = math pow (10, d)
    (math round x * s) / s
  }
}
