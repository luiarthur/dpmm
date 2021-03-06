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
    val u = Rand.nextUniform(0,p.sum)
    def loop(i:Int=0, cumsum:Double=0.0): Double = {
      if (cumsum < u) loop(i+1, cumsum+p(i)) else x(i-1)
    }
    loop()
  }

  def round(x: Double, d: Int=4) = {
    val s = math pow (10, d)
    (math round x * s) / s
  }
}
