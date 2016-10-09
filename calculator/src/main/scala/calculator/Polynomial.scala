package calculator

object Polynomial {

  def computeDelta(a: Signal[Double], b: Signal[Double], c: Signal[Double]): Signal[Double] = {
    Signal{
      Math.pow(b(),2) - 4*a()*c()
    }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double], c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal {
      val r1 = (-b() + Math.sqrt(computeDelta(a,b,c)())) / (2 * a())
      val r2 = (-b() - Math.sqrt(computeDelta(a,b,c)())) / (2 * a())
      Set[Double](r1, r2)
    }
  }
}
