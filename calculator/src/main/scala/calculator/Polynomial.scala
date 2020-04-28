package calculator

object Polynomial extends PolynomialInterface {
  def computeDelta(a: Signal[Double], b: Signal[Double],
                   c: Signal[Double]): Signal[Double] = {

    Signal {
      val b_val = b()
      (b_val * b_val) - (4 * a() * c())
    }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
                       c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal {
      val delta_val = delta()
      delta_val match {
        case 0 => Set(-b() / 2 * a())
        case x if x < 0 => Set()
        case _ => Set((-b() + Math.sqrt(delta_val)) / (2 * a()), (-b() - Math.sqrt(delta_val)) /( 2 * a()))
      }
    }
  }
}
