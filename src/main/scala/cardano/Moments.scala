package cardano

class Moments[+A](stochastic: Stochastic[A])(implicit numeric: Numeric[A]) {

  private def addLogNumbers(loga: Double, logb: Double) = {
    val max = math.max(loga, logb)
    val min = math.min(loga, logb)
    max + math.log(1 + math.exp(min - max))
  }

  def logExpectation(samples: Int = 1000): Double = {
    val logSum: Double = Stream.fill(samples)(stochastic.sample).foldLeft(Double.NegativeInfinity) { (logSum, sample) =>
      addLogNumbers(logSum, math.log(numeric.toDouble(sample)))
    }
    logSum - math.log(samples)
  }

  def expectation(samples: Int = 1000): Double = math.exp(logExpectation(samples))

  def variance(samples: Int = 1000): Double = {
    val mean: Double = expectation(samples)
    val squaredCenteredRV: Stochastic[Double] = stochastic.map { v =>
      val vDouble: Double = numeric.toDouble(v)
      (vDouble - mean) * (vDouble - mean)
    }
    squaredCenteredRV.expectation(samples)
  }

  def std(samples: Int = 1000): Double = math.sqrt(variance(samples))

}