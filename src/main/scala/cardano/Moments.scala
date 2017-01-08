package cardano

trait Moments[@specialized(Double, Int) +A] {
  def logExpectation(samples: Int = 1000): Double

  def expectation(samples: Int = 1000): Double

  def variance(samples: Int = 1000): Double

  def std(samples: Int = 1000): Double = math.sqrt(variance(samples))
}

class NumericMoments[+A](stochastic: Stochastic[A])(implicit numeric: Numeric[A]) extends Moments[A] {

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

  def expectation(samples: Int = 1000): Double = {
    numeric.toDouble(Stream.fill(samples)(stochastic.sample).sum) / samples.toDouble
  }

  def variance(samples: Int = 1000): Double = {
    val mean: Double = expectation(samples)
    val squaredCenteredRV: Stochastic[Double] = stochastic.map { v =>
      val vDouble: Double = numeric.toDouble(v)
      (vDouble - mean) * (vDouble - mean)
    }
    squaredCenteredRV.expectation(samples)
  }

}

class DoubleMoments(stochastic: Stochastic[Double]) extends Moments[Double] {

  private def addLogNumbers(loga: Double, logb: Double) = {
    val max = math.max(loga, logb)
    val min = math.min(loga, logb)
    max + math.log(1 + math.exp(min - max))
  }

  def logExpectation(samples: Int = 1000): Double = {
    val logSum: Double = Stream.fill(samples)(stochastic.sample).foldLeft(Double.NegativeInfinity) { (logSum, sample) =>
      addLogNumbers(logSum, math.log(sample))
    }
    logSum - math.log(samples)
  }

  def expectation(samples: Int = 1000): Double = {
    Stream.fill(samples)(stochastic.sample).reduceLeft(_ + _) / samples.toDouble
  }

  def variance(samples: Int = 1000): Double = {
    val mean: Double = expectation(samples)
    val squaredCenteredRV: Stochastic[Double] = stochastic.map { v => (v - mean) * (v - mean) }
    squaredCenteredRV.expectation(samples)
  }

}

class IntMoments(stochastic: Stochastic[Int]) extends Moments[Int] {

  private def addLogNumbers(loga: Double, logb: Double) = {
    val max = math.max(loga, logb)
    val min = math.min(loga, logb)
    max + math.log(1 + math.exp(min - max))
  }

  def logExpectation(samples: Int = 1000): Double = {
    val logSum: Double = Stream.fill(samples)(stochastic.sample).foldLeft(Double.NegativeInfinity) { (logSum, sample) =>
      addLogNumbers(logSum, math.log(sample))
    }
    logSum - math.log(samples)
  }

  def expectation(samples: Int = 1000): Double = {
    Stream.fill(samples)(stochastic.sample).reduceLeft(_ + _) / samples.toDouble
  }

  def variance(samples: Int = 1000): Double = {
    val mean: Double = expectation(samples)
    val squaredCenteredRV: Stochastic[Double] = stochastic.map { v => (v - mean) * (v - mean) }
    squaredCenteredRV.expectation(samples)
  }

}