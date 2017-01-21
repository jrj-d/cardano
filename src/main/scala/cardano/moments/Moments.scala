package cardano.moments

import cardano.Stochastic

/**
  * This class adds moment calculation methods to random variables.
  *
  * @tparam A the concrete type of the random variable.
  */
trait Moments[@specialized(Double, Int) +A] {

  /**
    * Computes the logarithm of the expectation of the random variable.
    *
    * This method is more precise than [[expectation]] when summing small values.
    * Note that the random variable needs to be positive.
    *
    * @param samples the number of samples to compute the expectation
    * @return the expected value of the random variable
    */
  def logExpectation(samples: Int = defaultNbSamples): Double

  /**
    * See [[logExpectation]].
    */
  def logExpectation: Double = logExpectation()

  /**
    * Computes the expectation of the random variable.
    *
    * @param samples the number of samples to compute the expectation
    * @return the expected value of the random variable
    */
  def expectation(samples: Int = defaultNbSamples): Double

  /**
    * See [[expectation]].
    */
  def expectation: Double = expectation()

  /**
    * Computes the variance of the random variable.
    *
    * @param samples the number of samples to compute the variance
    * @return the variance of the random variable
    */
  def variance(samples: Int = defaultNbSamples): Double

  /**
    * See [[variance]].
    */
  def variance: Double = variance()

  /**
    * Computes the standard deviation of the random variable.
    *
    * @param samples the number of samples to compute the standard deviation
    * @return the standard deviation of the random variable
    */
  def std(samples: Int = defaultNbSamples): Double = math.sqrt(variance(samples))

  /**
    * See [[std]].
    */
  def std: Double = std()
}

private[cardano] class NumericMoments[+A](stochastic: Stochastic[A])(implicit numeric: Numeric[A]) extends Moments[A] {

  private def addLogNumbers(loga: Double, logb: Double) = {
    val max = math.max(loga, logb)
    val min = math.min(loga, logb)
    max + math.log(1 + math.exp(min - max))
  }

  def logExpectation(samples: Int = defaultNbSamples): Double = {
    val logSum: Double = Stream.fill(samples)(stochastic.sample).foldLeft(Double.NegativeInfinity) { (logSum, sample) =>
      addLogNumbers(logSum, math.log(numeric.toDouble(sample)))
    }
    logSum - math.log(samples)
  }

  def expectation(samples: Int = defaultNbSamples): Double = {
    numeric.toDouble(Stream.fill(samples)(stochastic.sample).sum) / samples.toDouble
  }

  def variance(samples: Int = defaultNbSamples): Double = {
    val mean: Double = expectation(samples)
    val squaredCenteredRV: Stochastic[Double] = stochastic.map { v =>
      val vDouble: Double = numeric.toDouble(v)
      (vDouble - mean) * (vDouble - mean)
    }
    squaredCenteredRV.expectation(samples)
  }

}

private[cardano] class DoubleMoments(stochastic: Stochastic[Double]) extends Moments[Double] {

  private def addLogNumbers(loga: Double, logb: Double) = {
    val max = math.max(loga, logb)
    val min = math.min(loga, logb)
    max + math.log(1 + math.exp(min - max))
  }

  def logExpectation(samples: Int = defaultNbSamples): Double = {
    val logSum: Double = Stream.fill(samples)(stochastic.sample).foldLeft(Double.NegativeInfinity) { (logSum, sample) =>
      addLogNumbers(logSum, math.log(sample))
    }
    logSum - math.log(samples)
  }

  def expectation(samples: Int = defaultNbSamples): Double = {
    Stream.fill(samples)(stochastic.sample).reduceLeft(_ + _) / samples.toDouble
  }

  def variance(samples: Int = defaultNbSamples): Double = {
    val mean: Double = expectation(samples)
    val squaredCenteredRV: Stochastic[Double] = stochastic.map { v => (v - mean) * (v - mean) }
    squaredCenteredRV.expectation(samples)
  }

}

private[cardano] class IntMoments(stochastic: Stochastic[Int]) extends Moments[Int] {

  private def addLogNumbers(loga: Double, logb: Double) = {
    val max = math.max(loga, logb)
    val min = math.min(loga, logb)
    max + math.log(1 + math.exp(min - max))
  }

  def logExpectation(samples: Int = defaultNbSamples): Double = {
    val logSum: Double = Stream.fill(samples)(stochastic.sample).foldLeft(Double.NegativeInfinity) { (logSum, sample) =>
      addLogNumbers(logSum, math.log(sample))
    }
    logSum - math.log(samples)
  }

  def expectation(samples: Int = defaultNbSamples): Double = {
    Stream.fill(samples)(stochastic.sample).reduceLeft(_ + _) / samples.toDouble
  }

  def variance(samples: Int = defaultNbSamples): Double = {
    val mean: Double = expectation(samples)
    val squaredCenteredRV: Stochastic[Double] = stochastic.map { v => (v - mean) * (v - mean) }
    squaredCenteredRV.expectation(samples)
  }

}
