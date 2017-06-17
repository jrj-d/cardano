package cardano.continuous

import breeze.stats.distributions.{Beta, RandBasis, ThreadLocalRandomGenerator}
import cardano._
import org.apache.commons.math3.random.RandomGenerator

/**
  * This trait implements some standard continuous distributions.
  */
trait ContinuousDistributions {

  self =>

  /**
    * Creates a standard normal random variable.
    *
    * @return a standard normal random variable
    */
  def gaussian: Stochastic[Double] = (random: RandomGenerator) => random.nextGaussian()

  /**
    * Creates a normal random variable.
    *
    * @param mean the expectation
    * @param std the standard deviation
    * @return a normal random variable
    */
  def gaussian(mean: Double, std: Double): Stochastic[Double] = gaussian.map(mean + std * _)

  /**
    * Creates a Beta random variable.
    *
    * @param a positive parameter
    * @param b negative parameter
    * @return a Beta random variable
    */
  def beta(a: Double, b: Double): Stochastic[Double] = (random: RandomGenerator) => {
    // not cool performance-wise to create this object
    val sampler = new Beta(a, b)(new RandBasis(new ThreadLocalRandomGenerator(random)))
    sampler.draw()
  }

  /**
    * Creates a uniform random variable on [0, 1].
    *
    * @return a uniform random variable on [0, 1]
    */
  def continuousUniform: Stochastic[Double] = (random: RandomGenerator) => random.nextDouble()

  /**
    * Creates a uniform random variable on [`a`, `b`].
    *
    * @param a a first bound on the interval
    * @param b a second bound on the interval
    * @return a uniform random variable on [`a`, `b`]
    */
  def continuousUniform(a: Double, b: Double): Stochastic[Double] = continuousUniform.map(a + (b - a) * _)

}
