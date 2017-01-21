package cardano.continuous

import breeze.stats.distributions.{Beta, RandBasis, ThreadLocalRandomGenerator}
import cardano._

/**
  * This trait implements some standard continuous distributions.
  */
trait ContinuousDistributions extends Distributions {

  self =>

  /**
    * Creates a standard normal random variable.
    *
    * @return a standard normal random variable
    */
  def gaussian: Stochastic[Double] = new Stochastic[Double] {

    def sample: Double = randomGenerator.nextGaussian()

  }

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
  def beta(a: Double, b: Double): Stochastic[Double] = new Stochastic[Double] {

    private val sampler = new Beta(a, b)(new RandBasis(new ThreadLocalRandomGenerator(randomGenerator)))

    def sample: Double = sampler.draw()

  }

  /**
    * Creates a uniform random variable on [0, 1].
    *
    * @return a uniform random variable on [0, 1]
    */
  def continuousUniform: Stochastic[Double] = new Stochastic[Double] {

    def sample: Double = randomGenerator.nextDouble()

  }

  /**
    * Creates a uniform random variable on [`a`, `b`].
    *
    * @param a a first bound on the interval
    * @param b a second bound on the interval
    * @return a uniform random variable on [`a`, `b`]
    */
  def continuousUniform(a: Double, b: Double): Stochastic[Double] = continuousUniform.map(a + (b - a) * _)

}
