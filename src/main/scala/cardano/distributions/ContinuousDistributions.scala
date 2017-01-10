package cardano.distributions

import breeze.stats.distributions.{Beta, RandBasis, ThreadLocalRandomGenerator}
import cardano._

trait ContinuousDistributions extends Distributions {

  def gaussian: Stochastic[Double] = new Stochastic[Double] {

    def sample: Double = randomGenerator.nextGaussian()

  }

  def gaussian(mean: Double, std: Double): Stochastic[Double] = gaussian.map(mean + std * _)

  def beta(a: Double, b: Double): Stochastic[Double] = new Stochastic[Double] {

    private val sampler = new Beta(a, b)(new RandBasis(new ThreadLocalRandomGenerator(randomGenerator)))

    def sample: Double = sampler.draw()

  }

}
