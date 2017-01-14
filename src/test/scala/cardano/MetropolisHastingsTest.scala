package cardano

import cardano.distributions.AllDistributions
import org.apache.commons.math3.random.MersenneTwister
import org.scalatest._

class MetropolisHastingsTest extends FlatSpec with Matchers {

  val generator = new AllDistributions(new MersenneTwister(0))

  def gaussianMH(mean: Double, std: Double): Stochastic[Double] = {

    val inverseTemp: Double = 0.5 / std / std

    generator.maxEnt(inverseTemp)(generator.randomGenerator.nextDouble)(a => (a - mean) * (a - mean)) {
      d => d + generator.randomGenerator.nextDouble() - 0.5
    }

  }

  val rv: Stochastic[Double] = gaussianMH(3.0, 1.5)

  "A MetropolisHastings Gaussian RV" should "have correct mean" in {
    for(i <- 0 to 100) {
      rv.expectation(10000) should be (3.0 +- 0.2)
    }
    for(i <- 0 to 100) {
      rv.expectation(100000) should be (3.0 +- 0.1)
    }
  }

  it should "have correct std" in {
    for(i <- 0 to 100) {
      rv.std(10000) should be (1.5 +- 0.2)
    }
    for(i <- 0 to 100) {
      rv.std(100000) should be (1.5 +- 0.1)
    }
  }
}
