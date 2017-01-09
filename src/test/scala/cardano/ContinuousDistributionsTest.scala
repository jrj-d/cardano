package cardano

import cardano.distributions.AllDistributions
import org.apache.commons.math3.random.MersenneTwister
import org.scalatest._

class ContinuousDistributionsTest extends FlatSpec with Matchers {

  val generator = new AllDistributions(new MersenneTwister(0))

  "A gaussian (3, 1.5)" should "have its expected value around 3" in {
    val gaussian: Stochastic[Double] = generator.gaussian(3, 1.5)
    for(i <- 0 to 100) {
      gaussian.expectation(10000) should be (3.0 +- 0.1)
    }
    for(i <- 0 to 100) {
      gaussian.expectation(100000) should be (3.0 +- 0.02)
    }
  }

  it should "should have its standard deviation around 1.5" in {
    val gaussian: Stochastic[Double] = generator.gaussian(3, 1.5)
    for(i <- 0 to 100) {
      gaussian.std(10000) should be (1.5 +- 0.05)
    }
    for(i <- 0 to 100) {
      gaussian.std(100000) should be (1.5 +- 0.01)
    }
  }

  "A beta (10, 30)" should "have its expected value around 0.25" in {
    val beta: Stochastic[Double] = generator.beta(10, 30)
    for(i <- 0 to 100) {
      beta.expectation(10000) should be (0.25 +- 0.002)
    }
    for(i <- 0 to 100) {
      beta.expectation(100000) should be (0.25 +- 0.0005)
    }
  }

  it should "should have its standard deviation around 0.0676" in {
    val beta: Stochastic[Double] = generator.beta(10, 30)
    for(i <- 0 to 100) {
      beta.std(10000) should be (0.0676 +- 0.005)
    }
    for(i <- 0 to 100) {
      beta.std(100000) should be (0.0676 +- 0.001)
    }
  }

}
