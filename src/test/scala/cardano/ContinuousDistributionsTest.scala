package cardano

import org.apache.commons.math3.random.MersenneTwister
import org.scalatest._
import cardano.semifield.prob._

class ContinuousDistributionsTest extends FlatSpec with Matchers {

  implicit val random = new MersenneTwister(0)

  "A gaussian (3, 1.5)" should "have its expected value around 3" in {
    val gaussian: Stochastic[Double] = Stochastic.gaussian(3, 1.5)
    for(i <- 0 to 10) {
      gaussian.mean(10000).sample should be (3.0 +- 0.1)
    }
    for(i <- 0 to 10) {
      gaussian.mean(100000).sample should be (3.0 +- 0.02)
    }
  }

  it should "should have its standard deviation around 1.5" in {
    val gaussian: Stochastic[Double] = Stochastic.gaussian(3, 1.5)
    for(i <- 0 to 10) {
      gaussian.std(10000).sample should be (1.5 +- 0.05)
      math.sqrt(gaussian.moment(2, 1000).sample) should be (1.5 +- 0.05)
    }
    for(i <- 0 to 10) {
      gaussian.std(100000).sample should be (1.5 +- 0.01)
      math.sqrt(gaussian.moment(2, 10000).sample) should be (1.5 +- 0.05)
    }
  }

  "A beta (10, 30)" should "have its expected value around 0.25" in {
    val beta: Stochastic[Double] = Stochastic.beta(10, 30)
    for(i <- 0 to 10) {
      beta.mean(10000).sample should be (0.25 +- 0.002)
    }
    for(i <- 0 to 10) {
      beta.mean(100000).sample should be (0.25 +- 0.0005)
    }
  }

  it should "should have its standard deviation around 0.0676" in {
    val beta: Stochastic[Double] = Stochastic.beta(10, 30)
    for(i <- 0 to 10) {
      beta.std(10000).sample should be (0.0676 +- 0.005)
    }
    for(i <- 0 to 10) {
      beta.std(100000).sample should be (0.0676 +- 0.001)
    }
  }

  "A continuous uniform in [4, 10]" should "have its expected value around 7" in {
    val uniform: Stochastic[Double] = Stochastic.continuousUniform(4, 10)
    for(i <- 0 to 10) {
      uniform.mean(10000).sample should be (7.0 +- 0.05)
    }
    for(i <- 0 to 10) {
      uniform.mean(100000).sample should be (7.0 +- 0.01)
    }
  }

  it should "have its standard deviation around 1.732050808" in {
    val uniform: Stochastic[Double] = Stochastic.continuousUniform(4, 10)
    for(i <- 0 to 10) {
      uniform.std(10000).sample should be (1.732050808 +- 0.02)
    }
    for(i <- 0 to 10) {
      uniform.std(100000).sample should be (1.732050808 +- 0.005)
    }
  }

}
