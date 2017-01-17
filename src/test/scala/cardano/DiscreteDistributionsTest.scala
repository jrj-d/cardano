package cardano

import org.apache.commons.math3.random.MersenneTwister
import org.scalatest._

class DiscreteDistributionsTest extends FlatSpec with Matchers {

  val generator = new AllDistributions(new MersenneTwister(0))

  "A biased coin toss" should "have correct expectation" in {
    val coin: Stochastic[Boolean] = generator.coin(0.3)
    coin.expectation(10000) should be (0.3 +- 0.01)
  }

  "A die roll" should "have its expected value around 3.5 (int version)" in {
    val die: Stochastic[Int] = generator.discreteUniform(6).map(_ + 1)
    for(i <- 0 to 10) {
      die.expectation(10000) should be (3.5 +- 0.1)
      math.exp(die.logExpectation(10000)) should be (3.5 +- 0.1)
    }
    for(i <- 0 to 10) {
      die.expectation(100000) should be (3.5 +- 0.05)
      math.exp(die.logExpectation(100000)) should be (3.5 +- 0.05)
    }
  }

  it should "should have its standard deviation around 1.71 (int version)" in {
    val die: Stochastic[Int] = generator.discreteUniform(6).map(_ + 1)
    for(i <- 0 to 10) {
      die.std(10000) should be (1.71 +- 0.1)
    }
    for(i <- 0 to 10) {
      die.std(100000) should be (1.71 +- 0.01)
    }
  }

  it should "have its expected value around 3.5 (double version)" in {
    val die: Stochastic[Double] = generator.discreteUniform(6).map(_.toDouble + 1)
    for(i <- 0 to 10) {
      die.expectation(10000) should be (3.5 +- 0.1)
      math.exp(die.logExpectation(10000)) should be (3.5 +- 0.1)
    }
    for(i <- 0 to 10) {
      die.expectation(100000) should be (3.5 +- 0.05)
      math.exp(die.logExpectation(100000)) should be (3.5 +- 0.05)
    }
  }

  it should "should have its standard deviation around 1.71 (double version)" in {
    val die: Stochastic[Double] = generator.discreteUniform(6).map(_.toDouble + 1)
    for(i <- 0 to 10) {
      die.std(10000) should be (1.71 +- 0.1)
    }
    for(i <- 0 to 10) {
      die.std(100000) should be (1.71 +- 0.01)
    }
  }

  it should "have its expected value around 3.5 (long hence numeric version)" in {
    val die: Stochastic[Long] = generator.discreteUniform(6).map(_.toLong + 1)
    for(i <- 0 to 10) {
      die.expectation(10000) should be (3.5 +- 0.1)
      math.exp(die.logExpectation(10000)) should be (3.5 +- 0.1)
    }
    for(i <- 0 to 10) {
      die.expectation(100000) should be (3.5 +- 0.05)
      math.exp(die.logExpectation(100000)) should be (3.5 +- 0.05)
    }
  }

  it should "should have its standard deviation around 1.71 (long hence numeric version)" in {
    val die: Stochastic[Long] = generator.discreteUniform(6).map(_.toLong + 1)
    for(i <- 0 to 100) {
      die.std(10000) should be (1.71 +- 0.1)
    }
    for(i <- 0 to 100) {
      die.std(100000) should be (1.71 +- 0.01)
    }
  }

}
