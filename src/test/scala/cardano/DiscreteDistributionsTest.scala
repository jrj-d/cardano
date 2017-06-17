package cardano

import org.apache.commons.math3.random.MersenneTwister
import org.scalatest._
import cardano.semifield.prob._


class DiscreteDistributionsTest extends FlatSpec with Matchers {

  implicit val random = new MersenneTwister(0)

  "A biased coin toss" should "have correct expectation" in {
    val coin: Stochastic[Boolean] = Stochastic.coin(0.3)
    coin.mean(10000).sample should be (0.3 +- 0.01)
  }

  "A die roll" should "have its expected value around 3.5 (int version)" in {
    val die: Stochastic[Int] = Stochastic.discreteUniform(6).map(_ + 1)
    for(i <- 0 to 10) {
      die.mean(10000).sample should be (3.5 +- 0.1)
    }
    for(i <- 0 to 10) {
      die.mean(100000).sample should be (3.5 +- 0.05)
    }
  }

  it should "should have its standard deviation around 1.71 (int version)" in {
    val die: Stochastic[Int] = Stochastic.discreteUniform(6).map(_ + 1)
    for(i <- 0 to 10) {
      die.std(10000).sample should be (1.71 +- 0.1)
    }
    for(i <- 0 to 10) {
      die.std(100000).sample should be (1.71 +- 0.01)
    }
  }

  it should "have its expected value around 3.5 (double version)" in {
    val die: Stochastic[Double] = Stochastic.discreteUniform(6).map(_.toDouble + 1)
    for(i <- 0 to 10) {
      die.mean(10000).sample should be (3.5 +- 0.1)
    }
    for(i <- 0 to 10) {
      die.mean(100000).sample should be (3.5 +- 0.05)
    }
  }

  it should "should have its standard deviation around 1.71 (double version)" in {
    val die: Stochastic[Double] = Stochastic.discreteUniform(6).map(_.toDouble + 1)
    for(i <- 0 to 10) {
      die.std(10000).sample should be (1.71 +- 0.1)
    }
    for(i <- 0 to 10) {
      die.std(100000).sample should be (1.71 +- 0.01)
    }
  }

  it should "have its expected value around 3.5 (long hence numeric version)" in {
    val die: Stochastic[Long] = Stochastic.discreteUniform(6).map(_.toLong + 1)
    for(i <- 0 to 10) {
      die.mean(10000).sample should be (3.5 +- 0.1)
    }
    for(i <- 0 to 10) {
      die.mean(100000).sample should be (3.5 +- 0.05)
    }
  }

  it should "should have its standard deviation around 1.71 (long hence numeric version)" in {
    val die: Stochastic[Long] = Stochastic.discreteUniform(6).map(_.toLong + 1)
    for(i <- 0 to 10) {
      die.std(10000).sample should be (1.71 +- 0.1)
    }
    for(i <- 0 to 10) {
      die.std(100000).sample should be (1.71 +- 0.01)
    }
  }

  "A piped die" should "have an expected value of 8/3" in {
    val die: Stochastic[Int] = Stochastic.fromMass((6 to 1 by -1).map(_.toDouble)).map(_ + 1)
    for(i <- 0 to 10) {
      die.mean(10000).sample should be (2.6666667 +- 0.05)
    }
    for(i <- 0 to 10) {
      die.mean(100000).sample should be (2.6666667 +- 0.02)
    }
  }

  it should "have its standard deviation around 1.4907119849998598" in {
    val die: Stochastic[Int] = Stochastic.fromMass((6 to 1 by -1).map(_.toDouble)).map(_ + 1)
    for(i <- 0 to 10) {
      die.std(10000).sample should be (1.4907119849998598 +- 0.05)
    }
    for(i <- 0 to 10) {
      die.std(100000).sample should be (1.4907119849998598 +- 0.01)
    }
  }

}
