package cardano

import org.apache.commons.math3.random.MersenneTwister
import org.scalatest._

class MomentsTest extends FlatSpec with Matchers {

  val generator = new StochasticGenerator(new MersenneTwister(0))

  "A die roll" should "have its expected value around 3.5" in {
    val die: Stochastic[Int] = generator.uniform(6).map(_ + 1)
    for(i <- 0 to 100) {
      die.expectation(10000) should be (3.5 +- 0.1)
    }
    for(i <- 0 to 100) {
      die.expectation(100000) should be (3.5 +- 0.05)
    }
  }

  it should "should have its standard deviation around 1.71" in {
    val die: Stochastic[Int] = generator.uniform(6).map(_ + 1)
    for(i <- 0 to 100) {
      die.std(10000) should be (1.71 +- 0.1)
    }
    for(i <- 0 to 100) {
      die.std(100000) should be (1.71 +- 0.01)
    }
  }

}