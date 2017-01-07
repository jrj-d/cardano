package cardano

import org.apache.commons.math3.random.MersenneTwister
import org.scalatest._

class StochasticTest extends FlatSpec with Matchers {

  val generator = new StochasticGenerator(new MersenneTwister(0))

  "Expected value of a die throw" should "be around 3.5" in {
    val die: Stochastic[Int] = generator.uniform(6).map(_ + 1)
    for(i <- 0 to 100) {
      die.expectation(10000) should be (3.5 +- 0.1)
    }
    for(i <- 0 to 100) {
      die.expectation(100000) should be (3.5 +- 0.05)
    }
  }
}