package cardano

import cardano.distributions.AllDistributions
import org.apache.commons.math3.random.MersenneTwister
import org.scalatest._

class FunctionalTest extends FlatSpec with Matchers {

  val generator = new AllDistributions(new MersenneTwister(0))

  val rv: Stochastic[Boolean] = generator.coin.filter(b => b)

  "A coin filtered at true" should "always be true" in {
    Seq.fill(100)(rv.sample).forall(b => b) should equal(true)
  }

  "A gaussian (3, 4) plus a gaussian (5, 3)" should "have an expected value of 8" in {
    val rv = generator.gaussian(3, 4).flatMap(v => generator.gaussian(5, 3).map(_ + v))
    for(i <- 0 to 10) {
      rv.expectation(10000) should be (8.0 +- 0.1)
    }
  }

  it should "have an expected value of 5" in {
    val rv = generator.gaussian(3, 4).flatMap(v => generator.gaussian(5, 3).map(_ + v))
    for(i <- 0 to 10) {
      rv.std(10000) should be (5.0 +- 0.1)
    }
  }

  "Hitting a 6" should "take 6 dice rolls on average" in {
    val diceRolls: Stochastic[Stream[Int]] = generator.uniform(6).map(_ + 1).repeat(Stream.continually[Int])
    val requiredRolls: Stochastic[Int] = diceRolls.map(_.takeWhile(_ != 6).length + 1)
    for(i <- 0 to 10) {
      requiredRolls.expectation(10000) should be (6.0 +- 0.1)
    }
  }
}
