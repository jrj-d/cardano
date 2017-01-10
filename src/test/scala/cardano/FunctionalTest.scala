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
}
