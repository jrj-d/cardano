package cardano

import org.apache.commons.math3.random.MersenneTwister
import org.scalatest._

class MomentsFunctionsTest extends FlatSpec with Matchers {

  implicit val random = new MersenneTwister(0)

  "A weighted die (probability domain)" should "have correct moments" in {
    val weightedDie: Stochastic[(Double, Int)] = Stochastic.discreteUniform(6).map(i => (6.0 - i, i + 1))
    import cardano.semifield.prob._
    for (i <- 0 to 10) {
      weightedDie.mean(10000).sample should be(2.666666667 +- 0.05)
      weightedDie.std(10000).sample should be(1.4907119849998598 +- 0.05)
      math.sqrt(weightedDie.moment(2, 10000).sample) should be(1.4907119849998598 +- 0.05)
    }
  }

  "A weighted die (log-probability domain)" should "have correct moments" in {
    import cardano.semifield.logprob._
    val weightedDie: Stochastic[(Double, Int)] = Stochastic.discreteUniform(6).map(i => (math.log(6.0 - i), i + 1))
    for(i <- 0 to 10) {
      weightedDie.mean(10000).sample should be (2.666666667 +- 0.05)
      weightedDie.std(10000).sample should be (1.4907119849998598 +- 0.05)
      math.sqrt(weightedDie.moment(2, 10000).sample) should be (1.4907119849998598 +- 0.05)
    }
  }

  "A weighted coin (probability domain)" should "have correct moments" in {
    import cardano.semifield.prob._
    val weightedCoin: Stochastic[(Double, Boolean)] = Stochastic.coin.map(b => (if(b) 3.0 else 1.0, b))
    for(i <- 0 to 10) {
      weightedCoin.mean(10000).sample should be (0.75 +- 0.05)
      weightedCoin.std(10000).sample should be (0.433012702 +- 0.05)
      math.sqrt(weightedCoin.moment(2, 10000).sample) should be (0.433012702 +- 0.05)
    }
  }

  "A sequence of weighted coins (probability domain)" should "have correct moments" in {
    import cardano.semifield.prob._
    val weightedCoin: Stochastic[(Double, Boolean)] = Stochastic.coin.map(b => (if(b) 3.0 else 1.0, b))
    val weightedCoins: Stochastic[Seq[(Double, Boolean)]] = weightedCoin.repeat(Seq.fill[(Double, Boolean)](10000))
    for(i <- 0 to 10) {
      weightedCoins.mean(10000).sample should be (0.75 +- 0.05)
      weightedCoins.std(10000).sample should be (0.433012702 +- 0.05)
      math.sqrt(weightedCoins.moment(2, 10000).sample) should be (0.433012702 +- 0.05)
    }
  }

  "A weighted sequence of weighted coins (probability domain)" should "have correct moments" in {
    import cardano.semifield.prob._
    val weightedCoin: Stochastic[(Double, Boolean)] = Stochastic.coin.map(b => (if(b) 3.0 else 1.0, b))
    val weightedCoins: Stochastic[Seq[(Double, Boolean)]] = weightedCoin.repeat(Seq.fill[(Double, Boolean)](10000))
    val weightedSequence: Stochastic[(Double, Seq[(Double, Boolean)])] = weightedCoins.map(seq => (seq.head._1, seq)) // arbitrary
    for(i <- 0 to 10) {
      weightedSequence.mean(10000).sample should be (0.75 +- 0.05)
      weightedSequence.std(10000).sample should be (0.433012702 +- 0.05)
      math.sqrt(weightedSequence.moment(2, 10000).sample) should be (0.433012702 +- 0.05)
    }
  }

  "A stream of weighted coins (probability domain)" should "have correct moments" in {
    import cardano.semifield.prob._
    val weightedCoin: Stochastic[(Double, Boolean)] = Stochastic.coin.map(b => (if(b) 3.0 else 1.0, b))
    val weightedCoins: Stochastic[Stream[(Double, Boolean)]] = weightedCoin.repeat(Stream.continually[(Double, Boolean)])
    for(i <- 0 to 10) {
      weightedCoins.mean(10000).sample should be (0.75 +- 0.05)
      weightedCoins.std(10000).sample should be (0.433012702 +- 0.05)
      math.sqrt(weightedCoins.moment(2, 10000).sample) should be (0.433012702 +- 0.05)
    }
  }

}
