package cardano

import org.scalatest._
import cardano.inference.mh._
import cardano.semifield.logprob._
import org.apache.commons.math3.random.MersenneTwister

class MetropolisTest extends FlatSpec with Matchers {

  implicit val random = new MersenneTwister(42)

  def laplaceWeight(mu: Double, b: Double)(d: Double): Double = {
    - math.abs(d - mu) / b
  }

  def gaussianWeight(mu: Double, sigma: Double)(d: Double): Double = {
    - 0.5 * (d - mu) * (d - mu) / sigma / sigma
  }

  val mu = 5.0
  val b = 2.0

  val symmetricProposal: (Double) => Model[Double] = (d: Double) => Stochastic.gaussian(d, 1.0).l

  val weightedSymmetricProposal: (Double) => Model[Double] = (d: Double) => symmetricProposal(d).weight(
    laplaceWeight(mu, b)
  )

  val nonSymmetricProposal: (Double) => Model[(Double, Double, Double)] = (d: Double) => Stochastic.gaussian(d + 0.5, 2.0).l.map { newD =>
    val backwardProb = gaussianWeight(newD + 0.5, 2.0)(d)
    val forwardProb = gaussianWeight(d + 0.5, 2.0)(newD)
    (newD, forwardProb, backwardProb)
  }

  val weightedNonSymmetricProposal: (Double) => Model[(Double, Double, Double)] = (d: Double) => {
    nonSymmetricProposal(d).weight { case (newD, _, _) => laplaceWeight(mu, b)(newD) }
  }

  val initial: Model[Double] = Stochastic.continuousUniform(4.0, 6.0).l

  val abTestModel: Model[Double] = Stochastic.beta(3.0, 5.0).l.weight(p => 8 * math.log(1 - p) + 4 * math.log(p))

  "Non symmetric proposal with weights in target" should "reach equilibrium" in {
    for(i <- 1 to 5) {
      val chain: Stochastic[Stream[Double]] = metropolisHastings(initial, nonSymmetricProposal, laplaceWeight(mu, b), 100, 10)
      chain.mean(10000).sample should be (5.0 +- 0.2)
      chain.variance(10000).sample should be ((2 * b * b) +- 0.8)
    }
  }

  "Weighted non symmetric proposal" should "reach equilibrium" in {
    for(i <- 1 to 5) {
      val chain: Stochastic[Stream[Double]] = metropolisHastings(initial, weightedNonSymmetricProposal, (d: Double) => initial.semifield.unit, 100, 10)
      chain.mean(10000).sample should be(5.0 +- 0.2)
      chain.variance(10000).sample should be((2 * b * b) +- 0.8)
    }
  }

  "Symmetric proposal with weights in target" should "reach equilibrium" in {
    for(i <- 1 to 5) {
      val chain: Stochastic[Stream[Double]] = metropolis(initial, symmetricProposal, laplaceWeight(mu, b), 100, 10)
      chain.mean(10000).sample should be(5.0 +- 0.2)
      chain.variance(10000).sample should be((2 * b * b) +- 1.5)
    }
  }

  "Weighted symmetric proposal" should "reach equilibrium" in {
    for(i <- 1 to 5) {
      val chain: Stochastic[Stream[Double]] = metropolis(initial, weightedSymmetricProposal, (d: Double) => initial.semifield.unit, 100, 10)
      chain.mean(10000).sample should be(5.0 +- 0.2)
      chain.variance(10000).sample should be((2 * b * b) +- 1.0)
    }
  }

  "Metropolis from prior" should "reach equilibrium" in {
    for(i <- 1 to 5) {
      val chain: Stochastic[Stream[Double]] = metropolisHastingsFromPrior(abTestModel, 100, 10)
      chain.mean(10000).sample should be(0.35 +- 3e-3)
      chain.variance(10000).sample should be(0.010833333 +- 1e-3)
    }
  }

}
