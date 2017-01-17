package cardano

import org.apache.commons.math3.random.MersenneTwister
import org.scalatest._

class BayesianInferenceTest extends FlatSpec with Matchers {

  val generator = new AllDistributions(new MersenneTwister(0))

  val prior: Stochastic[Double] = generator.beta(100, 200)

  val observations: Seq[Boolean] = Seq.fill(300)(true) ++ Seq.fill(700)(false)

  val posterior: Stochastic[Double] = generator.posterior(prior, observations) { (parameter, observation) =>
    if(observation) parameter else 1 - parameter
  }

  "Posterior by Bayesian inference on beta-bernoulli" should "have the correct expectation" in {
    posterior.expectation(1000) should be (0.3077 +- 0.005)
  }

  "Posterior by Bayesian inference on beta-bernoulli" should "have the correct standard deviation" in {
    posterior.std(1000) should be (0.0128 +- 0.001)
  }

}
