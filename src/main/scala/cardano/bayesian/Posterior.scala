package cardano.bayesian

import cardano.Stochastic
import cardano._
import cardano.metropolis.{MetropolisStochastic, defaultSampleBurnIn, defaultSampleInterval}
import org.apache.commons.math3.random.RandomGenerator

class Posterior[+A](prior: Stochastic[A]) {

  def posterior[O](observations: Seq[O])(likelihood: (A, O) => Prob): Stochastic[A] = posterior()(observations)(likelihood)

  def posterior[O](burnIn: Int = defaultSampleBurnIn, interval: Int = defaultSampleInterval)
                  (observations: Seq[O])(likelihood: (A, O) => Prob): Stochastic[A] = new MetropolisStochastic[A] {

    // This is not an actual instance of the Metropolis-Hastings sampling procedure.
    // Instead, we sample using a non symmetric transition function,
    // but the back and forth "jump probabilities" are not given to the decision procedure.
    // This makes the Markov chain tend towards the distribution of the prior and the likelihood.

    override def symmetricTransitionFunction(a: A): A = prior.sample

    override def unnormalizedProbabilityOf(a: A): Prob = {
      val logLikelihood: Double = observations.map(o => math.log(likelihood(a, o))).reduce(_ + _)
      math.exp(logLikelihood)
    }

    override def initValue: A = prior.sample

    override def randomGenerator: RandomGenerator = prior.randomGenerator

  }

}
