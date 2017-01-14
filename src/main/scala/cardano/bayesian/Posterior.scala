package cardano.bayesian

import cardano.{Prob, Stochastic}
import cardano.metropolis.{MetropolisHastingsStochastic, defaultSampleBurnIn, defaultSampleInterval}
import org.apache.commons.math3.random.RandomGenerator

class Posterior[+A](prior: Stochastic[A]) {

  def posteriorByLog[O](observations: Seq[O])(logLikelihood: (A, O) => Double): Stochastic[A] = posteriorByLog()(observations)(logLikelihood)

  def posteriorByLog[O](burnIn: Int = defaultSampleBurnIn, interval: Int = defaultSampleInterval)
                  (observations: Seq[O])(logLikelihood: (A, O) => Double): Stochastic[A] = new MetropolisHastingsStochastic[A] {

    // This is not an actual instance of the Metropolis-Hastings sampling procedure.
    // Instead, we sample using a non symmetric transition function,
    // but the back and forth "jump probabilities" are not given to the decision procedure.
    // This makes the Markov chain tend towards the distribution of the prior and the logLikelihood.

    override def logTransitionFunction(a: A): (A, Double, Double) = (prior.sample, 0.0, 0.0)

    override def logUnnormalizedProbabilityOf(a: A): Double = {
      observations.map(o => logLikelihood(a, o)).reduce(_ + _)
    }

    override def initValue: A = prior.sample

    override def randomGenerator: RandomGenerator = prior.randomGenerator

    override val sampleBurnIn: Int = burnIn

    override val sampleInterval: Int = interval

  }

  def posterior[O](burnIn: Int = defaultSampleBurnIn, interval: Int = defaultSampleInterval)
                       (observations: Seq[O])(likelihood: (A, O) => Prob): Stochastic[A] = {
    posteriorByLog(burnIn, interval)(observations)((a, o) => math.log(likelihood(a, o)))
  }

  def posterior[O](observations: Seq[O])(likelihood: (A, O) => Prob): Stochastic[A] = posterior()(observations)(likelihood)

}
