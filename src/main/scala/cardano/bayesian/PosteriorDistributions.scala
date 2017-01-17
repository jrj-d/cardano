package cardano.bayesian

import cardano.{AllDistributions, Distributions, Prob, Stochastic}
import cardano.metropolis.{defaultSampleBurnIn, defaultSampleInterval}

trait PosteriorDistributions extends Distributions {

  self =>

  def posteriorByLog[A, O](prior: Stochastic[A], observations: Seq[O], burnIn: Int = defaultSampleBurnIn, interval: Int = defaultSampleInterval)
                          (logLikelihood: (A, O) => Double): Stochastic[A] = {

    val distributions = new AllDistributions(self.randomGenerator)

    def totalLogLikelihood(parameter: A): Double = observations.map(o => logLikelihood(parameter, o)).reduce(_ + _)

    // This is not an actual instance of the Metropolis-Hastings sampling procedure.
    // Instead, we sample using a non symmetric transition function,
    // but the back and forth "jump probabilities" are not given to the decision procedure.
    // This makes the Markov chain tend towards the distribution of the prior and the logLikelihood.

    distributions.metropolis(prior, burnIn, interval)(totalLogLikelihood)(_ => prior)

  }

  def posterior[A, O](prior: Stochastic[A], observations: Seq[O], burnIn: Int = defaultSampleBurnIn, interval: Int = defaultSampleInterval)
                     (likelihood: (A, O) => Prob): Stochastic[A] = {
    posteriorByLog(prior, observations, burnIn, interval)((a, o) => math.log(likelihood(a, o)))
  }

}
