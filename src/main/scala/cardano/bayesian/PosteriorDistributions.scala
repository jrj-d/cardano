package cardano.bayesian

import cardano.{AllDistributions, Distributions, Prob, Stochastic}
import cardano.metropolis.{defaultSampleBurnIn, defaultSampleInterval}

/**
  * This trait implements calculation of posterior distributions using Markov chain Monte-Carlo techniques.
  */
trait PosteriorDistributions extends Distributions {

  self =>

  /**
    * Creates a random variable representing a posterior distribution.
    *
    * The posterior distribution is built from a prior on the parameter to be inferred, observations,
    * and a generative model of the observations given the parameter (the likelihood).
    *
    * @param prior the prior distribution
    * @param observations the observations
    * @param burnIn the number of initial terms of the chain that are thrown away
    * @param interval the number of terms of the chain between two samples
    * @param logLikelihood a function giving the log-likelihood of an observation under a given model
    * @tparam A the concrete type of the parameter
    * @tparam O the type of the observations
    * @return a random variable representing a posterior distribution
    */
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

  /**
    * Creates a random variable representing a posterior distribution.
    *
    * The posterior distribution is built from a prior on the parameter to be inferred, observations,
    * and a generative model of the observations given the parameter (the likelihood).
    *
    * @param prior the prior distribution
    * @param observations the observations
    * @param burnIn the number of initial terms of the chain that are thrown away
    * @param interval the number of terms of the chain between two samples
    * @param likelihood a function giving the likelihood of an observation under a given model
    * @tparam A the concrete type of the parameter
    * @tparam O the type of the observations
    * @return a random variable representing a posterior distribution
    */
  def posterior[A, O](prior: Stochastic[A], observations: Seq[O], burnIn: Int = defaultSampleBurnIn, interval: Int = defaultSampleInterval)
                     (likelihood: (A, O) => Prob): Stochastic[A] = {
    posteriorByLog(prior, observations, burnIn, interval)((a, o) => math.log(likelihood(a, o)))
  }

}
