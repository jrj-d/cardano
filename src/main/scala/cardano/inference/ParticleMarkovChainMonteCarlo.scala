package cardano.inference

import cardano.{Model, Stochastic}
import cardano.inference.mh.pseudoMarginalMetropolis
import cardano.inference.smc.sequentialMonteCarlo

trait ParticleMarkovChainMonteCarlo {

  /**
    * Takes as input a probabilistic model divided into two parts, a `prior` and a non-marginalized `likelihood`
    * and outputs a stochastic stream whose equilibrium distribution is the posterior of the model.
    *
    * The `likelihood` is marginalized using a sequential Monte-Carlo algorithm.
    *
    * @param prior a probabilistic model that contains the prior of the global model. If this prior also contains a
    *              likelihood, this likelihood will be used as a factor of the global likelihood.
    * @param likelihood a probabilistic model whose likelihood is the main factor of the global likelihood. The prior
    *                   contained in this model is latent variable that is marginalized using a sequential Monte-Carlo
    *                   algorithm
    * @param burnIn the number of initial terms of the Markov chain to discard
    * @param interval every `interval`-eth term of the Markov chain is kept
    * @param nParticles the number of i.i.d instances to sample from the posterior
    * @tparam A the concrete type of the probability model
    * @tparam B the concrete type of the latent variable of the non-marginalized likelihood
    * @return a random variable representing a Markov chain whose equilibrium distribution is the posterior of the model
    */
  def particleMarginalMetropolisHastings[A, B](prior: Model[A], likelihood: A => Model[B],
                                               burnIn: Int, interval: Int, nParticles: Int): Stochastic[Stream[A]] = {
    pseudoMarginalMetropolis(prior, (_: A) => prior, (a: A) => sequentialMonteCarlo(likelihood(a), nParticles), burnIn, interval)
  }
}
