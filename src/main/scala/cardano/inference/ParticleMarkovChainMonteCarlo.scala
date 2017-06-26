package cardano.inference

import cardano.inference.mh.pseudoMarginalMetropolisHastings
import cardano.inference.smc.sequentialMonteCarlo
import cardano.{Model, Stochastic}

trait ParticleMarkovChainMonteCarlo {

  /**
    * Builds a Markov chain according to the Metropolis-Hastings algorithm and marginalizes the latent variables in
    * the likelihood using the sequential Monte Carlo algorithm.
    *
    * @param initial the first element of the Markov chain
    * @param proposal the proposal distribution as a probabilistic model. The second term of the tuple is the forward
    *                 sampling probability, the third term is the backward sampling probability.
    *                 The likelihood of the proposal is incorporated into the computation of the likelihood of the
    *                 target distribution.
    * @param target a probabilistic model whose likelihood is an additional non-marginalized factor in the likelihood
    *               of the target distribution. The model prior describes a latent variable that is discarded. The
    *               latent variable is marginalized using the sequential Monte Carlo algorithm.
    * @param burnIn the number of initial terms of the Markov chain to discard
    * @param interval every `interval`-eth term of the Markov chain is kept
    * @param nParticles the number of i.i.d instances to sample from the posterior of `target`
    * @tparam A the concrete type of the probability model
    * @tparam B the concrete type of the latent variable of the non-marginalized likelihood
    * @return a random variable representing the Markov chain produced by the Metropolis-Hastings algorithm
    */
  def particleMarginalMetropolisHastings[A, B](initial: Model[A], proposal: A => Model[(A, Double, Double)], target: A => Model[B],
                                               burnIn: Int, interval: Int, nParticles: Int): Stochastic[Stream[A]] = {
    pseudoMarginalMetropolisHastings(initial, proposal, (a: A) => sequentialMonteCarlo(target(a), nParticles), burnIn, interval)
  }

  /**
    * Builds a Markov chain according to the Metropolis algorithm (symmetric proposal) and marginalizes the latent
    * variables in the likelihood using the sequential Monte Carlo algorithm.
    *
    * Note that the proposal should be symmetric!
    *
    * @param initial the first element of the Markov chain
    * @param proposal the proposal distribution as a probabilistic model. The likelihood of the proposal is
    *                 incorporated into the computation of the likelihood of the target distribution.
    * @param target a probabilistic model whose likelihood is an additional non-marginalized factor in the likelihood
    *               of the target distribution. The model prior describes a latent variable that is discarded. The
    *               latent variable is marginalized using the sequential Monte Carlo algorithm.
    * @param burnIn the number of initial terms of the Markov chain to discard
    * @param interval every `interval`-eth term of the Markov chain is kept
    * @param nParticles the number of i.i.d instances to sample from the posterior of `target`
    * @tparam A the concrete type of the probability model
    * @tparam B the concrete type of the latent variable of the non-marginalized likelihood
    * @return a random variable representing the Markov chain produced by the Metropolis-Hastings algorithm
    */
  def particleMarginalMetropolis[A, B](initial: Model[A], proposal: A => Model[A], target: A => Model[B],
                                       burnIn: Int, interval: Int, nParticles: Int): Stochastic[Stream[A]] = {

    implicit val semifield = initial.semifield

    particleMarginalMetropolisHastings(initial, (a: A) => proposal(a).map((_, semifield.unit, semifield.unit)), target,
                                       burnIn, interval, nParticles)
  }

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
    * @param nParticles the number of i.i.d instances to sample from the posterior of `target`
    * @tparam A the concrete type of the probability model
    * @tparam B the concrete type of the latent variable of the non-marginalized likelihood
    * @return a random variable representing a Markov chain whose equilibrium distribution is the posterior of the model
    */
  def particleMarginalMetropolisHastingsFromPrior[A, B](prior: Model[A], likelihood: A => Model[B],
                                                        burnIn: Int, interval: Int, nParticles: Int): Stochastic[Stream[A]] = {
    particleMarginalMetropolis(prior, (_: A) => prior, likelihood, burnIn, interval, nParticles)
  }
}
