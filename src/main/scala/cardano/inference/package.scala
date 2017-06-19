package cardano

import org.apache.commons.math3.random.RandomGenerator
import cardano.semifield.syntax._

import scala.annotation.tailrec

package object inference {

  /**
    * Runs a model to completion a model by extracting data from a random variable.
    *
    * Inspired by [cats.Free.go].
    *
    * @param model the model to consume
    * @param f the function extracting data from a random variable
    * @tparam A the concrete type of the model
    * @return a tuple of type `(Double, A)`
    */
  def consume[A](model: Model[A], f: Stochastic[(Double, Model[A])] => (Double, Model[A])): (Double, A) = {

    implicit val semifield = model.semifield

    @tailrec def loop(weight: Double, m: Model[A]): (Double, A) = {
      m.step match {
        case Left(s) =>
          val (newWeight, newModel) = f(s)
          loop(newWeight |*| weight, newModel)
        case Right((finalWeight, a)) => (finalWeight |*| weight, a)
      }
    }

    loop(semifield.unit, model)
  }

  /**
    * Transforms a probabilistic model into a random variable that samples value from the prior along with the
    * likelihood.
    *
    * @param model the probabilistic model to transform
    * @tparam A the concrete type of the probabilistic model
    * @return a random variable that samples tuples of type `(Double, A)` where the second term is a sample from the
    *         model's prior, and the first term is the associated likelihood
    */
  def prior[A](model: Model[A]): Stochastic[(Double, A)] =
    Stochastic((random: RandomGenerator) => consume(model, (s: Stochastic[(Double, Model[A])]) => s.sample(random)))

  /**
    * Builds a Markov chain according to the Metropolis-Hastings algorithm.
    *
    * This function is the full-fledged algorithm with everything parameterizable; in practice, `metropolisFromPrior`
    * should be used.
    *
    * @param initial the first element of the Markov chain
    * @param proposal the proposal distribution as a probabilistic model. The second term of the tuple is the forward
    *                 sampling probability, the third term is the backward sampling probability.
    *                 The likelihood of the proposal is incorporated into the computation of the likelihood of the
    *                 target distribution.
    * @param target an additional factor in the likelihood of the target distribution
    * @param burnIn the number of initial terms of the Markov chain to discard
    * @param interval every `interval`-eth term of the Markov chain is kept
    * @tparam A the concrete type of the probability model
    * @return a random variable representing the Markov chain produced by the Metropolis-Hastings algorithm
    */
  def metropolisHastings[A](initial: Model[A], proposal: A => Model[(A, Double, Double)], target: A => Double,
                            burnIn: Int, interval: Int): Stochastic[Stream[A]] = {

    implicit val semifield = initial.semifield

    def chainTransition(weightedLastSample: (Double, A)): Stochastic[(Double, A)] = {

      val (lastFullWeight, lastSample) = weightedLastSample

      prior(proposal(lastSample)).flatMap {

        case (candidateWeight, (candidateSample, forwardProb, backwardProb)) =>

          val candidateFullWeight = target(candidateSample) |*| candidateWeight
          val candidateScore = candidateFullWeight |*| backwardProb
          val lastScore = lastFullWeight |*| forwardProb

          if(candidateScore >= lastScore) {
            Stochastic.pure(candidateFullWeight, candidateSample)
          } else {
            Stochastic.continuousUniform.map { r =>
              val uniform = semifield.inject(r)
              if((uniform |*| lastScore) <= candidateScore) {
                (candidateFullWeight, candidateSample)
              } else {
                (lastFullWeight, lastSample)
              }
            }
          }
      }
    }

    val weightedChainDist = prior(initial).markov(chainTransition)

    weightedChainDist.map { weightedChain =>
      weightedChain.map(_._2).drop(burnIn).grouped(interval).map(_.head).toStream
    }
  }

  /**
    * Builds a Markov chain according to the Metropolis algorithm (Metropolis algorithm with symmetric proposal).
    *
    * Note that the proposal should be symmetric!
    *
    * @param initial the first element of the Markov chain
    * @param proposal the proposal distribution as a probabilistic model. The likelihood of the proposal is
    *                 incorporated into the computation of the likelihood of the target distribution.
    * @param target an additional factor in the likelihood of the target distribution
    * @param burnIn the number of initial terms of the Markov chain to discard
    * @param interval every `interval`-eth term of the Markov chain is kept
    * @tparam A the concrete type of the probability model
    * @return a random variable representing the Markov chain produced by the Metropolis algorithm
    */
  def metropolis[A](initial: Model[A], proposal: A => Model[A], target: A => Double, burnIn: Int, interval: Int): Stochastic[Stream[A]] = {

    implicit val semifield = initial.semifield

    metropolisHastings(initial, (a: A) => proposal(a).map((_, semifield.unit, semifield.unit)), target, burnIn, interval)
  }

  /**
    * Transforms a probabilistic model (prior + likelihood) into a Markov chain whose equilibrium state is the posterior
    * of the model.
    *
    * Beware, the metropolis algorithm (especially when the proposal is the prior as it is the case here) may not
    * converge.
    *
    * @param model the probabilistic model from which to extract the posterior
    * @param burnIn the number of initial terms of the Markov chain to discard
    * @param interval every `interval`-eth term of the Markov chain is kept
    * @tparam A the concrete type of the probability model
    * @return a random variable representing a Markov chain whose equilibrium state is the posterior of the model
    */
  def metropolisFromPrior[A](model: Model[A], burnIn: Int, interval: Int): Stochastic[Stream[A]] = {
    implicit val semifield = model.semifield
    metropolis(model, (_: A) => model, (_: A) => semifield.unit, burnIn, interval)
  }

}
