package cardano.metropolis

import cardano.{AllDistributions, Distributions, Stochastic}

/**
  * This trait implements some distributions revolving around the Metropolis-Hastings sampling procedure.
  */
trait MetropolisDistributions extends Distributions {

  self =>

  /**
    * Creates a random variable that samples from a Metropolis-Hastings procedure.
    *
    * If the Markov chain converges properly, the random variable will have a distribution given by
    * `logUnnormalizedProbabilityOf`.
    *
    * @param init the first term of the Markov chain
    * @param burnIn the number of initial terms of the chain that are thrown away
    * @param interval the number of terms of the chain between two samples
    * @param logUnnormalizedProbabilityOf the unnormalized log density of the target distribution
    * @param logTransitionFunction a function that creates the next term of the Markov chain from the value of the
    *                              previous one. The random variable contains the next value and the log probabilities
    *                              of transitioning forth and back from the previous value to the next one.
    * @tparam A the concrete type of the random variable
    * @return a random variable whose distribution is given by `logUnnormalizedProbabilityOf`
    */
  def metropolisHastings[A](init: Stochastic[A], burnIn: Int = defaultSampleBurnIn, interval: Int = defaultSampleInterval)
                           (logUnnormalizedProbabilityOf: A => Double)(logTransitionFunction: A => Stochastic[(A, Double, Double)]): Stochastic[A] = {

    val distributions = new AllDistributions(self.randomGenerator)

    def metropolisTransition(lastValue: A): Stochastic[A] = logTransitionFunction(lastValue).flatMap {
      case (candidateValue, forwardProbLog, backwardProbLog) =>
        val probLastLog = logUnnormalizedProbabilityOf(lastValue)
        val probNewLog = logUnnormalizedProbabilityOf(candidateValue)
        val acceptanceProbLog = probNewLog + backwardProbLog - probLastLog - forwardProbLog
        if(acceptanceProbLog >= 0.0) {
          distributions.constant(candidateValue)
        } else {
          distributions.continuousUniform.map { r =>
            val uniform = math.log(r)
            if (uniform <= acceptanceProbLog) {
              candidateValue
            } else {
              lastValue
            }
          }
        }
    }

    val markovChain: Stochastic[Iterator[A]] = init.markov(metropolisTransition).map(_.drop(burnIn).grouped(interval).map(_.head))

    val iterator: Iterator[A] = markovChain.sample

    new Stochastic[A] {
      override def sample = iterator.next
    }
  }

  /**
    * Creates a random variable that samples from a Metropolis procedure.
    *
    * A Metropolis procedure is simply a Metropolis-Hastings procedure with a symmetric transition function.
    *
    * If the Markov chain converges properly, the random variable will have a distribution given by
    * `logUnnormalizedProbabilityOf`.
    *
    *
    * @param init the first term of the Markov chain
    * @param burnIn the number of initial terms of the chain that are thrown away
    * @param interval the number of terms of the chain between two samples
    * @param logUnnormalizedProbabilityOf the unnormalized log density of the target distribution
    * @param symmetricTransitionFunction a function that creates the next term of the Markov chain from the value of the
    *                                    previous one. Jumping back and forth should have the same probability
    * @tparam A the concrete type of the random variable
    * @return a random variable whose distribution is given by `logUnnormalizedProbabilityOf`
    */
  def metropolis[A](init: Stochastic[A], burnIn: Int = defaultSampleBurnIn, interval: Int = defaultSampleInterval)
                   (logUnnormalizedProbabilityOf: A => Double)(symmetricTransitionFunction: A => Stochastic[A]): Stochastic[A] = {
    metropolisHastings(init, burnIn, interval)(logUnnormalizedProbabilityOf)(a => symmetricTransitionFunction(a).map((_, 0.0, 0.0)))
  }

  /**
    * Creates a random variable sampled from the maximum entropy distribution of `costFunction` at inverse temperature
    * `inverseTemp`.
    *
    * @param init the first term of the Markov chain
    * @param burnIn the number of initial terms of the chain that are thrown away
    * @param interval the number of terms of the chain between two samples
    * @param costFunction a function that gives the cost of every possible value of the random variable
    * @param symmetricTransitionFunction a function that creates the next term of the Markov chain from the value of the
    *                                    previous one. Jumping back and forth should have the same probability
    * @tparam A the concrete type of the random variable
    * @return a random variable with the maximum entropy distribution of `costFunction` at inverse temperature
    *         `inverseTemp`
    */
  def maxEntropy[A](init: Stochastic[A], inverseTemp: Double, burnIn: Int = defaultSampleBurnIn, interval: Int = defaultSampleInterval)
                   (costFunction: A => Double)(symmetricTransitionFunction: A => Stochastic[A]): Stochastic[A] = {
    metropolis(init, burnIn, interval)(a => -inverseTemp * costFunction(a))(symmetricTransitionFunction)
  }

}
