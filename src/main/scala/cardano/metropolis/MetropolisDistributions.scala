package cardano.metropolis

import cardano.{AllDistributions, Distributions, Stochastic, StochasticConstant}

trait MetropolisDistributions extends Distributions {

  self =>

  def metropolisHastings[A](init: Stochastic[A], burnIn: Int = defaultSampleBurnIn, interval: Int = defaultSampleInterval)
                           (logUnnormalizedProbabilityOf: A => Double)(logTransitionFunction: A => Stochastic[(A, Double, Double)]): Stochastic[A] = {

    val distributions = new AllDistributions(self.randomGenerator)

    def metropolisTransition(lastValue: A): Stochastic[A] = logTransitionFunction(lastValue).flatMap {
      case (candidateValue, forwardProbLog, backwardProbLog) =>
        val probLastLog = logUnnormalizedProbabilityOf(lastValue)
        val probNewLog = logUnnormalizedProbabilityOf(candidateValue)
        val acceptanceProbLog = probNewLog + backwardProbLog - probLastLog - forwardProbLog
        if(acceptanceProbLog >= 0.0) {
          StochasticConstant(candidateValue)
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

  def metropolis[A](init: Stochastic[A], burnIn: Int = defaultSampleBurnIn, interval: Int = defaultSampleInterval)
                   (logUnnormalizedProbabilityOf: A => Double)(symmetricTransitionFunction: A => Stochastic[A]): Stochastic[A] = {
    metropolisHastings(init, burnIn, interval)(logUnnormalizedProbabilityOf)(a => symmetricTransitionFunction(a).map((_, 0.0, 0.0)))
  }

  def maxEntropy[A](init: Stochastic[A], inverseTemp: Double, burnIn: Int = defaultSampleBurnIn, interval: Int = defaultSampleInterval)
                   (costFunc: A => Double)(symmetricTransitionFunction: A => Stochastic[A]): Stochastic[A] = {
    metropolis(init, burnIn, interval)(a => -inverseTemp * costFunc(a))(symmetricTransitionFunction)
  }

}
