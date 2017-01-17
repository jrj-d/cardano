package cardano

import cardano.bayesian.PosteriorDistributions
import cardano.continuous.ContinuousDistributions
import cardano.discrete.DiscreteDistributions
import cardano.metropolis.MetropolisDistributions
import org.apache.commons.math3.random.RandomGenerator

trait Distributions {

  def randomGenerator: RandomGenerator
}

class AllDistributions(val randomGenerator: RandomGenerator) extends DiscreteDistributions
  with ContinuousDistributions with MetropolisDistributions with PosteriorDistributions
