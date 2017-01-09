package cardano.distributions

import org.apache.commons.math3.random.RandomGenerator

trait Distributions {
  def randomGenerator: RandomGenerator
}

class AllDistributions(val randomGenerator: RandomGenerator) extends DiscreteDistributions with ContinuousDistributions
