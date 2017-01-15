package cardano.distributions

import cardano.Stochastic
import org.apache.commons.math3.random.RandomGenerator

trait Distributions {

  self =>

  def constant[A](value: A): Stochastic[A] = new Stochastic[A] {
    override def sample: A = value
    override def randomGenerator: RandomGenerator = self.randomGenerator
  }

  def randomGenerator: RandomGenerator
}

class AllDistributions(val randomGenerator: RandomGenerator) extends DiscreteDistributions with ContinuousDistributions with MaximumEntropyDistributions
