package cardano

import cardano.bayesian.PosteriorDistributions
import cardano.continuous.ContinuousDistributions
import cardano.discrete.DiscreteDistributions
import cardano.metropolis.MetropolisDistributions
import org.apache.commons.math3.random.RandomGenerator

private[cardano] trait Distributions {

  /**
    * Creates a random variable that is constant.
    *
    * @param a the value of this constant random variable
    * @tparam A the concrete type of this random variable
    * @return a constant random variable
    */
  def constant[A](a: A): Stochastic[A] = StochasticConstant(a)

  protected def randomGenerator: RandomGenerator
}

/**
  * A class containing all the standard distributions implemented in [[cardano]].
  *
  * If you don't need to set the seed, it is simpler to use the companion object [[Stochastic]] directly.
  * Otherwise use [[AllDistributions]] and provide it with your own [[org.apache.commons.math3.random.RandomGenerator]].
  *
  * @param randomGenerator the [[org.apache.commons.math3.random.RandomGenerator]] which will be used by all random
  *                        variables.
  */
class AllDistributions(val randomGenerator: RandomGenerator) extends DiscreteDistributions
  with ContinuousDistributions with MetropolisDistributions with PosteriorDistributions
