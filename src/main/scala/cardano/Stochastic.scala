package cardano

import cardano.distributions.AllDistributions
import org.apache.commons.math3.random.{MersenneTwister, RandomGenerator}

trait Stochastic[+A] {

  def map[B](f: A => B): Stochastic[B] = StochasticMap(this, f)

  def flatMap[B](f: A => Stochastic[B]): Stochastic[B] = StochasticFlatMap(this, f)

  def sample: A

  def randomGenerator: RandomGenerator

}

final case class StochasticMap[A, +B](stochastic: Stochastic[A], f: A => B) extends Stochastic[B] {
  def sample: B = f(stochastic.sample)
  def randomGenerator: RandomGenerator = stochastic.randomGenerator
}

final case class StochasticFlatMap[A, +B](stochastic: Stochastic[A], f: A => Stochastic[B]) extends Stochastic[B] {
  def sample: B = f(stochastic.sample).sample
  def randomGenerator: RandomGenerator = stochastic.randomGenerator
}

object Stochastic extends AllDistributions(new MersenneTwister())
