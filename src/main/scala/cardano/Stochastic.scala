package cardano

import cardano.distributions.AllDistributions
import org.apache.commons.math3.random.MersenneTwister

trait Stochastic[+A] {

  def map[B](f: A => B): Stochastic[B] = StochasticMap(this, f)

  def flatMap[B](f: A => Stochastic[B]): Stochastic[B] = StochasticFlatMap(this, f)

  def sample: A

}

final case class StochasticMap[A, +B](stochastic: Stochastic[A], f: A => B) extends Stochastic[B] {
  def sample: B = f(stochastic.sample)
}

final case class StochasticFlatMap[A, +B](stochastic: Stochastic[A], f: A => Stochastic[B]) extends Stochastic[B] {
  def sample: B = f(stochastic.sample).sample
}

object Stochastic extends AllDistributions(new MersenneTwister())
