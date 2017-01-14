package cardano

import cardano.distributions.AllDistributions
import org.apache.commons.math3.random.{MersenneTwister, RandomGenerator}

import scala.language.higherKinds

trait Stochastic[+A] {

  def map[B](f: A => B): Stochastic[B] = StochasticMap(this, f)

  def flatMap[B](f: A => Stochastic[B]): Stochastic[B] = StochasticFlatMap(this, f)

  def filter(f: A => Boolean): Stochastic[A] = StochasticFilter(this, f)

  def repeat[B >: A, F[_]](f: (=> B) => F[B]): Stochastic[F[B]] = StochasticHigherKind(this, f)

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

final case class StochasticFilter[A](stochastic: Stochastic[A], f: A => Boolean) extends Stochastic[A] {
  def sample: A = Stream.continually(stochastic.sample).dropWhile(a => !f(a)).head
  def randomGenerator: RandomGenerator = stochastic.randomGenerator
}

final case class StochasticHigherKind[F[_], A](stochastic: Stochastic[A], f: (=> A) => F[A]) extends Stochastic[F[A]] {

  /** This repeat feature could have been implemented using "map".
    * The only change is replacing `f: A => B` by `f: (=> A) => B`.
    * This would have worked:
    * {{{
    *   scala> val die = Stochastic.uniform(6).map(_ + 1)
    *   die: cardano.Stochastic[Int] = StochasticMap(cardano.distributions.DiscreteDistributions$$anon$1@fc1001b,<function1>)
    *
    *   scala> val dice = die.map(Stream.continually[Int])
    *   dice: cardano.Stochastic[scala.collection.immutable.Stream[Int]] = StochasticMap(StochasticMap(
    *   cardano.distributions.DiscreteDistributions$$anon$1@fc1001b,<function1>),<function1>)
    *
    *   scala> val diePlus5 = die.map(_ + 5)
    *   diePlus5: cardano.Stochastic[Int] = StochasticMap(StochasticMap(cardano.distributions.DiscreteDistributions$$anon$1@fc1001b,<function1>),<function1>)
    * }}}
    * But not:
    * {{{
    *   scala> def f(i: Int) = i + 3
    *   f: (i: Int)Int
    *
    *   scala> die.map(f)
    *   <console>:17: error: type mismatch;
    *     found   : Int => Int
    *     required: (=> Int) => ?
    *        die.map(f)
    * }}}
    * Since it is not reasonable to ask the user to put `(=> A)` in all their signatures for something as basic as `map`,
    * it is better to separate the two features
    */

  def sample: F[A] = f(stochastic.sample)
  def randomGenerator: RandomGenerator = stochastic.randomGenerator
}

object Stochastic extends AllDistributions(new MersenneTwister())
