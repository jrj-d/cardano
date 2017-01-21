package cardano

import cardano.moments.{DoubleMoments, IntMoments, Moments, NumericMoments}
import org.apache.commons.math3.random.MersenneTwister

import scala.language.higherKinds

/**
  * A random variable of concrete type `A`.
  *
  * @tparam A the concrete type of the random variable
  */
trait Stochastic[+A] {

  /**
    * Creates a new random variable by applying a deterministic function
    * to the underlying random variable.
    * Samples from the new random variable will be samples from the underlying one
    * to which function `f` is applied.
    *
    * @param f the deterministic function to apply
    * @tparam B the output type of function `f` and the concrete type of the new random variable
    * @return a random variable
    */
  def map[B](f: A => B): Stochastic[B] = StochasticMap(this, f)

  /**
    * Creates a new random variable by applying a stochastic function to the underlying
    * random variable.
    * A sample from the new random variable is a sample from the random variable
    * created by applying `f` to a sample of the underlying random variable.
    *
    * @param f the stochastic function to apply
    * @tparam B the concrete type of the output of stochastic function `f` and of the new random variable
    * @return a random variable
    */
  def flatMap[B](f: A => Stochastic[B]): Stochastic[B] = StochasticFlatMap(this, f)

  /**
    * Creates a new random variable by filtering the values of the underlying random variable.
    * This implements rejection sampling.
    * Same as `filter`, but needed for `for`-comprehensions.
    *
    * @param f the filter function
    * @return a random variable
    */
  def withFilter(f: A => Boolean): Stochastic[A] = StochasticFilter(this, f)

  /**
    * Creates a new random variable by filtering the values of the underlying random variable.
    * This implements rejection sampling. Same as `withFilter`.
    *
    * @param f the filter function
    * @return a random variable
    */
  def filter(f: A => Boolean): Stochastic[A] = withFilter(f)

  /**
    * Creates a new random variable which is a collection or any higher-kinded type of
    * independent and identically distributed copies of the underlying random variable.
    * This can be used to create [[List]]s or even [[Stream]]s of random variables.
    *
    * @param f the function that performs the lifting
    * @tparam B the type of the new random variable (mainly for covariance)
    * @tparam F the type of the collection or higher-kinded type
    * @return a random variable
    */
  def repeat[B >: A, F[_]](f: (=> B) => F[B]): Stochastic[F[B]] = StochasticHigherKind(this, f)

  /**
    * Creates a new random variable which represents a Markov chain of the first order.
    * The relationship between two consecutive terms in the chain is defined by the
    * stochastic function `f`.
    *
    * @param f the distribution of the next term given the previous one
    * @tparam B the type of the next term (mainly for covariance)
    * @return a random variable in the space of infinite sequences of `B`.
    */
  def markov[B >: A](f: B => Stochastic[B]): Stochastic[Stream[B]] = StochasticMarkov(this, f)

  /**
    * Returns a sample from the random variable.
    *
    * @return a sample from the random variable
    */
  def sample: A

}

final private case class StochasticConstant[A](sample: A) extends Stochastic[A]

final private case class StochasticMap[A, +B](stochastic: Stochastic[A], f: A => B) extends Stochastic[B] {
  def sample: B = f(stochastic.sample)
}

final private case class StochasticFlatMap[A, +B](stochastic: Stochastic[A], f: A => Stochastic[B]) extends Stochastic[B] {
  def sample: B = f(stochastic.sample).sample
}

final private case class StochasticFilter[A](stochastic: Stochastic[A], f: A => Boolean) extends Stochastic[A] {
  def sample: A = Stream.continually(stochastic.sample).dropWhile(a => !f(a)).head
}

final private case class StochasticHigherKind[F[_], A](stochastic: Stochastic[A], f: (=> A) => F[A]) extends Stochastic[F[A]] {

  /** This repeat feature could have been implemented using "map".
    * The only change is replacing `f: A => B` by `f: (=> A) => B`.
    * This would have worked:
    * {{{
    *   scala> val die = Stochastic.discreteUniform(6).map(_ + 1)
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
    * Since it is not reasonable to ask the user to put `(=> A)` in all their signatures for something as basic as
    * `map`, it is better to separate the two features.
    */

  def sample: F[A] = f(stochastic.sample)
}

final private case class StochasticMarkov[A](stochastic: Stochastic[A], f: A => Stochastic[A]) extends Stochastic[Stream[A]] {
  def sample: Stream[A] = {
    lazy val chain: Stream[A] = stochastic.sample #:: chain.map(f(_).sample)
    chain
  }
}

/**
  * The main entry point to [[cardano]]. All standard distributions implemented in [[cardano]] can be
  * accessed from here.
  *
  * The seed is automatically set using the current timestamp; if you wish to set the seed yourself,
  * have a look at [[AllDistributions]].
  */
object Stochastic extends AllDistributions(new MersenneTwister()) {

  /**
    * Converts a boolean random variable into an integer random variable.
    */
  implicit def booleanIsInteger(rv: Stochastic[Boolean]): Stochastic[Int] = rv.map(b => if(b) 1 else 0)

  /**
    * Adds moment calculation methods to numeric random variables.
    *
    * See [[moments.Moments]].
    */
  implicit def numericsHaveMoments[A](rv: Stochastic[A])(implicit numeric: Numeric[A]): Moments[A] = new NumericMoments(rv)

  /**
    * Adds specialized moment calculation methods to double random variables.
    *
    * See [[moments.Moments]].
    */
  implicit def doublesHaveMoments(rv: Stochastic[Double]): Moments[Double] = new DoubleMoments(rv)

  /**
    * Adds specialized moment calculation methods to integer random variables.
    *
    * See [[moments.Moments]].
    */
  implicit def intsHaveMoments(rv: Stochastic[Int]): Moments[Int] = new IntMoments(rv)

  /**
    * Adds moment calculation methods to boolean random variables.
    *
    * See [[moments.Moments]]. This method is needed because of double implicit resolution.
    */
  implicit def booleansHaveMoments[A](rv: Stochastic[A])(implicit f: Stochastic[A] => Stochastic[Int]): Moments[Int] = new IntMoments(rv)

}
