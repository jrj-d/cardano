package cardano

import cardano.moments.MomentsFunctions
import cardano.semifield.Semifield
import cats.Monad
import org.apache.commons.math3.random.RandomGenerator

import scala.language.{higherKinds, implicitConversions}

/**
  * A random variable of concrete type `A`.
  *
  * @tparam A the concrete type of the random variable
  */
trait Stochastic[+A] extends MomentsFunctions[A] {

  override def toString: String = "Stochastic(...)"

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
  def map[B](f: A => B): Stochastic[B] = Stochastic((random: RandomGenerator) => f(this.sample(random)))

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
  def flatMap[B](f: A => Stochastic[B]): Stochastic[B] = Stochastic(
    (random: RandomGenerator) => f(this.sample(random)).sample(random)
  )

  /**
    * Creates a new random variable by filtering the values of the underlying random variable.
    * This implements rejection sampling.
    * Same as `filter`, but needed for `for`-comprehensions.
    *
    * @param f the filter function
    * @return a random variable
    */
  def withFilter(f: A => Boolean): Stochastic[A] = Stochastic((random: RandomGenerator) => {
    Stream.continually(this.sample(random)).dropWhile(a => !f(a)).head
  })

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
    * This can be used to create sequences or even streams of random variables.
    *
    * @param f the function that performs the lifting
    * @tparam B the type of the new random variable (mainly for covariance)
    * @tparam F the type of the collection or higher-kinded type
    * @return a random variable
    */
  def repeat[B >: A, F[_]](f: (=> B) => F[B]): Stochastic[F[B]] = Stochastic((random: RandomGenerator) => f(this.sample(random)))

  /**
    * Creates a new random variable which represents a Markov chain of the first order.
    * The relationship between two consecutive terms in the chain is defined by the
    * stochastic function `f`.
    *
    * @param f the distribution of the next term given the previous one
    * @tparam B the type of the next term (mainly for covariance)
    * @return a random variable in the space of infinite sequences of `B`.
    */
  def markov[B >: A](f: B => Stochastic[B]): Stochastic[Stream[B]] = Stochastic((random: RandomGenerator) => {
    lazy val chain: Stream[B] = this.sample(random) #:: chain.map(f(_).sample(random))
    chain
  })

  /**
    * Lifts the random variable into a model to perform inference
    *
    * @tparam B the type of the model (mainly for covariance)
    * @return a model equivalent to the random variable, i.e. whose prior is the random variable and without any
    *         likelihood mixed in
    */
  def lifted[B >: A](implicit semifield: Semifield[Double]): Model[B] = Model.primitive(this)

  /**
    * Alias for [lifted].
    */
  def l[B >: A](implicit semifield: Semifield[Double]): Model[B] = lifted[B]

  /**
    * Returns a sample from the random variable.
    *
    * @return a sample from the random variable
    */
  def sample(implicit random: RandomGenerator): A

}

/**
  * The main entry point to [[cardano]]. All standard distributions implemented in [[cardano]] can be
  * accessed from here.
  *
  * The seed is automatically set using the current timestamp; if you wish to set the seed yourself,
  * have a look at [[Distributions]].
  */
object Stochastic extends Distributions {

  def apply[A](f: RandomGenerator => A): Stochastic[A] = new Stochastic[A] {
    def sample(implicit randomGenerator: RandomGenerator): A = f(randomGenerator)
  }

  /**
    * Creates a random variable that is constant.
    *
    * @param a the value of this constant random variable
    * @tparam A the concrete type of this random variable
    * @return a constant random variable
    */
  def pure[A](a: A): Stochastic[A] = Stochastic((_: RandomGenerator) => a)

  implicit def monadForStochastic: Monad[Stochastic] = new Monad[Stochastic] {

    def flatMap[A, B](fa: Stochastic[A])(f: A => Stochastic[B]): Stochastic[B] = fa.flatMap(f)

    override def map[A, B](fa: Stochastic[A])(f: A => B): Stochastic[B] = fa.map(f)

    def pure[A](a: A): Stochastic[A] = Stochastic.pure(a)

    def tailRecM[A, B](a: A)(f: A => Stochastic[Either[A, B]]): Stochastic[B] =
      f(a).flatMap {
        case Left(a1) => tailRecM(a1)(f)
        case Right(b) => pure(b)
      }

  }

}
