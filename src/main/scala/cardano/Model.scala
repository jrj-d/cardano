package cardano

import cardano.semifield.Semifield
import cardano.semifield.syntax._
import cats.Monad
import scala.annotation.tailrec

/**
  * A free monad adapted to probabilistic computing. A model contains both the prior and the likelihood in a single
  * object.
  *
  * The underlying functor is [Stochastic]. Along to the usual trampolining scala monad (Pure, Suspend, FlatMapped), a
  * new class [Model.Weight] has been added, which represents some weight/likelihood/observation applied to a random
  * variable.
  *
  * @param semifield the semifield tells how weights should be treated (probability or log-probability domain)
  * @tparam A the concrete type of the random variable
  */
sealed abstract class Model[+A](implicit val semifield: Semifield[Double]) {

  //TODO: what laws does Model verify?

  import Model.{Pure, Primitive, Weight, FlatMapped}

  /**
    * Creates a new probabilistic model by applying a stochastic function to the underlying
    * probabilistic model.
    * A sample from the prior of the new probabilistic model is a sample from the probabilistic model
    * created by applying `f` to a sample of the prior of the underlying probabilistic model.
    * The likelihood associated to a sample from the prior of the new probabilistic model is the product of
    * the likelihood of the sample of the prior of the underlying model and the likelihood comming out of `f`.
    *
    * @param f the stochastic function to apply
    * @tparam B the concrete type of the output of stochastic function `f` and of the new probabilistic model
    * @return a probabilistic model
    */
  def flatMap[B](f: A => Model[B]): Model[B] = Model.FlatMapped(this, f)

  /**
    * Creates a new probabilistic model by applying a deterministic function
    * to the underlying probabilistic model.
    * Samples from the prior of the new probablistic variable will be samples from the underlying one
    * to which function `f` is applied.
    *
    * @param f the deterministic function to apply
    * @tparam B the output type of function `f` and the concrete type of the new probabilistic model
    * @return a probabilistic model
    */
  def map[B](f: A => B): Model[B] = flatMap(a => Model.pure(f(a)))

  /**
    * Associates a likelihood function to the probabilistic model.
    *
    * @param weight the likelihood function
    * @return a new probabilistic model with the likelihood mixed in.
    */
  def weight(weight: A => Double): Model[A] = this match {
    case Model.Weight(m, w) => Model.Weight(m, (a: A) => w(a) |*| weight(a))
    case _ => Model.Weight(this, weight)
  }

  /**
    * Evaluates a single layer of the free monad.
    *
    * Unless you implement some inference algorithm, you should not need to use [step].
    *
    * The function can be compared to [cats.Free.resume]. It is more complex because of the Weight case class,
    * that imposes digging one layer below into the free monad (3 levels here, [cats.Free.resume] has 2).
    *
    * The function could be simplified by separating it into several pieces. This is unfortunately not possible because
    * Scala does not support double tail recursion yet.
    *
    * @return a stochastic of (Double, Model[A]) if the free monad is not entirely consumed, a (Double, A) otherwise
    */
  @tailrec
  final def step: Either[Stochastic[(Double, Model[A])], (Double, A)] = this match { // scalastyle:ignore
    case Pure(a) => Right((semifield.unit, a))
    case Primitive(s) => Left(s.map(a => (semifield.unit, Pure(a))))
    case Weight(inner0, w) => inner0 match {
      case Pure(a) => Right((w(a), a))
      case Primitive(dist) => Left(dist.map(a => (w(a), Pure(a))))
      case FlatMapped(inner1, g) => inner1.flatMap(a => g(a).weight(w)).step
      case Weight(_, _) => throw new RuntimeException("Consecutive Weight's case classes, not possible")
    }
    case FlatMapped(inner0, f) => inner0 match {
      case Pure(a) => f(a).step
      case Primitive(s) => Left(s.map(a => (semifield.unit, f(a))))
      case FlatMapped(inner1, g) => inner1.flatMap { a => g(a).flatMap(f) }.step
      // case Weight(d, w) => d.flatMap { a => f(a).weight(_ => w(a))}.step
      // This is kept here to show what could have been done to replace the further digging into the free monad,
      // without introducing bugs. However, it has the effect to push all weight evaluations at the end; this would
      // break the whole purpose of SMC (to cite only one example).
      case Weight(inner1, w) => inner1 match {
        case Pure(a) => Left(Stochastic.pure((w(a), f(a))))
        case Primitive(dist) => Left(dist.map(a => (w(a), f(a))))
        case FlatMapped(inner2, g) => inner2.flatMap(a => g(a).weight(w).flatMap(f)).step
        case Weight(_, _) => throw new RuntimeException("Consecutive Weight's case classes, not possible")
      }
    }
  }

}


object Model {

  /**
    * A pure value in the free monad.
    */
  case class Pure[A](a: A)(implicit semifield: Semifield[Double]) extends Model[A]

  /**
    * A random variable lifted into the free monad. In functional parlance, a suspension.
    */
  case class Primitive[A](dist: Stochastic[A])(implicit semifield: Semifield[Double]) extends Model[A]

  /**
    * A monadic bind and and same meaning as [Stochastic.flatMap].
    */
  case class FlatMapped[A, B](underlying: Model[A], f: A => Model[B])(implicit semifield: Semifield[Double]) extends Model[B]

  /**
    * A class not in the classical free monad structure that represents a likelihood function applied to the underlying
    * random variable.
    */
  case class Weight[A](underlying: Model[A], f: A => Double)(implicit semifield: Semifield[Double]) extends Model[A]

  /**
    * Lifts a pure value into a probabilistic model.
    */
  def pure[A](value: A)(implicit semifield: Semifield[Double]): Model[A] = Pure(value)

  /**
    * Lifts a random variable into a probabilistic model.
    */
  def primitive[A](dist: Stochastic[A])(implicit semifield: Semifield[Double]): Model[A] = Primitive(dist)

  /**
    * A probabilistic model is a monad.
    */
  implicit def monadForModel(implicit semifield: Semifield[Double]): Monad[Model] = new Monad[Model] {

    def flatMap[A, B](fa: Model[A])(f: A => Model[B]): Model[B] = fa.flatMap(f)

    override def map[A, B](fa: Model[A])(f: A => B): Model[B] = fa.map(f)

    def pure[A](a: A): Model[A] = Model.pure(a)

    def tailRecM[A, B](a: A)(f: A => Model[Either[A, B]]): Model[B] =
      flatMap(f(a)) {
        case Left(a1) => tailRecM(a1)(f)
        case Right(b) => pure(b)
      }

  }

}
