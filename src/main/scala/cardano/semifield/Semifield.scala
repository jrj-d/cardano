package cardano.semifield

import cats.kernel.{Monoid, SemigroupFunctions}
import cats.syntax.MonoidSyntax

import scala.language.implicitConversions
import scala.{specialized => sp}

trait Semifield[@sp(Int, Long, Float, Double) A] extends Monoid[A] {
  def multiply(x: A, y: A): A

  def unit: A

  def inject(x: A): A

  def extract(x: A): A

  def inverse(x: A): A

  def divide(x: A, y: A): A = multiply(x, inverse(y))
}

abstract class SemifieldFunctions[S[T] <: Semifield[T]] extends SemigroupFunctions[S] {
  def multiply[@sp(Int, Long, Float, Double) A](x: A, y: A)(implicit ev: S[A]): A =
    ev.multiply(x, y)

  def neutral[@sp(Int, Long, Float, Double) A](implicit ev: S[A]): A =
    ev.unit

  def inject[@sp(Int, Long, Float, Double) A](x: A)(implicit ev: S[A]): A =
    ev.inject(x)

  def extract[@sp(Int, Long, Float, Double) A](x: A)(implicit ev: S[A]): A =
    ev.extract(x)

  def inverse[@sp(Int, Long, Float, Double) A](x: A)(implicit ev: S[A]): A =
    ev.inverse(x)

  def divide[@sp(Int, Long, Float, Double) A](x: A, y: A)(implicit ev: S[A]): A =
    ev.divide(x, y)
}

object Semifield extends SemifieldFunctions[Semifield] {

  /**
    * Access an implicit `Semifield[A]`.
    */
  @inline final def apply[A](implicit ev: Semifield[A]): Semifield[A] = ev
}

trait SemifieldSyntax extends MonoidSyntax {
  implicit def catsSyntaxSemifield[A: Semifield](a: A): SemifieldOps[A] =
    new SemifieldOps[A](a)
}

final class SemifieldOps[A: Semifield](lhs: A) {
  def |*|(rhs: A): A = Semifield[A].multiply(lhs, rhs)  // scalastyle:ignore
  def |/|(rhs: A): A = Semifield[A].divide(lhs, rhs)  // scalastyle:ignore
}
