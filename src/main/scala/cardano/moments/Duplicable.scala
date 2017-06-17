package cardano.moments

import cardano.Stochastic
import cardano.semifield.Semifield

import scala.language.higherKinds

trait Duplicable[-A] {
  def duplicate[A1 <: A](stochastic: Stochastic[A1], n: Int)(implicit semifield: Semifield[Double]): Stochastic[Seq[(Double, Double)]]
}

object Duplicable {

  implicit def summableIsDuplicable[A](implicit summable: Summable[A]): Duplicable[A] =
    new Duplicable[A] {
      def duplicate[A1 <: A](stochastic: Stochastic[A1], n: Int)(implicit semifield: Semifield[Double]): Stochastic[Seq[(Double, Double)]] = {
        stochastic.repeat(Seq.fill[A1](n)).map(seq => seq.map(summable.convert))
      }
    }

  implicit def sequenceOfSummablesIsDuplicable[A](implicit summable: Summable[A]): Duplicable[TraversableOnce[A]] =
    new Duplicable[TraversableOnce[A]] {
      def duplicate[T <: TraversableOnce[A]](stochastic: Stochastic[T], n: Int)(implicit semifield: Semifield[Double]): Stochastic[Seq[(Double, Double)]] = {
        stochastic.map(_.toSeq.take(n).map[(Double, Double), Seq[(Double, Double)]](summable.convert))
      }
  }

  implicit def weightedSequenceOfSummablesIsDuplicable[A : Summable]
  (implicit duplicable: Duplicable[TraversableOnce[A]]): Duplicable[(Double, TraversableOnce[A])] = new Duplicable[(Double, TraversableOnce[A])] {
    def duplicate[T <: (Double, TraversableOnce[A])](stochastic: Stochastic[T], n: Int)
                                                    (implicit semifield: Semifield[Double]): Stochastic[Seq[(Double, Double)]] = {
      duplicable.duplicate(stochastic.map(_._2), n)
    }
  }
}
