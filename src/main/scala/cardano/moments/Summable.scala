package cardano.moments

import cardano.semifield.Semifield

trait Summable[-A] {
  def convert[A1 <: A](a: A1)(implicit semifield: Semifield[Double]): (Double, Double)
}

object Summable {
  implicit def numericIsSummable[A](implicit num: Numeric[A]): Summable[A] = new Summable[A] {
    def convert[A1 <: A](a: A1)(implicit semifield: Semifield[Double]): (Double, Double) = (semifield.unit, num.toDouble(a))
  }

  implicit val booleanIsSummable: Summable[Boolean] = new Summable[Boolean] {
    def convert[A1 <: Boolean](a: A1)(implicit semifield: Semifield[Double]): (Double, Double) = (semifield.unit, if(a) 1.0 else 0.0)
  }

  implicit def weightedNumericIsSummable[A](implicit num: Numeric[A]): Summable[(Double, A)] = new Summable[(Double, A)] {
    def convert[T <: (Double, A)](wa: T)(implicit semifield: Semifield[Double]): (Double, Double) = (wa._1, num.toDouble(wa._2))
  }

  implicit val weightedBooleanIsSummable: Summable[(Double, Boolean)] = new Summable[(Double, Boolean)] {
    def convert[T <: (Double, Boolean)](wa: T)(implicit semifield: Semifield[Double]): (Double, Double) = (wa._1, if(wa._2) 1.0 else 0.0)
  }
}

