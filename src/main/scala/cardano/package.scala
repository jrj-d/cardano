import cardano.bayesian.Posterior
import cardano.moments.{DoubleMoments, IntMoments, Moments, NumericMoments}

package object cardano {

  type Prob = Double
  type Dist[+A] = Iterable[(A, Double)]

  implicit def booleanIsInteger(rv: Stochastic[Boolean]): Stochastic[Int] = rv.map(b => if(b) 1 else 0)

  implicit def numericsHaveMoments[A](rv: Stochastic[A])(implicit numeric: Numeric[A]): Moments[A] = new NumericMoments(rv)
  implicit def doublesHaveMoments(rv: Stochastic[Double]): Moments[Double] = new DoubleMoments(rv)
  implicit def intsHaveMoments(rv: Stochastic[Int]): Moments[Int] = new IntMoments(rv)

  implicit def canPerformBayesianInference[A](rv: Stochastic[A]): Posterior[A] = new Posterior(rv)
}
