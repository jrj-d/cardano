package object cardano {

  type Prob = Double
  type Dist[+A] = Iterable[(A, Double)]

  implicit def numericStochasticsHaveMoments[A](rv: Stochastic[A])(implicit numeric: Numeric[A]) = new Moments(rv)

}