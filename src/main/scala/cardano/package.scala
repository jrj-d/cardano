package object cardano {

  type Prob = Double
  type Dist[+A] = Iterable[(A, Double)]

  implicit def randVarHasNumericOps[A](rv: Stochastic[A])(implicit numeric: Numeric[A]) = new NumericOps(rv)

}