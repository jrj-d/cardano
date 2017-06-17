package cardano.moments

import cardano.Stochastic
import cardano.semifield.Semifield
import cardano.semifield.syntax._

trait MomentsFunctions[+A] {

  self: Stochastic[A] =>

  /**
    * Returns a random variable that is the empirical mean with `n` elements.
    *
    * 1. If `A` is numeric, the random variable is repeated `n` times and averaged.
    *
    * 2. If `A` is of the form `(Double, B)` with B numeric, it is assumed that the random variable is the second element
    *    of the tuple, and that the first one is a weight. A weighted average is computed with repeating the variable
    *    `n` times.
    *
    * 3. If `A` is of the form `F[B]` with B numeric and `F` a subtype of `TraversableOnce`, the empirical mean is the
    *    average of the `n` first elements of `F[B]`, i.e. only one sequence `F[B]` will be drawn.
    *
    * 4. If `A` is of the form `F[(Double, B)]` with B numeric and `F` a subtype of `TraversableOnce`, the empirical mean
    *    is the weighted average of the `n` first elements of type `B` inside `F[(Double, B)]`, , i.e. only one sequence
    *    `F[(Double, B)]` will be drawn.
    *
    * 5. If `A` is of the form `(Double, F[(Double, B)])` or `(Double, F[B])` with B numeric and `F` a subtype of
    *    `TraversableOnce`, the first element of the outer tuple is discarded.
    *
    * Why this strange behavior with weights and sequences? Because many inference algorithms output a sequence or a
    * stream of values, sometimes weighted. With this design, if you use a Metropolis algorithm for instance and you get
    * a `Stochastic[Stream[A]]`, you can call `mean` on the random variable to get the mean of the variable you were
    * interested in.
    *
    * Note that in cases 3 to 5, if the sequence is shorter that n`, say `m`, the average of the `m` first elements
    * only is computed!
    *
    *
    * @param n the desired number of elements in the empirical mean
    * @param duplicable a case class that selects the right case (see above)
    * @param semifield a case class that selects the nature of weights (probability or log-probability domain)
    * @tparam B a supertype of `A` (mainly for covariance)
    * @return a random variable that is the empirical mean with `n` elements.
    */
  def mean[B >: A](n: Int)(implicit duplicable: Duplicable[B], semifield: Semifield[Double]): Stochastic[Double] = {
    duplicable.duplicate(this, n).map { seq =>
      val denominator = semifield.extract(seq.map(_._1).reduce(_ |+| _))
      val sum = seq.foldLeft(0.0) { case (acc, (w, a)) => acc + semifield.extract(w) * a }
      sum / denominator
    }
  }

  /**
    * Returns a random variable that is the empirical variance with `n` elements.
    *
    * @param n the desired number of elements in the empirical variance
    * @param duplicable a case class that selects the right case (see explanations in [[mean]])
    * @param semifield a case class that selects the nature of weights (probability or log-probability domain)
    * @tparam B a supertype of `A` (mainly for covariance)
    * @return a random variable that is the empirical variance with `n` elements.
    */
  def variance[B >: A](n: Int)(implicit duplicable: Duplicable[B], semifield: Semifield[Double]): Stochastic[Double] = {
    duplicable.duplicate(this, n).map { seq =>
      val denominator = semifield.extract(seq.map(_._1).reduce(_ |+| _))
      val extractedSeq = seq.map { case (w, a) => (semifield.extract(w), a)}
      val sum = extractedSeq.foldLeft(0.0) { case (acc, (w, a)) => acc + w * a }
      val mean = sum / denominator
      val deviationSum = extractedSeq.foldLeft(0.0) { case (acc, (w, a)) => acc + w * (a - mean) * (a - mean) }
      deviationSum / denominator
    }
  }

  /**
    * Returns a random variable that is the empirical standard deviation with `n` elements.
    *
    * @param n the desired number of elements in the empirical standard deviation
    * @param duplicable a case class that selects the right case (see explanations in [[mean]])
    * @param semifield a case class that selects the nature of weights (probability or log-probability domain)
    * @tparam B a supertype of `A` (mainly for covariance)
    * @return a random variable that is the empirical standard deviation with `n` elements.
    */
  def std[B >: A](n: Int)(implicit duplicable: Duplicable[B], semifield: Semifield[Double]): Stochastic[Double] =
    variance(n).map(math.sqrt)

  /**
    * Returns a random variable that is the empirical moment of order `order` with `n` elements.
    *
    * @param n the desired number of elements used to compute the empirical moment
    * @param duplicable a case class that selects the right case (see explanations in [[mean]])
    * @param semifield a case class that selects the nature of weights (probability or log-probability domain)
    * @tparam B a supertype of `A` (mainly for covariance)
    * @return a random variable that is the empirical moment of order `order` with `n` elements.
    */
  def moment[B >: A](order: Double, n: Int)(implicit duplicable: Duplicable[B], semifield: Semifield[Double]): Stochastic[Double] = {
    duplicable.duplicate(this, n).map { seq =>
      val denominator = semifield.extract(seq.map(_._1).reduce(_ |+| _))
      val extractedSeq = seq.map { case (w, a) => (semifield.extract(w), a)}
      val sum = extractedSeq.foldLeft(0.0) { case (acc, (w, a)) => acc + w * a }
      val mean = sum / denominator
      val deviationSum = extractedSeq.foldLeft(0.0) { case (acc, (w, a)) => acc + w * math.pow(a - mean, order) }
      deviationSum / denominator
    }
  }

}
