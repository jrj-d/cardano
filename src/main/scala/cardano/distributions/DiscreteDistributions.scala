package cardano.distributions

import breeze.linalg.{DenseVector, QuasiTensor}
import breeze.stats.distributions.{Multinomial, RandBasis, ThreadLocalRandomGenerator}
import cardano._

import scala.collection.immutable.IndexedSeq

trait DiscreteDistributions extends Distributions {

  def choose[A](distribution: Dist[A])(
    implicit ev: DenseVector[Double] => QuasiTensor[Int, Double],
    sumImpl: breeze.linalg.sum.Impl[DenseVector[Double], Double]): Stochastic[A] = new Stochastic[A] {

    private lazy val values: IndexedSeq[A] = distribution.map(_._1).toIndexedSeq
    private lazy val sampler: Multinomial[DenseVector[Prob], Int] = Multinomial(
      DenseVector(distribution.map(_._2).toArray))(ev, sumImpl, new RandBasis(new ThreadLocalRandomGenerator(randomGenerator)))

    def sample: A = values(sampler.sample(1).head)

  }

  def fromMass(mass: Iterable[Prob]): Stochastic[Int] = choose(mass.zipWithIndex.map{case (p, v) => (v, p)})

  def uniform(n: Int): Stochastic[Int] = {
    val p = 1.0 / n
    fromMass(Seq.fill(n)(p))
  }

  def coin(p: Prob = 0.5): Stochastic[Int] = fromMass(Seq(1 - p, p))

  def coin: Stochastic[Int] = fromMass(Seq(0.5, 0.5))

}
