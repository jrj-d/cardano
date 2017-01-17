package cardano.discrete

import breeze.linalg.{DenseVector, QuasiTensor}
import breeze.stats.distributions.{Multinomial, RandBasis, ThreadLocalRandomGenerator}
import cardano._

import scala.collection.immutable.IndexedSeq

trait DiscreteDistributions extends Distributions {

  self =>

  def choose[A](distribution: Dist[A])(implicit ev: DenseVector[Double] => QuasiTensor[Int, Double],
                                       sumImpl: breeze.linalg.sum.Impl[DenseVector[Double], Double]): Stochastic[A] = new Stochastic[A] {

    private lazy val values: IndexedSeq[A] = distribution.map(_._1).toIndexedSeq
    private lazy val sampler: Multinomial[DenseVector[Prob], Int] = Multinomial(
      DenseVector(distribution.map(_._2).toArray))(ev, sumImpl, new RandBasis(new ThreadLocalRandomGenerator(randomGenerator)))

    def sample: A = values(sampler.sample(1).head)

  }

  def discreteUniform[A](values: Seq[A]): Stochastic[A] = new Stochastic[A] {

    private lazy val indexedValues: IndexedSeq[A] = values.toIndexedSeq

    def sample: A = indexedValues(randomGenerator.nextInt(indexedValues.length))

  }

  def discreteUniform(n: Int): Stochastic[Int] = discreteUniform(0 until n)

  def fromMass(mass: Iterable[Prob]): Stochastic[Int] = choose(mass.zipWithIndex.map{case (p, v) => (v, p)})

  def coin(p: Prob = 0.5): Stochastic[Boolean] = fromMass(Seq(1 - p, p)).map(_ == 1)

  def coin: Stochastic[Boolean] = coin()

}
