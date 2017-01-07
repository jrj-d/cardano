package cardano

import breeze.linalg.{DenseVector, QuasiTensor}
import breeze.stats.distributions.{Multinomial, RandBasis, ThreadLocalRandomGenerator}
import org.apache.commons.math3.random.{MersenneTwister, RandomGenerator}

import scala.collection.immutable.IndexedSeq

trait Stochastic[+A] {

  def map[B](f: A => B): Stochastic[B] = StochasticMap(this, f)

  def flatMap[B](f: A => Stochastic[B]): Stochastic[B] = StochasticFlatMap(this, f)

  def sample: A

}

final case class StochasticMap[A, +B](stochastic: Stochastic[A], f: A => B) extends Stochastic[B] {
  def sample: B = f(stochastic.sample)
}

final case class StochasticFlatMap[A, +B](stochastic: Stochastic[A], f: A => Stochastic[B]) extends Stochastic[B] {
  def sample: B = f(stochastic.sample).sample
}

class StochasticGenerator(randomGenerator: RandomGenerator) {

  def choose[A](distribution: Dist[A])(implicit ev: DenseVector[Double] => QuasiTensor[Int, Double], sumImpl: breeze.linalg.sum.Impl[DenseVector[Double], Double]) = new Stochastic[A] {

    private lazy val values: IndexedSeq[A] = distribution.map(_._1).toIndexedSeq
    private lazy val sampler: Multinomial[DenseVector[Prob], Int] = Multinomial(DenseVector(distribution.map(_._2).toArray))(ev, sumImpl, new RandBasis(new ThreadLocalRandomGenerator(randomGenerator)))

    def sample: A = values(sampler.sample(1).head)

  }

  def coin(p: Prob = 0.5): Stochastic[Int] = choose(Seq((0, 1 - p), (1, p)))

  def uniform(n: Int): Stochastic[Int] = {
    val p = 1.0 / n
    choose((0 until n).map((_, p)))
  }

  def fromMass(mass: Iterable[Prob]) = choose(mass.zipWithIndex.map{case (p, v) => (v, p)})

}

object Stochastic extends StochasticGenerator(new MersenneTwister())