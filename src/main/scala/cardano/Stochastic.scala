package cardano

import breeze.linalg.DenseVector

import scala.collection.immutable.IndexedSeq
import breeze.stats.distributions.Multinomial

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

final case class StochasticChoose[+A](distribution: Dist[A]) extends Stochastic[A] {

  lazy val values: IndexedSeq[A] = distribution.map(_._1).toIndexedSeq
  lazy val sampler: Multinomial[DenseVector[Prob], Int] = Multinomial(DenseVector(distribution.map(_._2).toArray))

  def sample: A = values(sampler.sample(1).head)
}

object Stochastic {

  def coin(p: Prob = 0.5): StochasticChoose[Int] = StochasticChoose(Seq((0, 1 - p), (1, p)))

  def uniform(n: Int): StochasticChoose[Int] = {
    val p = 1.0 / n
    StochasticChoose((0 until n).map((_, p)))
  }

  def fromMass(mass: Iterable[Prob]) = StochasticChoose(mass.zipWithIndex.map{case (p, v) => (v, p)})

}