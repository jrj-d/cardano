package cardano.distributions

import cardano._
import org.apache.commons.math3.random.RandomGenerator

import scala.collection.immutable.IndexedSeq

trait DiscreteDistributions extends Distributions {

  self =>

  def choose[A](distribution: Dist[A]): Stochastic[A] = new Stochastic[A] {

    private lazy val values: IndexedSeq[A] = distribution.map(_._1).toIndexedSeq

    def sample: A = values(randomGenerator.nextInt(values.length))

    def randomGenerator: RandomGenerator = self.randomGenerator

  }

  def fromMass(mass: Iterable[Prob]): Stochastic[Int] = choose(mass.zipWithIndex.map{case (p, v) => (v, p)})

  def uniform(n: Int): Stochastic[Int] = {
    val p = 1.0 / n
    fromMass(Seq.fill(n)(p))
  }

  def coin(p: Prob = 0.5): Stochastic[Boolean] = fromMass(Seq(1 - p, p)).map(_ == 1)

  def coin: Stochastic[Boolean] = coin()

}
