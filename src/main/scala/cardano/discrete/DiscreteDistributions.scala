package cardano.discrete

import breeze.linalg.{DenseVector, QuasiTensor}
import breeze.stats.distributions.{Multinomial, RandBasis, ThreadLocalRandomGenerator}
import cardano._

import scala.collection.immutable.IndexedSeq

/**
  * This trait implements some standard discrete distributions.
  */
trait DiscreteDistributions extends Distributions {

  self =>

  /**
    * Creates a random variable that samples from the discrete distribution given as input.
    *
    * @param distribution a discrete distribution given as a (finite) sequence of (value, probability) pairs
    * @param ev Breeze internals
    * @param sumImpl Breeze internals
    * @tparam A the concrete type of the random variable
    * @return a random variable that samples from the discrete distribution given as input
    */
  def choose[A](distribution: Seq[(A, Prob)])
               (implicit ev: DenseVector[Double] => QuasiTensor[Int, Double],
                sumImpl: breeze.linalg.sum.Impl[DenseVector[Double], Double]): Stochastic[A] = new Stochastic[A] {

    private lazy val values: IndexedSeq[A] = distribution.map(_._1).toIndexedSeq
    private lazy val sampler: Multinomial[DenseVector[Prob], Int] = Multinomial(
      DenseVector(distribution.map(_._2).toArray))(ev, sumImpl, new RandBasis(new ThreadLocalRandomGenerator(randomGenerator)))

    def sample: A = values(sampler.sample(1).head)

  }

  /**
    * Creates a random variable that samples uniformly from the values given as input.
    *
    * @param values values from which to sample
    * @tparam A the concrete type of the random variable
    * @return a random variable that samples uniformly from the values given as input
    */
  def discreteUniform[A](values: Seq[A]): Stochastic[A] = new Stochastic[A] {

    private lazy val indexedValues: IndexedSeq[A] = values.toIndexedSeq

    def sample: A = indexedValues(randomGenerator.nextInt(indexedValues.length))

  }

  /**
    * Creates a random variable that samples uniformly from `0` to `n` excluded.
    *
    * @param n the number of values that the random variables can take
    * @return a random variable that samples uniformly from `0` to `n` excluded
    */
  def discreteUniform(n: Int): Stochastic[Int] = discreteUniform(0 until n)

  /**
    * Creates a random variable that samples from the first natural numbers with probabilities given by `mass`.
    *
    * @param mass a discrete distribution on the first natural numbers given as probabilities
    * @return a random variable that samples from the first natural numbers with probabilities given by `mass`
    */
  def fromMass(mass: Seq[Prob]): Stochastic[Int] = choose(mass.zipWithIndex.map{case (p, v) => (v, p)})

  /**
    * Creates a Bernoulli random variable.
    *
    * @param p the probability of a positive outcome
    * @return a Bernoulli random variable
    */
  def coin(p: Prob = 0.5): Stochastic[Boolean] = fromMass(Seq(1 - p, p)).map(_ == 1)

  /**
    * See [[coin]].
    */
  def coin: Stochastic[Boolean] = coin()

}
