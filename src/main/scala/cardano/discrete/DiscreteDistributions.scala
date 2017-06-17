package cardano.discrete

import cardano._
import org.apache.commons.math3.random.RandomGenerator

import scala.collection.immutable.IndexedSeq

/**
  * This trait implements some standard discrete distributions.
  */
trait DiscreteDistributions {

  /**
    * Creates a random variable that samples from the discrete distribution given as input.
    *
    * @param distribution a discrete distribution given as a (finite) sequence of (probability, value) pairs
    * @tparam A the concrete type of the random variable
    * @return a random variable that samples from the discrete distribution given as input
    */
  def choose[A](distribution: Seq[(Double, A)]): Stochastic[A] = new Stochastic[A] {

    private val sum: Double = distribution.map(_._1).reduce(_ + _)
    private val indexedValues: IndexedSeq[A] = distribution.map(_._2).toIndexedSeq

    def sample(implicit random: RandomGenerator): A = {
      val threshold = sum * random.nextDouble()
      var prob = 0.0
      var i = -1
      for (w <- distribution.map(_._1)) {
        i += 1
        prob = prob + w
        if (prob >= threshold) return indexedValues(i) // scalastyle:ignore
      }
      throw new RuntimeException("Sampling failed")
    }
  }

  /**
    * Creates a random variable that samples uniformly from the values given as input.
    *
    * @param values values from which to sample
    * @tparam A the concrete type of the random variable
    * @return a random variable that samples uniformly from the values given as input
    */
  def discreteUniform[A](values: Seq[A]): Stochastic[A] = new Stochastic[A] {

    private val indexedValues: IndexedSeq[A] = values.toIndexedSeq

    def sample(implicit random: RandomGenerator): A = indexedValues(random.nextInt(indexedValues.length))

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
  def fromMass(mass: Seq[Double]): Stochastic[Int] = choose(mass.zipWithIndex)

  /**
    * Creates a Bernoulli random variable.
    *
    * @param p the probability of a positive outcome
    * @return a Bernoulli random variable
    */
  def coin(p: Double = 0.5): Stochastic[Boolean] = Stochastic((random: RandomGenerator) => if (random.nextDouble <= p) true else false)

  /**
    * See [[coin]].
    */
  def coin: Stochastic[Boolean] = coin()

}
