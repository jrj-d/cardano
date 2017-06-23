package cardano.discrete

import cardano._
import cardano.semifield.Semifield
import cardano.semifield.syntax._
import org.apache.commons.math3.random.RandomGenerator

import scala.collection.immutable.IndexedSeq
import scala.annotation.tailrec

/**
  * This trait implements some standard discrete distributions.
  */
trait DiscreteDistributions {

  /**
    * Creates a random variable that samples from the discrete distribution given as input.
    *
    * @param distribution a discrete distribution given as a (finite) sequence of (probability, value) pairs
    * @param semifield a semifield indicating how to interpret weights (probability or log-probability domain)
    * @tparam A the concrete type of the random variable
    * @return a random variable that samples from the discrete distribution given as input
    */
  def choose[A](distribution: Seq[(Double, A)])(implicit semifield: Semifield[Double]): Stochastic[A] = new Stochastic[A] {

    private val n: Int = distribution.length
    private val sum: Double = distribution.map(_._1).reduce(_ |+| _)
    private val indexedValues: IndexedSeq[A] = distribution.map(_._2).toIndexedSeq
    private lazy val cumulativeValues: IndexedSeq[Double] = distribution.map(_._1).scanLeft(semifield.empty)(_ |+| _).tail.toIndexedSeq
    private var firstSample: Boolean = true

    def sampleLinear(random: RandomGenerator): Int = {
      val threshold = sum |*| semifield.inject(random.nextDouble)
      var prob = semifield.empty
      var i = -1
      for (w <- distribution.map(_._1)) {
        i += 1
        prob = prob |+| w
        if (prob >= threshold) return i // scalastyle:ignore
      }
      throw new RuntimeException("Sampling failed")
    }

    def sampleBinarySearch(random: RandomGenerator): Int = {

      @tailrec def search(value: Double, min: Int, max: Int): Int = max - min match {
        case l if l <= 1 => max
        case _ =>
          val middle = (min + max) / 2
          if(value <= cumulativeValues(middle)) {
            search(value, min, middle)
          } else {
            search(value, middle, max)
          }
      }

      val value = sum |*| semifield.inject(random.nextDouble)
      if(value <= cumulativeValues(0)) {
        0
      } else {
        search(value, 0, n - 1)
      }
    }

    def sample(implicit random: RandomGenerator): A = {
      val pos: Int = if(firstSample) {
        firstSample = false
        sampleLinear(random)
      } else {
        sampleBinarySearch(random)
      }
      indexedValues(pos)
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
    * @param semifield a semifield indicating how to interpret weights (probability or log-probability domain)
    * @return a random variable that samples from the first natural numbers with probabilities given by `mass`
    */
  def fromMass(mass: Seq[Double])(implicit semifield: Semifield[Double]): Stochastic[Int] = choose(mass.zipWithIndex)

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
