package cardano.distributions

import cardano.Stochastic
import cardano.metropolis._
import org.apache.commons.math3.random.RandomGenerator

trait MaximumEntropyDistributions extends Distributions {

  self =>

  def maxEnt[A](inverseTemp: Double, burnIn: Int = defaultSampleBurnIn, interval: Int = defaultSampleInterval)(init: => A)
               (costFunc: A => Double)(symmetricTransitionFunc: A => A): Stochastic[A] = new MaximumEntropy[A] {

    override val randomGenerator: RandomGenerator = self.randomGenerator

    override val inverseTemperature: Double = inverseTemp

    override val sampleBurnIn: Int = burnIn

    override val sampleInterval: Int = interval

    override def costFunction(a: A): Double = costFunc(a)

    override def initValue: A = init

    override def logTransitionFunction(a: A): (A, Double, Double) = (symmetricTransitionFunc(a), 0.0, 0.0)

  }

}
