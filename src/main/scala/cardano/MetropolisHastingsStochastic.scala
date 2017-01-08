package cardano

import org.apache.commons.math3.random.RandomGenerator

trait MetropolisHastingsStochastic[A] extends Stochastic[A] {

  private[this] var lastValueOption: Option[A] = None

  def randomGenerator: RandomGenerator

  def initValue: A

  def transitionFunction(a: A): (A, Prob, Prob)

  def unnormalizedProbabilityOf(a: A): Prob

  def sample: A = {
    val newValue = lastValueOption match {
      case None => initValue
      case Some(lastValue) => sampleInternal(lastValue)
    }
    lastValueOption = Some(newValue)
    newValue
  }

  private[this] def sampleInternal(lastValue: A): A = {
    val (newValue, forwardProb, backwardProb) = transitionFunction(lastValue)
    val probLast = unnormalizedProbabilityOf(lastValue)
    val probNew = unnormalizedProbabilityOf(newValue)
    val acceptanceProb = probNew * backwardProb / probLast / forwardProb
    if(acceptanceProb >= 1.0) {
      newValue
    } else {
      val uniform = randomGenerator.nextDouble()
      if (uniform <= acceptanceProb) {
        newValue
      } else {
        lastValue
      }
    }
  }

}

trait SymmetricMetropolisHastingsStochastic[A] extends MetropolisHastingsStochastic[A] {

  def transitionFunction(a: A): (A, Prob, Prob) = (symmetricTransitionFunction(a), 1.0, 1.0)

  def symmetricTransitionFunction(a: A): A

}

abstract class MaximumEntropy[A](inverseTemperature: Double) extends MetropolisHastingsStochastic[A] {

  def costFunction(a: A): Double

  def unnormalizedProbabilityOf(a: A): Prob = math.exp(-inverseTemperature * costFunction(a))
}
