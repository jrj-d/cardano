package cardano.metropolis

import cardano._
import org.apache.commons.math3.random.RandomGenerator

trait MetropolisHastingsStochastic[A] extends Stochastic[A] {

  private[this] var lastValueOption: Option[A] = None

  private[this] var sampleNumber: Int = 0

  def randomGenerator: RandomGenerator

  def sampleBurnIn: Int

  def sampleInterval: Int

  def initValue: A

  def transitionFunction(a: A): (A, Prob, Prob)

  def unnormalizedProbabilityOf(a: A): Prob

  def sample: A = {
    (0 until (sampleInterval - 1)).foreach(i => sampleInternal)
    while(sampleNumber < sampleBurnIn) sampleInternal
    sampleInternal
  }

  def sampleInternal: A = {
    val newValue = lastValueOption match {
      case None => initValue
      case Some(lastValue) => mhProcedure(lastValue)
    }
    sampleNumber += 1
    lastValueOption = Some(newValue)
    newValue
  }

  private[this] def mhProcedure(lastValue: A): A = {
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

trait MaximumEntropy[A] extends MetropolisHastingsStochastic[A] {

  def inverseTemperature: Double

  def costFunction(a: A): Double

  def unnormalizedProbabilityOf(a: A): Prob = math.exp(-inverseTemperature * costFunction(a))
}
