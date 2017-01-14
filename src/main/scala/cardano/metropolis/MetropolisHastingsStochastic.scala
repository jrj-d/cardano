package cardano.metropolis

import cardano._
import org.apache.commons.math3.random.RandomGenerator

trait MetropolisHastingsStochastic[A] extends Stochastic[A] {

  private[this] var lastValueOption: Option[A] = None

  private[this] var sampleNumber: Int = 0

  def randomGenerator: RandomGenerator

  def sampleBurnIn: Int = defaultSampleBurnIn

  def sampleInterval: Int = defaultSampleInterval

  def initValue: A

  def logTransitionFunction(a: A): (A, Double, Double)

  def logUnnormalizedProbabilityOf(a: A): Double

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
    val (newValue, forwardProbLog, backwardProbLog) = logTransitionFunction(lastValue)
    val probLastLog = logUnnormalizedProbabilityOf(lastValue)
    val probNewLog = logUnnormalizedProbabilityOf(newValue)
    val acceptanceProbLog = probNewLog + backwardProbLog - probLastLog - forwardProbLog
    if(acceptanceProbLog >= 0.0) {
      newValue
    } else {
      val uniform = math.log(randomGenerator.nextDouble())
      if (uniform <= acceptanceProbLog) {
        newValue
      } else {
        lastValue
      }
    }
  }

}

trait MaximumEntropy[A] extends MetropolisHastingsStochastic[A] {

  def inverseTemperature: Double

  def costFunction(a: A): Double

  def logUnnormalizedProbabilityOf(a: A): Prob = -inverseTemperature * costFunction(a)
}
