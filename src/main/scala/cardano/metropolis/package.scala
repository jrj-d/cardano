package cardano

package object metropolis {

  /**
    * Default burn-in value for Metropolis-Hastings algorithm.
    */
  val defaultSampleBurnIn: Int = 100

  /**
    * Default interval between samples for Metropolis-Hastings algorithm.
    */
  private[cardano] val defaultSampleInterval: Int = 10
}
