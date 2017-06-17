package cardano

import cardano.semifield.{LogProbInstance, Semifield}
import cardano.semifield.syntax._
import org.scalatest.{FlatSpec, Matchers}

class LogProbSemifieldTest extends FlatSpec with Matchers {

  implicit val semifield: Semifield[Double] = (new LogProbInstance {}).semifieldForLogProb

  "inject/extract" should "work properly" in {
    semifield.inject(math.E) should be (1.0 +- 1e-6)
    semifield.extract(0.0) should be (1.0 +- 1e-6)
    semifield.inject(semifield.extract(3.0)) should be (3.0 +- 1e-6)
    semifield.extract(semifield.inject(3.0)) should be (3.0 +- 1e-6)
  }

  "addition" should "work properly" in {
    semifield.extract(semifield.inject(3.0) |+| semifield.inject(2.0)) should be (5.0 +- 1e-6)
  }

  "multiplication" should "work properly" in {
    semifield.extract(semifield.inject(3.0) |*| semifield.inject(2.0)) should be (6.0 +- 1e-6)
  }

  "division" should "work properly" in {
    semifield.extract(semifield.inject(3.0) |/| semifield.inject(2.0)) should be (1.5 +- 1e-6)
  }

  "unit" should "be extracted to 1" in {
    semifield.extract(semifield.unit) should be (1.0 +- 1e-6)
  }

  "empty" should "be extracted to 0" in {
    semifield.extract(semifield.empty) should be (0.0 +- 1e-6)
  }

  "inverse" should "be extracted to the usual multiplicative inverse" in {
    semifield.extract(semifield.inverse(semifield.inject(2.0))) should be (0.5 +- 1e-6)
  }

}
