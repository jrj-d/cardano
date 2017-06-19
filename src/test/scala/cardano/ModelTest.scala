package cardano

import org.scalatest._
import cardano.inference.prior
import org.apache.commons.math3.random.MersenneTwister

class ModelTest extends FlatSpec with Matchers {

  "Model prior and Stochastic" should "output same samples" in {

    import cardano.semifield.logprob._

    val stochastic: Stochastic[Double] = Stochastic.gaussian(3.0, 2.0).map(_ > 2.5)
        .flatMap { b => if(b) Stochastic.beta(3.0, 5.0) else Stochastic.pure(0.8) }

    val model: Model[Double] = Stochastic.gaussian(3.0, 2.0).l.map(_ > 2.5).weight(b => if(b) 4.0 else 3.0)
      .flatMap { b => if(b) Stochastic.beta(3.0, 5.0).l.weight(v => v) else Model.pure(0.8).weight(_ => 0.0) }.weight(v => v * 2)

    val modelPrior: Stochastic[Double] = prior(model).map(_._2)

    val random0 = new MersenneTwister(42)
    val random1 = new MersenneTwister(42)

    Seq.fill(100)(stochastic.sample(random0)) should equal (Seq.fill(100)(modelPrior.sample(random1)))
  }

}
