package cardano

import org.scalatest._
import cardano.inference.prior
import org.apache.commons.math3.random.{MersenneTwister, RandomGenerator}
import cardano.semifield.syntax._

import scala.annotation.tailrec

class ModelTest extends FlatSpec with Matchers {

  "Model prior and Stochastic" should "output same samples (1)" in {

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

  "Model prior and Stochastic" should "output same samples (2)" in {

    import cardano.semifield.logprob._

    val stochastic: Stochastic[Double] = Stochastic.gaussian(3.0, 2.0)

    val model: Model[Double] = Stochastic.gaussian(3.0, 2.0).l

    val modelPrior: Stochastic[Double] = prior(model).map(_._2)

    val random0 = new MersenneTwister(42)
    val random1 = new MersenneTwister(42)

    Seq.fill(100)(stochastic.sample(random0)) should equal (Seq.fill(100)(modelPrior.sample(random1)))
  }

  "Model prior and Stochastic" should "output same samples (3)" in {

    import cardano.semifield.logprob._

    val stochastic: Stochastic[Double] = Stochastic.pure(3.0).flatMap(a => Stochastic.gaussian(a, 2.0))

    val model: Model[Double] = Model.pure(3.0).flatMap(a => Stochastic.gaussian(a, 2.0).l)

    val modelPrior: Stochastic[Double] = prior(model).map(_._2)

    val random0 = new MersenneTwister(42)
    val random1 = new MersenneTwister(42)

    Seq.fill(100)(stochastic.sample(random0)) should equal (Seq.fill(100)(modelPrior.sample(random1)))
  }

  "Model prior and Stochastic" should "output same samples (4)" in {

    import cardano.semifield.logprob._

    val stochastic: Stochastic[Double] = Stochastic.gaussian(3.0, 2.0)
      .flatMap(a => Stochastic.gaussian(a, 2.0))
      .flatMap(a => Stochastic.gaussian(a, 2.0))

    val model: Model[Double] = Stochastic.gaussian(3.0, 2.0).l
      .flatMap(a => Stochastic.gaussian(a, 2.0).l)
      .flatMap(a => Stochastic.gaussian(a, 2.0).l)

    val modelPrior: Stochastic[Double] = prior(model).map(_._2)

    val random0 = new MersenneTwister(42)
    val random1 = new MersenneTwister(42)

    Seq.fill(100)(stochastic.sample(random0)) should equal (Seq.fill(100)(modelPrior.sample(random1)))
  }

  "Model prior and Stochastic" should "output same samples (5)" in {

    import cardano.semifield.logprob._

    val stochastic: Stochastic[Double] = Stochastic.gaussian(3.0, 2.0)
      .flatMap(a => Stochastic.gaussian(a, 2.0))

    val model: Model[Double] = Stochastic.gaussian(3.0, 2.0).l
      .weight(v => v)
      .flatMap(a => Stochastic.gaussian(a, 2.0).l)

    val modelPrior: Stochastic[Double] = prior(model).map(_._2)

    val random0 = new MersenneTwister(42)
    val random1 = new MersenneTwister(42)

    Seq.fill(100)(stochastic.sample(random0)) should equal (Seq.fill(100)(modelPrior.sample(random1)))
  }

  "Model" should "not support consecutive weights" in {

    import cardano.semifield.logprob._

    implicit val random = new MersenneTwister(42)

    a [RuntimeException] should be thrownBy {
      prior(Model.Weight(Model.pure(4).weight(i => i), (i: Int) => i)).sample
    }

    a [RuntimeException] should be thrownBy {
      prior(Model.Weight(Model.pure(4).weight(i => i), (i: Int) => i).flatMap(i => Model.pure(i))).sample
    }
  }

  "Weights" should "be applied at the right time" in {

    import cardano.semifield.logprob._

    implicit val random = new MersenneTwister(42)

    def stochasticMarker(i: Int, acc: StringBuilder): Model[Int] = Stochastic { (_: RandomGenerator) =>
      acc ++= s"s$i"
      i
    }.l

    def weightMarker(acc: StringBuilder)(i: Int): Double = {
      acc ++= s"w$i"
      0.0
    }

    val acc = new StringBuilder

    val model: Model[Int] = for {
      i <- stochasticMarker(3, acc).weight(weightMarker(acc))
      j <- stochasticMarker(4, acc).weight(weightMarker(acc))
      k <- stochasticMarker(i + j, acc).weight(weightMarker(acc))
    } yield k

    @tailrec def loop(m: Model[Int]): Int = {
      acc += '|'
      m.step match {
        case Left(s) =>
          val (_, newModel) = s.sample
          loop(newModel)
        case Right((_, a)) => a
      }
    }

    loop(model)

    acc.toString shouldEqual "|s3w3|s4w4|s7w7|"
  }

}
