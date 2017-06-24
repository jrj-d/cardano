package cardano

import org.scalatest._
import cardano.inference.base.prior
import org.apache.commons.math3.random.{MersenneTwister, RandomGenerator}
import cardano.semifield.logprob._

import scala.annotation.tailrec

class ModelTest extends FlatSpec with Matchers {

  "Model prior and Stochastic" should "output same samples (1)" in {

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

    val stochastic: Stochastic[Double] = Stochastic.gaussian(3.0, 2.0)

    val model: Model[Double] = Stochastic.gaussian(3.0, 2.0).l

    val modelPrior: Stochastic[Double] = prior(model).map(_._2)

    val random0 = new MersenneTwister(42)
    val random1 = new MersenneTwister(42)

    Seq.fill(100)(stochastic.sample(random0)) should equal (Seq.fill(100)(modelPrior.sample(random1)))
  }

  "Model prior and Stochastic" should "output same samples (3)" in {

    val stochastic: Stochastic[Double] = Stochastic.pure(3.0).flatMap(a => Stochastic.gaussian(a, 2.0))

    val model: Model[Double] = Model.pure(3.0).flatMap(a => Stochastic.gaussian(a, 2.0).l)

    val modelPrior: Stochastic[Double] = prior(model).map(_._2)

    val random0 = new MersenneTwister(42)
    val random1 = new MersenneTwister(42)

    Seq.fill(100)(stochastic.sample(random0)) should equal (Seq.fill(100)(modelPrior.sample(random1)))
  }

  "Model prior and Stochastic" should "output same samples (4)" in {

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

    implicit val random = new MersenneTwister(42)

    a [RuntimeException] should be thrownBy {
      prior(Model.Weight(Model.pure(4).weight(i => i), (i: Int) => i)).sample
    }

    a [RuntimeException] should be thrownBy {
      prior(Model.Weight(Model.pure(4).weight(i => i), (i: Int) => i).flatMap(i => Model.pure(i))).sample
    }
  }

  @tailrec final def loop(acc: StringBuilder, m: Model[Int])(implicit random: RandomGenerator): Int = {
    acc += '|'
    m.step match {
      case Left(s) =>
        val (_, newModel) = s.sample
        loop(acc, newModel)
      case Right((_, a)) => a
    }
  }

  def stochasticMarker(i: Int, acc: StringBuilder): Model[Int] = Stochastic { (_: RandomGenerator) =>
    acc ++= s"s$i"
    i
  }.l

  def weightMarker(acc: StringBuilder)(i: Int): Double = {
    acc ++= s"w$i"
    0.0
  }

  "Weights" should "be applied at the right time" in {

    implicit val random = new MersenneTwister(42)

    val acc = new StringBuilder

    val model: Model[Int] = for {
      i <- stochasticMarker(3, acc).weight(weightMarker(acc))
      j <- stochasticMarker(4, acc).weight(weightMarker(acc))
      k <- stochasticMarker(i + j, acc).weight(weightMarker(acc))
    } yield k

    loop(acc, model)

    acc.toString shouldEqual "|s3w3|s4w4|s7w7|"
  }

  "Weighting" should "not change a thing when applied left or right hand side" in {

    implicit val random = new MersenneTwister(42)

    val acc1 = new StringBuilder
    val model1: Model[Int] = stochasticMarker(1, acc1)
      .flatMap(i => stochasticMarker(i + 1, acc1))
      .weight(weightMarker(acc1))
    loop(acc1, model1)

    val acc2 = new StringBuilder
    val model2: Model[Int] = stochasticMarker(1, acc2)
      .flatMap(i => stochasticMarker(i + 1, acc2))
      .weight(weightMarker(acc2))
    loop(acc2, model2)

    acc1.toString shouldEqual "|s1|s2w2|"
    acc2.toString shouldEqual "|s1|s2w2|"
  }

  "Model" should "not cause stack overflows (contrary to Stochastic)" in {

    implicit val random = new MersenneTwister(42)

    def loopM(acc: Model[Double], n: Int): Model[Double] = n match {
      case 0 => acc
      case i => loopM(acc.flatMap(a => Stochastic.gaussian.l.map(a + _)), i - 1)
    }

    prior(loopM(Model.pure(0.0), 100000)).sample
  }

}
