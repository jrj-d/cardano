package cardano.inference

import cardano.{Model, Stochastic}
import org.apache.commons.math3.random.RandomGenerator
import cardano.semifield.syntax._

import scala.annotation.tailrec

trait Base {

  /**
    * Runs a model to completion a model by extracting data from a random variable.
    *
    * Inspired by [cats.Free.go].
    *
    * @param model the model to consume
    * @param f the function extracting data from a random variable
    * @tparam A the concrete type of the model
    * @return a tuple of type `(Double, A)`
    */
  def consume[A](model: Model[A], f: Stochastic[(Double, Model[A])] => (Double, Model[A])): (Double, A) = {

    implicit val semifield = model.semifield

    @tailrec def loop(weight: Double, m: Model[A]): (Double, A) = {
      m.step match {
        case Left(s) =>
          val (newWeight, newModel) = f(s)
          loop(newWeight |*| weight, newModel)
        case Right((finalWeight, a)) => (finalWeight |*| weight, a)
      }
    }

    loop(semifield.unit, model)
  }

  /**
    * Transforms a probabilistic model into a random variable that samples value from the prior along with the
    * likelihood.
    *
    *
    * If the model describes a prior `P(X)` and a likelihood `P(Y=y | X)`, then applying [prior] and sampling
    * from it will return a sample `x ~ P(X)` and the associated likelihood `P(Y=y | X=x)`.
    *
    * This is the standard way to sample from a [Model].
    *
    * {{{
    *   prior(model).sample
    * }}}
    *
    * @param model the probabilistic model to transform
    * @tparam A the concrete type of the probabilistic model
    * @return a random variable that samples tuples of type `(Double, A)` where the second term is a sample from the
    *         model's prior, and the first term is the associated likelihood
    */
  def prior[A](model: Model[A]): Stochastic[(Double, A)] =
    Stochastic((random: RandomGenerator) => consume(model, (s: Stochastic[(Double, Model[A])]) => s.sample(random)))

  /**
    * Transforms a probabilistic model (prior + likelihood) into another probabilistic model whose prior samples
    * i.i.d instances of the prior of the initial model weighted by likelihoods of the initial model,
    * and whose likelihood is an unbiased estimate of the marginal likelihood of the initial model.
    *
    * If the model describes a prior `P(X)` and a likelihood `P(Y=y | X)`, then applying [marginalize] and
    * sampling from it will return a [List] of samples from `x ~ P(X)` weighted with `P(X=x | Y=y)`. The [List] is
    * weighted by an unbiased estimate of `P(Y=y)` that has been constructed using those samples.
    *
    * The unbiased estimate of `P(Y=y)` may be far off depending on the structure of the probabilistic model.
    * [sequentialMonteCarlo] can help get a better estimate if factors of the likelihood appear at different stages in
    * the model, as in a state-space model for instance.
    *
    * @param model the probabilistic model to marginalize
    * @param nSamples the number of i.i.d instances to sample from the prior
    * @tparam A the concrete type of the probability model
    * @return a probabilistic model as described above
    */
  def marginalize[A](model: Model[A], nSamples: Int): Model[List[(Double, A)]] = {
    implicit val semifield = model.semifield

    prior(model).repeat(List.fill[(Double, A)](nSamples)).l.weight { weightedSamples =>
      weightedSamples.map(_._1).reduce(_ |+| _) |/| semifield.inject(nSamples)
    }
  }

}
