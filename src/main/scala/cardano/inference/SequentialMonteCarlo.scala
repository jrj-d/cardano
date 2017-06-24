package cardano.inference

import cardano.{Model, Stochastic}
import cardano.semifield.syntax._
import cats.instances.list._

trait SequentialMonteCarlo {

  /**
    * Transforms a probabilistic model (prior + likelihood) into another probabilistic model whose prior samples
    * i.i.d instances of the posterior of the initial model, and whose likelihood is an unbiased estimate of the
    * marginal likelihood of the initial model.
    *
    * If the model describes a prior `P(X)` and a likelihood `P(Y=y | X)`, then applying [sequentialMonteCarlo] and
    * sampling from it will return a [List] of samples from `x ~ P(X | Y=y)` and weighted by an unbiased estimate
    * of `P(Y=y)` that has been constructed using those samples.
    *
    * The unbiased estimate of `P(Y=y)` converges rapidly if factors of the likelihood appear at different stages in
    * the model, as in a state-space model for instance.
    *
    * @param model the probabilistic model to transform
    * @param nParticles the number of i.i.d instances to sample from the posterior
    * @tparam A the concrete type of the probability model
    * @return a probabilistic model as described above
    */
  def sequentialMonteCarlo[A](model: Model[A], nParticles: Int): Model[List[A]] = {

    implicit val semifield = model.semifield

    val injectedNParticles = semifield.inject(nParticles)

    def resample[B](particles: List[(Double, B)]): Model[List[B]] = {
      val weights = particles.map(_._1)
      if(weights.forall(_ == semifield.unit)) { // no need to resample if no weight
        Model.pure(particles.map(_._2))
      } else {
        Stochastic.choose(particles)
          .repeat(List.fill[B](nParticles))
          .l.weight(_ => weights.reduce(_ |+| _) |/| injectedNParticles)
      }
    }

    def remodel(mixedParticles: List[Either[Stochastic[(Double, Model[A])], (Double, A)]]): Model[List[(Double, Model[A])]] = {
      val stochasticParticles: List[Model[(Double, Model[A])]] = mixedParticles.map {
        case Left(s) => s.l
        case Right((w, a)) => Model.pure((w, Model.pure(a)))
      }
      Model.monadForModel.sequence(stochasticParticles)
    }

    def step(particles: List[Model[A]]): Model[Either[List[Model[A]], List[A]]] = {
      val advancedParticles: List[Either[Stochastic[(Double, Model[A])], (Double, A)]] = particles.map(_.step)
      if(advancedParticles.forall(_.isRight)) {
        val rightProjections = advancedParticles.map(_.right.get)
        resample(rightProjections).map(Right(_))
      } else {
        remodel(advancedParticles).flatMap(resample).map(Left(_))
      }
    }

    Model.monadForModel.tailRecM(List.fill(nParticles)(model))(step)
  }

}
