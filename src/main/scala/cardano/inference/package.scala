package cardano

package object inference {

  object all extends All                             // scalastyle:ignore

  object base extends Base                           // scalastyle:ignore
  object mh extends Metropolis               // scalastyle:ignore
  object smc extends SequentialMonteCarlo            // scalastyle:ignore
  object pmcmc extends ParticleMarkovChainMonteCarlo // scalastyle:ignore

}
