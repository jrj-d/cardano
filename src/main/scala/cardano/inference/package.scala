package cardano

package object inference {

  object all extends All                  // scalastyle:ignore

  object base extends Base                // scalastyle:ignore
  object metropolis extends Metropolis    // scalastyle:ignore
  object smc extends SequentialMonteCarlo // scalastyle:ignore

}
