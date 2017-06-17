package cardano

import cardano.continuous.ContinuousDistributions
import cardano.discrete.DiscreteDistributions

/**
  * A class containing all the standard distributions implemented in [[cardano]].
  */
trait Distributions extends DiscreteDistributions with ContinuousDistributions
