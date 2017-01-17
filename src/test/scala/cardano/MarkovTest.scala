package cardano

import org.apache.commons.math3.random.MersenneTwister
import org.scalatest._

class MarkovTest extends FlatSpec with Matchers {

  val generator = new AllDistributions(new MersenneTwister(0))

  sealed trait Weather
  case object Sunny extends Weather
  case object Rainy extends Weather

  val weather: Stochastic[Stream[Weather]] = StochasticConstant[Weather](Sunny).markov[Weather] {
    case Sunny => generator.coin(0.9).map {
      case true => Sunny
      case false => Rainy
    }
    case Rainy => generator.coin(0.5).map {
      case true => Sunny
      case false => Rainy
    }
  }

  val nbSunnyDays: Stochastic[Int] = weather.map(_.drop(1).take(1000).count(_ == Sunny))

  "A Californian weather model" should "have around 833 sunny days out of 1000" in {
    for(i <- 0 to 5) {
      nbSunnyDays.expectation(100) should be (833.0 +- 10)
    }
  }

}
