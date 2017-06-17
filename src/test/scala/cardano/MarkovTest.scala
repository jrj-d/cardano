package cardano

import org.apache.commons.math3.random.MersenneTwister
import org.scalatest._
import cardano.semifield.prob._

class MarkovTest extends FlatSpec with Matchers {

  implicit val random = new MersenneTwister(0)

  sealed trait Weather
  case object Sunny extends Weather
  case object Rainy extends Weather

  val weather: Stochastic[Stream[Weather]] = Stochastic.pure[Weather](Sunny).markov[Weather] {
    case Sunny => Stochastic.coin(0.9).map {
      case true => Sunny
      case false => Rainy
    }
    case Rainy => Stochastic.coin(0.5).map {
      case true => Sunny
      case false => Rainy
    }
  }

  val nbSunnyDays: Stochastic[Int] = weather.map(_.drop(1).take(1000).count(_ == Sunny))

  "A Californian weather model" should "have around 833 sunny days out of 1000" in {
    for(i <- 0 to 5) {
      nbSunnyDays.mean(100).sample should be (833.0 +- 10)
    }
  }

}
