package cardano.semifield


class LogProbSemifield extends Semifield[Double] {

  def unit: Double = 0.0

  def multiply(x: Double, y: Double): Double = x + y

  def combine(x: Double, y: Double): Double = {
    val max = math.max(x, y)
    val min = math.min(x, y)
    max + math.log(1 + math.exp(min - max))
  }

  def empty: Double = Double.NegativeInfinity

  def inject(x: Double): Double = math.log(x)

  def extract(x: Double): Double = math.exp(x)

  def inverse(x: Double): Double = -x

  override def divide(x: Double, y: Double): Double = x - y

}

trait LogProbInstance {
  implicit val semifieldForLogProb: LogProbSemifield = new LogProbSemifield
}
