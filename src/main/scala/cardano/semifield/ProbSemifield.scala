package cardano.semifield


class ProbSemifield extends Semifield[Double] {

  def unit: Double = 1.0

  def multiply(x: Double, y: Double): Double = x * y

  def combine(x: Double, y: Double): Double = x + y

  def empty: Double = 0.0

  def inject(x: Double): Double = x

  def extract(x: Double): Double = x

  def inverse(x: Double): Double = 1.0 / x

  override def divide(x: Double, y: Double): Double = x / y

}

trait ProbInstance {
  implicit val semifieldForProb: ProbSemifield = new ProbSemifield
}
