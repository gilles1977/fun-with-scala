package book

class Rational(n: Int, d: Int) { //primary constructor
  require(d != 0)

  private val g = gcd(n, d)
  val num: Int = n / g
  val den: Int = d / g

  def this(n: Int) = this(n, 1) //auxiliary constructor

  override def toString =
    num + "/" + den

  def +(r: Rational): Rational =
    new Rational(num * r.den + den * r.num, den * r.den)

  def *(r: Rational): Rational =
    new Rational(num * r.num, den * r.den)

  def lessThan(that: Rational) =
    this.num * that.den < that.num * this.den

  def max(that: Rational) =
    if (this.lessThan(that)) that else this

  private def gcd(a: Int, b: Int): Int = {
    if (b == 0) a else gcd(b, a % b)
  }
}