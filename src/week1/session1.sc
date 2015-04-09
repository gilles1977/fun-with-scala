package week1
import book._

object session1 {

  def abs(x: Double) = if (x < 0) -x else x       //> abs: (x: Double)Double

  def sqrt(x: Double) = {

    def sqrtIter(guess: Double): Double = {
      if (isGoodEnough(guess)) guess
      else sqrtIter(improve(guess))
    }

    def isGoodEnough(guess: Double) = abs(guess * guess - x) / x < 0.001

    def improve(guess: Double) = {
      (guess + x / guess) / 2
    }

    sqrtIter(1.0)
  }                                               //> sqrt: (x: Double)Double
  
  sqrt(2)                                         //> res0: Double = 1.4142156862745097
  sqrt(4)                                         //> res1: Double = 2.000609756097561
  
  val oneHalf = new Rational(1,2)                 //> oneHalf  : book.Rational = 1/2
  val twoThird = new Rational(2, 3)               //> twoThird  : book.Rational = 2/3
  oneHalf + twoThird                              //> res2: book.Rational = 7/6
  oneHalf * twoThird                              //> res3: book.Rational = 1/3
  oneHalf.max(twoThird)                           //> res4: book.Rational = 2/3
  val five = new Rational(5)                      //> five  : book.Rational = 5/1
  val r = new Rational(66, 42)                    //> r  : book.Rational = 11/7
  
  implicit def intToRational(x: Int) =
    new Rational(x)                               //> intToRational: (x: Int)book.Rational
  2 * r                                           //> res5: book.Rational = 22/7
  
}