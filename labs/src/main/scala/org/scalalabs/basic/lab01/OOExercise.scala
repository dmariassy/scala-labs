package org.scalalabs.basic.lab01
import scala.language.implicitConversions
/**
 * The goal of this exercise is to get familiar basic OO constructs in scala
 *
 * Fix the code so that the unit test 'CurrencyExerciseTest' passes.
 *
 * In order for the tests to pass you need to do the following:
 *
 * Exercise 1:
 * - Create a class Euro
 * - Provide it with two constructor parameters: euro:Int, cents:Int
 * - Provide the cents field with default value: 0
 * - Provide an immutable field named: inCents that converts euro + cents into cents.
 * - Create an object Euro with a factory method named: fromCents that creates an Euro based on cents.
 * - Create a method named: + to the Euro class that adds another Euro
 * - Create a method named: * to the Euro class that multiplies an Euro
 *
 * Exercise 2:
 * - Create an abstract class Currency
 * - Provide it with one constructor parameter: symbol:String
 * - Extend the previously created Euro class from Currency
 * - Override the toString method of Euro to represent the following String:
 *   -> symbol + ': ' + euro + ',' + cents.  E.g: EUR 200,05
 * - In case the cents are 0 use this representation:
 *   -> symbol + ': ' + euro + ',--. E.g.: EUR 200.--
 *
 * Exercise 3:
 * - Mix the Ordered trait in Euro
 * - Implement the compare method
 *
 * Exercise 4:
 * - Provide an implicit class that adds a *(euro:Euro) method to Int
 * - Create a new currency Dollar
 * - Provide a implicit conversion method that converts from Euro to Dollar using the
 *   [[org.scalalabs.basic.lab01.DefaultCurrencyConverter]]
 *
 * Exercise 5:
 * - Extend the conversion method from Euro to Dollar with an implicit parameter
 *   of type [[org.scalalabs.basic.lab01.CurrencyConverter]]
 * - Use the implicit CurrencyConverter to do the conversion.
 */

abstract class Currency(intPart: Int, decimalPart: Int = 0, val symbol: String) {

  def inCents: Int = intPart * 100 + decimalPart

}

object Euro {

  def fromCents(cents: Int): Euro = {

    val euro = cents / 100
    val remainderCents = cents - euro * 100

    new Euro(euro, remainderCents)

  }

  implicit class IntTimesEuro(x: Int) {
    def *(y: Euro): Euro = {
      y * x
    }
  }

}

case class Euro(euro: Int, cents: Int = 0, override val symbol: String = "EUR") extends Currency(euro, cents, symbol) with Ordered[Euro] {

  def +(other: Euro): Euro = {
    val centVal = other.inCents + this.inCents
    Euro.fromCents(centVal)
  }

  def *(multiplier: Int): Euro = {
    val centVal = multiplier * this.inCents
    Euro.fromCents(centVal)
  }

  def compare(that: Euro): Int = {
    this.inCents - that.inCents
  }

  override lazy val toString = s"$symbol: $euro,${if (cents > 0) f"$cents%02d" else "--"}"

}

object Dollar extends DefaultCurrencyConverter {
  implicit def DollartoEuro(x: Dollar): Euro = {
    Euro.fromCents(toEuroCents(x.inCents))
  }
}

case class Dollar(dollar: Int, cents: Int = 0, override val symbol: String = "USD") extends Currency(dollar, cents, symbol) {

}