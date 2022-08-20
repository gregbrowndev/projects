package exercises
package exercise5

/**
 * Exercise 5: Custom Exceptions
 *
 * 1. Crash your program with an OutOfMemoryError
 * 2. Crash with SOError
 * 3. PocketCalculator
 *      - add(x,y)
 *      - multiply(x,y)
 *      - divide(x,y)
 *
 * Throw
 *        - OverflowException if add(x,y) exceeds Int.MAX_VALUE
 *        - UnderflowException if subtract(x,y) exceeds INT.MIN_VALUE
 *        - MathCalculationException for division by 0
 */

class OverflowException extends Exception
class UnderflowException extends Exception
class MathCalculationException extends Exception

object PocketCalculator {
  def add(x: Int, y: Int): Int = {
    val result = x + y
    // To figure out if we overflowed the int, we can check if the result
    // has wrapped around becoming a large negative number
    if (x > 0 && y > 0 && result < 0)
      throw OverflowException()
    // likewise for underflow
    else if (x < 0 && y < 0 && result > 0)
      throw UnderflowException()
    else result
  }

  def subtract(x: Int, y: Int): Int = {
    val result = x - y
    if (x > 0 && y < 0 && result < 0)
      throw OverflowException()
    else if (x < 0 && y > 0 && result > 0)
      throw UnderflowException()
    else result
  }

  def multiply(x: Int, y: Int): Int = {
    val result = x * y
    if ((x > 0 && y > 0 && result < 0) || (x < 0 && y < 0 && result < 0))
      throw OverflowException()
    else if ((x < 0 && y > 0 && result > 0) || (x > 0 && y < 0 && result > 0))
      throw UnderflowException()
    else result
  }

  def divide(x: Int, y: Int): Int = {
    if (y == 0) throw MathCalculationException()
    else x / y
  }
}

object Exercise5 extends App {
  // 1. OOM
//  val array = Array.ofDim[Int](Int.MaxValue)

  // 2. SO Error
//  def infinite: Int = 1 + infinite
//  val noLimit = infinite

  // 3. Math problems
}
