package lectures.part1Basics

import scala.annotation.tailrec

object Recursion extends App {
  // Let's take a look at that factorial function again
  // but this time with some extra print statements
  def factorial(n: Int): Int = {
    if (n <= 0) 1
    else {
      println("Computing factorial of " + n + " - I first need factorial of " + (n - 1))
      val result = n * factorial(n - 1)
      println("Computed factorial of " + n)
      result
    }
  }

  println("Factorial")
  println(factorial(10))

  // As you can see in the output, the function has to recurse 10 times
  // before it finally reaches the base case and can start to step out
  // Each of these function calls creates a stack frame.

  // The problem with recursive functions is they can easily cause a
  // StackOverFlow error!
  //  println(factorial(5000))
  // Exception in thread "main" java.lang.StackOverflowError

  // To solve this, we need to write the code in a smarter way
  def anotherFactorial(n: BigInt): BigInt = {
    @tailrec
    def factHelper(x: BigInt, accumulator: BigInt): BigInt = {
      if (x <= 1) accumulator
      else factHelper(x - 1, x * accumulator)
    }

    factHelper(n, 1)
  }

  // lets break this code down, as it isn't so obvious!
  // anotherFactorial(10) = factHelper(10, 1)
  //    = factHelper(9, 10 * 1)
  //    = factHelper(8, 9 * 10 * 1)
  //    = factHelper(7, 8 * 9 * 10 * 1)
  //    = factHelper(6, 7 * 8 * 9 * 10 * 1)
  //    ...
  //    = factHelper(1, 2 * 3 * 4 * 5 * 6 * 7 * 8 * 9 * 10 * 1)
  //    = 2 * 3 * 4 * 5 * 6 * 7 * 8 * 9 * 10 * 1
  println(anotherFactorial(5000))
  // returns a massive number (we had to replace Int with BigInt)

  // If you look closely, you may wonder why this function doesn't
  // also throw a StackOverFlow error. The inner factHelper is called
  // N times! It nothing to do with the fact it is nested inside the
  // other function, this would still be the case if we called it directly.

  // The reason is because of the last expression of the function:
  // New: `else factHelper(x - 1, x * accumulator)`
  // Old: `else  n * factorial(n - 1)`
  // The previous implementation requires a new recursive stack frame in order
  // to evaluate `n` against whatever value of `factorial(n - 1)` and then
  // pass it back from the stack frame. However, the new impl does not require
  // this. Scala doesn't need to save any intermediate result to be used for later
  // so it can use the existing stack frame!

  // This is called TAIL RECURSION = use recursive call as the LAST expression of
  // each path through the function

  // Scala provides an annotation `@tailrec` that can be added to the function to
  // do a compile-time check that the function is tail recursive

  // WHEN YOU NEED LOOPS, USE TAIL RECURSION!

  /**
   * Lets solve some problems using tail recursion
   *
   * 1. Concatentate a string n times
   * 2. isPrime
   * 3. fibonacci
   *
   * Hint: any function can be made tail recursive. The trick is to use
   * accumulators to store intermediate results rather than call the
   * function recursively. You need as many accumulators as there are
   * recursive calls within the code path.
   */

  // 1. Concatenate
  @tailrec
  def concatenate(aString: String, n: Int,  accumulator: String): String = {
    if(n <= 0) accumulator
    else concatenate(aString, n - 1, accumulator + aString)
  }
  println("Concatenate")
  println(concatenate("Bob", 4, ""))

  // 2. isPrime
  def isPrime(num: Int): Boolean = {
    @tailrec
    def inner(t: Int): Boolean = {
      if (t <= 1) true
      else if (num % t == 0) false
      else inner(t - 1)
    }

    inner(num / 2)
  }
  println("Is Prime")
  println("0 is prime: " + isPrime(0))
  println("1 is prime: " + isPrime(1))
  println("2 is prime: " + isPrime(2))
  println("3 is prime: " + isPrime(3))
  println("4 is prime: " + isPrime(4))
  println("12333943 is prime: " + isPrime(12333943))

  // 3. Fibonacci
  def fibonacci(n: Int): Int = {
    @tailrec
    def inner(i: Int, last: Int, nextToLast: Int): Int = {
      if (i >= n) last
      else inner(i + 1, last + nextToLast, last)
    }

    inner(0, 1, 0)
  }
  println("Fibonacci")
  println(fibonacci(0))
  println(fibonacci(1))
  println(fibonacci(2))
  println(fibonacci(3))
  println(fibonacci(4))
  println(fibonacci(5))
  println(fibonacci(6))
  println(fibonacci(7))
  println(fibonacci(8))
}
