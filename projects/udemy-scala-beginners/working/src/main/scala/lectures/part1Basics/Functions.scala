package lectures.part1Basics

object Functions extends App {

  // lets look at the syntax for defining a function
  def aFunction(a: String, b: Int): String =
    a + " " + b

  // note we will usually define the body as a code block:
  def anotherFunction(a: String, b: Int): String = {
    a + " " + b
  }

  println(anotherFunction("hello", 3))

  def aParameterlessFunction(): Int = 42

  println(aParameterlessFunction())

  // Note: In Scala 2, you can evaluate a parameterless function
  // without having to call it with parentheses
  //  println(aParameterlessFunction)  // 42

  // In FP, we use functions instead of loops
  def aRepeatedFunction(aString: String, n: Int): String = {
    if (n == 1) aString
    else aString + aRepeatedFunction(aString, n - 1)
  }

  println(aRepeatedFunction("hello", 3))

  // WHEN YOU NEED LOOPS, USE RECURSION!

  // Note: specifying a function's return type is optional as the
  // compiler can infer this from the implementation. However, for
  // recursive functions it is required.

  // we can use Unit as a return type for functions that just do side-effects
  def aFunctionWithSideEffects(aString: String): Unit = println(aString)

  // we can nest functions
  def aBigFunction(n: Int): Int = {
    def aSmallFunction(a: Int, b: Int): Int = a + b

    aSmallFunction(n, n - 1)
  }

  /* Tasks - write a function to:
  1. print a greeting (name, age) => "Hi, my name is $name and I am $age years old."
  2. compute nth Factorial 1 * 2 * 3 * ... * n
  3. compute nth Fibonacci number 1 + 1 + 2 + 3 + 5 + 8 + ...
  4. test is a number of prime
   */

  // 1
  def greet(name: String, age: Int) = {
    "Hi, my name is " + name + " and I am " + age + " years old."
  }

  println("Greeting")
  println(greet("Bob", 2))

  // 2
  def factorial(n: Int): Int = {
    if (n <= 0) 1
    else n * factorial(n - 1)
  }

  println("Factorial")
  println(factorial(0))
  println(factorial(1))
  println(factorial(2))
  println(factorial(3))

  // 3
  def fibonacci(n: Int): Int = {
    if (n <= 1) 1
    else fibonacci(n - 1) + fibonacci(n - 2)
  }

  println("Fibonacci")
  println(fibonacci(0))
  println(fibonacci(1))
  println(fibonacci(2))
  println(fibonacci(3))
  println(fibonacci(4))

  // 4
  def isPrime(n: Int): Boolean = {
    def isPrimeUntil(t: Int): Boolean = {
      //      println("n=" + n + ", t=" + t)
      if (t <= 1) true
      else n % t != 0 && isPrimeUntil(t - 1)
    }

    isPrimeUntil(n / 2)
  }

  println("Is Prime")
  println("0 is prime: " + isPrime(0))
  println("1 is prime: " + isPrime(1))
  println("2 is prime: " + isPrime(2))
  println("3 is prime: " + isPrime(3))
  println("4 is prime: " + isPrime(4))
  println("5 is prime: " + isPrime(5))
}
