package lectures.part2OOP

object Exceptions extends App {
  val x: String = null
  //  println(x.length)  // throws NullPointerException

  // 1. Throwing Exceptions
  // we can throw exceptions and like everything in Scala,
  // this is an expression that can be assigned to a value
  //  val aWeirdValue: String = throw new NullPointerException()

  // Note: it is kind of wierd that it causes the program to crash given that we've
  // assigned it as a value. I thought this would change the behaviour to be more of
  // an error as a value.

  // Note: the type of the throw expression is Nothing. We can assign Nothing to
  // any other type, e.g. a String like above.

  // throwable classes extend the Throwable class
  // Exception and Error are the major Throwable subtypes
  // Note: they have different intended semantics:
  //  Exception = something went wrong in the program, e.g. NullPointerException
  //  Error = something went with the system/JVM, e.g. StackOverflowError

  // 2. Catching Exceptions
  def getInt(withExceptions: Boolean): Int =
    if (withExceptions) throw new RuntimeException("No int for you")
    else 42

  try {
    getInt(true)
  } catch {
    case e: RuntimeException => println("caught a Runtime exception")
    // Note: if we don't match on the RuntimeException type then it is not caught
    // and will crash the program in this case
  } finally {
    // the finally clause is always executed regardless of an error being thrown
    // this block is optional
    println("finally")
  }

  // the last thing to know is that we can assign the whole `try { } catch { }` construct to
  // a value. The type of this value is inferred from the return types of the try and the
  // catch blocks but NOT the finally block.

  // 3. Custom Exceptions
  class MyException extends Exception
  val exception = new MyException
  throw exception
}
