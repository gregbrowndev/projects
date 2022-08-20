package lectures.part1Basics

object CallByNameOrValue extends App {

  // In this lesson, we'll look at two different semantics for calling
  // functions: call by name and call by value

  def calledByValue(x: Long): Unit = {
    println("by value: " + x)
    println("by value: " + x)
  }

  def calledByName(x: => Long): Unit = {
    // Note the => syntax
    println("by name: " + x)
    println("by name: " + x)
  }

  calledByValue(System.nanoTime())
  calledByName(System.nanoTime())
  // Output:
  //  by value: 204716098475087
  //  by value: 204716098475087
  //  by name: 204716176322071
  //  by name: 204716177541439

  // We can observe that the by value call prints out the exact
  // same timestamp. However, the two printed values in the
  // calledByName function are different!

  // In call by value, the expression is evaluated before the
  // function call and the value is passed in (by value).
  // In call by name, the literal expression is passed by name
  // and only evaluated within the function body.

  // Let's look at another example.
  def infinite(): Int = 1 + infinite()

  def printFirst(x: Int, y: => Int): Unit = println(x)

  // Here we have two not very useful functions but let us
  // look at what happens when we call:
  // printFirst(infinite(), 42)   // of course, this causes a StackOverflowError

  // however, if we call it like:
  printFirst(42, infinite()) // no error!

  // This is because the call by name argument has lazy semantics
}
