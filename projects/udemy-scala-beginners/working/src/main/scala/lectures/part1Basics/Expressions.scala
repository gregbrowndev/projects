package lectures.part1Basics

object Expressions extends App {
  val x = 1 + 2
  println(x)

  // bit-wise operators
  // + - * / & | ^ << >> >>> (right shift with zero extension)

  // boolean operators
  // == != > >= < <=

  // logical operators
  // ! && ||

  var aVariable = 2
  aVariable += 3 // also works with -+ *= /_  (side effects)

  // Instructions vs Expressions
  // Instructions are imperative statements that are executed
  // Expressions are values with a type that are evaluated

  // Example: In Scala 'if' is an expression not an instruction!
  val aCondition = true
  val aConditionedValue = if (aCondition) 5 else 3
  println(aConditionedValue)

  // we can print the whole expression as a value!
  println(if (aCondition) 5 else 3)

  // There are loops in Scala, but they are discouraged!
  // NEVER WRITE THIS
  var i = 0
  while (i < 10) {
    println(i)
    i += 1
  }

  // EVERYTHING in Scala is an Expression!

  // even an assignment to a variable is an expression that returns something
  // it returns Unit which is equivalent to void in other languages
  val aWeirdValue = (aVariable = 3) // Unit === void
  print(aWeirdValue) // Unit

  // you can see there here too
  val aWhile = while (i < 10) {
    println(i)
    i += 1
  }
  // aWhile has type Unit

  // side effects: println(), whiles, reassigning

  // Code blocks
  val aCodeBlock = {
    val y = 2
    val z = y + 1

    if (z > 2) "hello" else "goodbye"
  }
  // code blocks are also expressions, they have a value and a type!
  // The value of acodeblock is the value of its last expression.
  // aCodeBlock has type String and will contain the value "hello"

  // values and variables defined in code blocks are locally scoped
  //   val anotherValue = z + 1  // error

  // Questions

  // 1. What is the difference between "hello world" vs println("hello world")?
  // Answer: they are both expressions but println is a side effect. The former
  //         has String value "hello world" and the latter has value Unit

  // 2. What is the value of:
  //       val someValue = { 2 > 3 }
  // Answer: the value is false

  // 3. What is the value of
  //        val someOtherValue = {
  //          if(someValue) 239 else 986
  //          42
  //        }
  // Answer: 42
}