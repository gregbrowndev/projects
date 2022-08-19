package lectures.part1Basics

object StringOps extends App {

  val aString: String = "Hello, I am learning Scala"

  println(aString.charAt(2))
  println(aString.substring(7, 11))
  println(aString.split(" ").toList)
  println(aString.startsWith("Hello"))
  println(aString.replace(" ", "-"))
  println(aString.toLowerCase())
  println(aString.length())

  // All of these utilities are the same as they are in Java

  // There are some Scala specific utilities
  val aNumberString = "45"
  val aNumber = aNumberString.toInt // parsing an int
  println('a' +: aNumberString :+ 'z') // append and prepend operators
  println(aString.reverse) // reverse the string
  println(aString.take(2)) // take first n characters

  // String interpolators

  // S-interpolators
  val name = "David"
  val age = 12
  val greeting = s"Hello, my name is $name and I am $age years old"
  val anotherGreeting = s"Hello, my name is $name and I am ${age + 1} years old"
  println(anotherGreeting)

  // F-interpolators
  val speed = 1.2f
  val myth = f"$name%s can eat $speed%2.2f burgers per minute"
  println(myth)

  // f"" - interpolated formatted string
  // $name - will expand value of `name`
  // %2.2f - float number format: 2 chars total, minimum 2 dp

  // F-interpolated strings can all check for type correctness. E.g.
  val x = 1.1f
  //  val str = f"$x%3d"  // error because %3d expects integer value


  // Raw-interpolator
  println(raw"This is a \n newline")
  val escaped = "This is a \n newline"
  println(raw"$escaped")
}
