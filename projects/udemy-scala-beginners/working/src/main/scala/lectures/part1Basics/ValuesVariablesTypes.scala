package lectures.part1Basics

object ValuesVariablesTypes extends App {
  val x: Int = 42
  println(x)

  //  cannot reassign val - they are immutable!
  //  x = 2

  // built-in types
  val aString: String = "hello"
  val aBoolean: Boolean = true
  val aChar: Char = 'a'
  val anInt = x
  val aShort: Short = 4612
  val aLong: Long = 54353453534543L
  val aFloat: Float = 2.0f
  val aDouble: Double = 3.14

  // variables
  var aVariable: Int = 4
  aVariable = 5  // side-effect
}

