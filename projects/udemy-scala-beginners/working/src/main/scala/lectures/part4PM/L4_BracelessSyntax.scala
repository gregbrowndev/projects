package lectures.part4PM

object L4_BracelessSyntax {
  // if-expression
  val anIfExpression = if (2 > 3) "bigger" else "smaller"

  // there is also the Java style
  val anIfExpressionV2 =
    if (2 > 3) {
      "bigger"
    } else {
      "smaller"
    }

  // and a compact-style
  val anIfExpressionV3 =
    if (2 > 3) "bigger"
    else "smaller"

  // In Scala 3, there is a new syntax
  /* Note: the formatter doesn't like the code below

  val anIfExpressionV4 =
    if 2 > 3 then
      "bigger"  // higher indentation than the if part
    else
      "smaller"
   */
  // The main idea is that Scala 3 introduces a 'then' keyword which allows
  // the braces to be dropped
  val anIfExpressionV4 =
    if 2 > 3 then "bigger"
    else "smaller"

  // the syntax works just like a code block where the last expression is returned
  val anIfExpressionV5 =
    if 2 > 3 then
      val result = "bigger"
      result
    else
      val result = "smaller"
      result

  // Scala 3 one-liner
  val anIfExpressionV6 = if 2 > 3 then "bigger" else "smaller"

  // for-comprehensions
  val aForComprehension = for {
    n <- List(1, 2, 3)
    s <- List("black", "white")
  } yield s"$n$s"

  val aForComprehensionV2 =
    for
      n <- List(1, 2, 3)
      s <- List("black", "white")
    yield s"$n$s"

  // pattern matching
  val meaningOfLife = 42
  val aPatternMatch = meaningOfLife match {
    case 1 => "the one"
    case 2 => "double or nothing"
    case _ => "something"
  }

  // scala 3
  val aPatternMatchV2 =
    meaningOfLife match
      case 1 => "the one"
      case 2 => "double or nothing"
      case _ => "something"

  // methods without braces
  def computeMeaningOfLife(arg: Int): Int = {
    val partialResult = 40
    // any number of newlines

    partialResult + 2
  }

  def computeMeaningOfLifeV2(arg: Int): Int =
    val partialResult = 40
    // any number of newlines

    partialResult + 2

  // The code above isn't too bad since the formatter removes redundant newlines
  // If this wasn't the case, it would be very easy to make mistakes with the
  // indentation. The "end token" helps with this

  // Significant indentation also applies to classes, traits, objects, enums,
  // and data types
  class Animal {
    def eat(): Unit =
      println("I'm eating")
  }

  class AnimalV2: // compiler expects the body of Animal
    def eat(): Unit =
      println("I'm eating")

    def grow(): Unit =
      println("I'm growing")

    // Many, many lines of code
  end AnimalV2

  // The end token is optional, but useful when using the new Scala 3 syntax
  // in very large blocks of code. The end token can be used in if, match, for,
  // methds, classes, traits, enums, objects

  // As a general rule of thumb, use an end token if the code block has 4
  // or more lines of code

  // anonymous classes
  val aSpecialAnimal = new Animal:
    override def eat(): Unit = println("I'm special")

  // indentation = strictly larger indentation
  // 3 spaces + 2 tabs > 2 spaces + 2 tabs
  // 3 spaces + 2 tabs > 3 spaces + 1 tab
  // 3 tabs + 2 spaces ??? 2 tabs + 3 spaces

  // Rule of thumb: use spaces or tabs for indentation, don't mix them!

  def main(args: Array[String]): Unit = {}
}
