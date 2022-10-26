package lectures.part4PM

object L3_PatternsEverywhere extends App {
  // big idea 1 - catches are based on PATTERN MATCHING
  try {
    // code
  } catch {
    case e: RuntimeException       => "runtime"
    case npe: NullPointerException => "npe"
    case _                         => "something else"
  }

  // catches are actually pattern matches!
  /* It's like we wrote the code below:

  try {
    // code
  } catch(e) {
    e match {
      case e: RuntimeException       => "runtime"
      case npe: NullPointerException => "npe"
      case _                         => "something else"
    }
  }
   */

  // big idea #2 - generators are based on PATTERN MATCHING
  val list = List(1, 2, 3, 4)
  val evenOne = for {
    x <- list if x % 2 == 0 // ?!
  } yield 10 * x

  // I think it's really the expression the left of <- that is doing PM

  // Lets look at another example
  val tuples = List((1, 2), (3, 4))
  val filterTuples = for {
    (first, second) <- tuples
  } yield first * second
  // We can also decompose case classes, :: operators, etc.

  // big idea #3 - multiple value definitions are based on PATTERN MATCHING
  val tuple = (1, 2, 3)
  val (a, b, c) = tuple

  // we can define head and tail and bind them as values of list!
  val head :: tail = list
  println(head)
  println(tail)

  // big idea #4 - partial functions are based on PATTERN MATCHING
  val mappedList = list.map {
    case v if v % 2 == 0 => v + " is even"
    case 1               => "the one"
    case _               => "something else"
  }

  // equivalent to
  val mappedList2 = list.map(x =>
    x match {
      case v if v % 2 == 0 => v + " is even"
      case 1               => "the one"
      case _               => "something else"
    }
  )

  println(mappedList)
}
