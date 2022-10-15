package lectures.part3FP

object L4_MapFlatmapFilterFor extends App {
  // Let's introduce the official List data struct
  val list = List(1, 2, 3)
  println(list)
  println(list.head)
  println(list.tail)

  // map
  println(list.map(_ + 1))
  println(list.map(_ + " is a number"))

  // filter
  println(list.map(_ % 2 == 0))

  // flatmap
  val toPair = (x: Int) => List(x, x + 1)
  println(list.flatMap(toPair))

  // print all combinations between two lists
  val numbers = List(1, 2, 3, 4)
  val chars = List("a", "b", "c", "d")
  // List("a1", "a2", "a3", ..., "d4")
  val combinations = chars.flatMap(c => numbers.map(n => s"${c}${n}"))
  println(combinations)

  // foreach
  list.foreach(println)

  // The combinations example shows that chaining together mutliple
  // flatMaps can become difficult to read. There is a nicer syntax
  // to deal with this: for-comprehensions
  val forCombinations = for {
    n <- numbers
    c <- chars
  } yield s"${c}${n}"
  println(forCombinations)

  // for-comprehension guards (filtering)
  val forGuard = for {
    n <- numbers if n % 2 == 0
    c <- chars
  } yield s"${c}${n}"
  println(forGuard)

  // using foreach with side effects with for-comprehensions
  // this is equivalent to numbers.foreach(println)
  for {
    n <- numbers
  } println(n)
}
