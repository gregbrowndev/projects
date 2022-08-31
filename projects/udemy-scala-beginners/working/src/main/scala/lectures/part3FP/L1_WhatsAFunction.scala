package lectures.part3FP

object L1_WhatsAFunction extends App {

  // Our goal is to use functions as first class elements
  // problem: coming from a OOP background, we are used to working with
  // classes and methods.

  // In OOP, we might write a class like "Action" below that really just
  // represents a function.
  trait Action[A, B] {
    def execute(element: A): B
  }

  // with this we can define a anonymous classes / lambdas within the code so
  // we don't have to implement a concrete subclass
  val action = new Action[Int, Int] {
    override def execute(element: Int): Int = element * 2
  }
  println(action.execute(2))

  // This is the best an OOP language could do.

  // However, Scala has apply semantics

  trait MyFunction[A, B] {
    def apply(element: A): B
  }

  val doubler = new MyFunction[Int, Int] {
    override def apply(element: Int): Int = element * 2
  }

  println(doubler(2))
  // now we can call this object like an ordinary function!

  // function types = Function1[A, B]

  val stringToIntConverter = new Function1[String, Int] {
    override def apply(string: String): Int = string.toInt
  }
  println(stringToIntConverter("34"))

  // By default, Scala supports up to Function22, i.e. a function that takes
  // up to 22 parameters (plus 1 return type), e.g. a Function2 would look like
  val adder: Function2[Int, Int, Int] = new Function2[Int, Int, Int] {
    override def apply(v1: Int, v2: Int): Int = v1 + v2
  }

  // there is syntactic sugar for these types:
  val adder2: ((Int, Int) => Int) = new Function2[Int, Int, Int] {
    override def apply(v1: Int, v2: Int): Int = v1 + v2
  }

  // ALL SCALA FUNCTIONS ARE OBJECTS
  // that implement an interface Function1, Function2, Function3, etc.
}
