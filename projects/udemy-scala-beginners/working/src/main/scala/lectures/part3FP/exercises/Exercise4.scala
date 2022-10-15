package lectures.part3FP.exercises

object Exercise4 extends App {

  /*
   * 1. Check if MyList supports for-comprehensions
   * 2. A small collection of at most ONE element - Maybe[+T]
   *    - map, flatMap, filter
   */
  // 1 - check MyList works with for-comprehensions
  import MyListExercise3._
  /* In order for for-comprehensions to work on our data structure,
     we need ensure it has functions with signatures:
       - map(f: A => B): MyList[B]
       - filter(p: A => Boolean): MyList[A]
       - flatMap(f: A => MyList[B]): MyList[B]
   */

  val listOfIntegers: MyList[Int] = Cons(1, Cons(2, Cons(3, Empty)))
  val listOfStrings: MyList[String] = Cons("Hello", Cons("Scala", Empty))
  val combinations = for {
    x <- listOfIntegers // if x % 2 == 0 // doesn't like filtering though!
    y <- listOfStrings
  } yield x + "-" + y
  println(combinations)

  // 2 - lets implement a collection

}

object Exercise4Collection extends App {

  abstract class Maybe[+A] {
    def map[B](f: A => B): Maybe[B]
    def filter(p: A => Boolean): Maybe[A]
    def flatMap[B](f: A => Maybe[B]): Maybe[B]
  }

  case object Empty extends Maybe[Nothing] {
    override def map[B](f: Nothing => B): Maybe[B] = Empty
    override def filter(p: Nothing => Boolean): Maybe[Nothing] = Empty
    override def flatMap[B](f: Nothing => Maybe[B]): Maybe[B] = Empty
  }

  case class Just[+A](value: A) extends Maybe[A] {
    override def map[B](f: A => B): Maybe[B] = Just(f(value))

    override def filter(p: A => Boolean): Maybe[A] =
      if (p(value)) this
      else Empty

    override def flatMap[B](f: A => Maybe[B]): Maybe[B] = f(value)
  }

  val maybeInt = Just(2)
  println(maybeInt)
  println(maybeInt.map(_ * 2))
  println(maybeInt.map(x => s"string-${x}"))
//  println(Empty.map(x => x * 2))

  println(maybeInt.filter(_ % 2 == 0))
  println(maybeInt.filter(_ % 3 == 0))

  println(maybeInt.flatMap(x => Just(x % 2 == 0)))
  println(maybeInt.flatMap(_ => Empty))
}
