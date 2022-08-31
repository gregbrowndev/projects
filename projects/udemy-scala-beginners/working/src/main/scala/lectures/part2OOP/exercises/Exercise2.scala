package lectures.part2OOP.exercises

object Exercise2 extends App {

  /**
   * Exercise 2: Expand MyList in src/main/exercises/Exercise1 to be covariant in type A
   */

  abstract class MyList[+A] {
    def head(): A

    def tail(): MyList[A]

    def isEmpty: Boolean

    def add[B >: A](item: B): MyList[B]

    override def toString: String = "[" + printElements + "]"

    def printElements: String
  }

  object Empty extends MyList[Nothing] {
    def head(): Nothing = throw new NoSuchElementException

    def tail(): MyList[Nothing] = throw new NoSuchElementException

    def isEmpty: Boolean = true

    def add[B >: Nothing](item: B): MyList[B] = new Cons(item, Empty)

    def printElements: String = ""
  }

  class Cons[+A](h: A, t: MyList[A]) extends MyList[A] {
    def head(): A = h

    def tail(): MyList[A] = t

    def isEmpty: Boolean = false

    def add[B >: A](item: B): MyList[B] = new Cons(item, this)

    def printElements: String = {
      if (t.isEmpty) "" + h
      else s"${h} ${t.printElements}"
    }
  }

  val listOfIntegers: MyList[Int] = new Cons(1, new Cons(2, new Cons(3, Empty)))
  val listOfStrings: MyList[String] =
    new Cons("Hello", new Cons("Scala", Empty))

  println(listOfIntegers.toString)
  println(listOfStrings.toString)
}
