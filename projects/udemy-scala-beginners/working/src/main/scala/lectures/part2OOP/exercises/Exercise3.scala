package lectures.part2OOP.exercises

import scala.annotation.tailrec

object Exercise3 extends App {

  /**
   * Exercise 3
   *
   *   1. Define generic trait MyPredicate[-T] with a method test(T) => Boolean
   *      2. Define generic trait MyTransformer[-A, B] with a method transform(A) => B
   *      3. Extend MyList with functions:
   *      - map(transformer) => MyList
   *      - filter(predicate) => MyList
   *      - flatMap(transformer from A to MyList[B]) => MyList[B]
   *
   * Hint: some of the type parameters must be contravariant to compile
   *
   * E.g.
   *
   * class EvenPredicate extends MyPredicate[Int]
   * class StringToIntTransformer extends MyTransformer[String, Int]
   *
   * [1, 2, 3].map(n * 2) == [2, 4, 6]
   * [1, 2, 3, 4].filter(n % 2) == [2, 4]
   * [1, 2, 3].flatMap(n => [n, n+1]) == [1, 2, 2, 3, 3, 4]
   *
   * Note: These functions needn't use tail recursion. In a real setting that would be
   * important. However, the implementations are more involved and would detract for this exercise.
   */

  trait MyPredicate[-T] {
    def test(elem: T): Boolean
  }

  trait MyTransformer[-A, B] {
    def transform(elem: A): B
  }

  class EvenPredicate extends MyPredicate[Int] {
    override def test(elem: Int): Boolean = elem % 2 == 0
  }

  class StringToIntTransformer extends MyTransformer[String, Int] {
    override def transform(elem: String): Int = elem.toInt
  }

  abstract class MyList[+A] {
    def head(): A

    def tail(): MyList[A]

    def isEmpty: Boolean

    def add[B >: A](item: B): MyList[B]

    override def toString: String = "[" + printElements + "]"

    def printElements: String

    def map[B](transformer: MyTransformer[A, B]): MyList[B]

    def filter(predicate: MyPredicate[A]): MyList[A]

    def flatMap[B](transformer: MyTransformer[A, MyList[B]]): MyList[B]

    def ++[B >: A](other: MyList[B]): MyList[B]
  }

  object Empty extends MyList[Nothing] {
    def head(): Nothing = throw new NoSuchElementException

    def tail(): MyList[Nothing] = throw new NoSuchElementException

    def isEmpty: Boolean = true

    def add[B >: Nothing](item: B): MyList[B] = new Cons(item, Empty)

    def printElements: String = ""

    override def map[B](transformer: MyTransformer[Nothing, B]): MyList[B] =
      Empty

    override def filter(predicate: MyPredicate[Nothing]): MyList[Nothing] =
      Empty

    override def flatMap[B](
        transformer: MyTransformer[Nothing, MyList[B]]
    ): MyList[B] = Empty

    override def ++[B >: Nothing](other: MyList[B]): MyList[B] = other
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

    /**
     * [1,2,3].map(n * 2)
     * = new Cons(2, [2,3].map(n * 2))
     * = new Cons(2, new Cons(4, [3].map(n * 2)))
     * = new Cons(2, new Cons(4, new Cons(6, Empty.map(n * 2))))
     * = new Cons(2, new Cons(4, new Cons(6, Empty)))
     */
    override def map[B](transformer: MyTransformer[A, B]): MyList[B] = {
      new Cons(transformer.transform(h), t.map(transformer))
    }

    /**
     * [1,2,3].filter(n % 2 == 0)
     * = [2,3].filter(n % 2 == 0)
     * = new Cons(2, [3].filter(n % 2 == 0))
     * = new Cons(2, Empty.filter(n % 2 == 0))
     * = new Cons(2, Empty)
     */
    override def filter(predicate: MyPredicate[A]): MyList[A] = {
      if (predicate.test(h)) new Cons(h, t.filter(predicate))
      else t.filter(predicate)
    }

    /**
     * [1,2].flatMap(n => [n, n+1])
     * = [1,2] ++ [2].flatMap(n => [n, n+1])
     * = [1,2] ++ [2,3] ++ Empty.flatMap(n => [n, n+1])
     * = [1,2] ++ [2,3] ++ Empty
     * = [1,2,2,3]
     */
    override def flatMap[B](
        transformer: MyTransformer[A, MyList[B]]
    ): MyList[B] = {
      transformer.transform(h) ++ t.flatMap(transformer)
    }

    /**
     * [1,2] ++ [3,4,5]
     * = new Cons(1, [2] ++ [3,4,5])
     * = new Cons(1, new Cons(2, Empty ++ [3,4,5]))
     * = new Cons(1, new Cons(2, [3,4,5])
     */
    override def ++[B >: A](other: MyList[B]): MyList[B] = {
      new Cons(h, t ++ other)
    }
  }

  val listOfIntegers: MyList[Int] = new Cons(1, new Cons(2, new Cons(3, Empty)))
  val listOfStrings: MyList[String] = new Cons("1", new Cons("2", Empty))

  // map
  val transformer = new StringToIntTransformer()
  println(listOfStrings.map(transformer).toString)
  println(
    listOfIntegers
      .map(
        new MyTransformer[Int, Int] {
          override def transform(elem: Int): Int = elem * 2
        }
      )
      .toString
  )

  // filter
  val predicate = new EvenPredicate()
  println(listOfIntegers.filter(predicate).toString)

  // flatMap
  println(
    listOfIntegers
      .flatMap(
        new MyTransformer[Int, MyList[Int]] {
          override def transform(elem: Int): MyList[Int] =
            new Cons(elem, new Cons(elem * 2, Empty))
        }
      )
      .toString
  )
}
