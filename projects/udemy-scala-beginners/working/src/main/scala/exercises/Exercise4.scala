package exercises
package exercise4

import exercises.exercise3.MyPredicate

/**
 * Exercise 4: Expand MyList to use case classes and case objects
 */

trait MyPredicate[-T] {
  def test(elem: T): Boolean
}

trait MyTransformer[-A, B] {
  def transform(elem: A): B
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

case object Empty extends MyList[Nothing] {
  def head(): Nothing = throw new NoSuchElementException

  def tail(): MyList[Nothing] = throw new NoSuchElementException

  def isEmpty: Boolean = true

  def add[B >: Nothing](item: B): MyList[B] = Cons(item, Empty)

  def printElements: String = ""

  override def map[B](transformer: MyTransformer[Nothing, B]): MyList[B] = Empty

  override def filter(predicate: MyPredicate[Nothing]): MyList[Nothing] = Empty

  override def flatMap[B](
      transformer: MyTransformer[Nothing, MyList[B]]
  ): MyList[B] = Empty

  override def ++[B >: Nothing](other: MyList[B]): MyList[B] = other
}

case class Cons[+A](h: A, t: MyList[A]) extends MyList[A] {
  def head(): A = h

  def tail(): MyList[A] = t

  def isEmpty: Boolean = false

  def add[B >: A](item: B): MyList[B] = Cons(item, this)

  def printElements: String = {
    if (t.isEmpty) "" + h
    else s"${h} ${t.printElements}"
  }

  override def map[B](transformer: MyTransformer[A, B]): MyList[B] = {
    Cons(transformer.transform(h), t.map(transformer))
  }

  override def filter(predicate: MyPredicate[A]): MyList[A] = {
    if (predicate.test(h)) Cons(h, t.filter(predicate))
    else t.filter(predicate)
  }

  override def flatMap[B](
      transformer: MyTransformer[A, MyList[B]]
  ): MyList[B] = {
    transformer.transform(h) ++ t.flatMap(transformer)
  }

  override def ++[B >: A](other: MyList[B]): MyList[B] = {
    Cons(h, t ++ other)
  }
}

object Exercise4 extends App {
  val listOfIntegers: MyList[Int] = Cons(1, Cons(2, Cons(3, Empty)))
  val anotherListOfIntegers: MyList[Int] = Cons(1, Cons(2, Cons(3, Empty)))

  // Now that MyList is a case class we gain all the built-in functions.
  // E.g. equals, hashCode and toString, auto-promoting params to fields, and cloning
  println(listOfIntegers == anotherListOfIntegers)
}
