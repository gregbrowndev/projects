package lectures.part3FP.exercises

object Exercise1 extends App {

  /**
   * 1. a function that takes 2 strings and concatenates them
   * 2. transform the MyPredicate and MyTransformer into function types
   * 3. define a function which takes an int and returns another function which
   *    takes an int and returns an int
   *      - what's the type of this function
   *      - how to do it
   */

  // 1
  val concat = new ((String, String) => String) {
    override def apply(v1: String, v2: String): String = v1 + v2
  }
  println(concat("Hello ", "Scala"))

  // 2
  trait MyPredicate[-T] {
    def apply(elem: T): Boolean
  }

  trait MyTransformer[-A, B] {
    def apply(elem: A): B
  }

  // Note: after going through the solutions, the main idea to take away here is
  // that these traits are not necessary! Instead, we can simply use
  // "A => Boolean" for the predicate type and "A => B" for the transformer type
  // inplace of these traits. We will rewrite the MyList in the Solution4 object

  // 3
  val transformer = new Function1[Int, Int => Int] {
    override def apply(elem: Int): Int => Int = { (elem2: Int) =>
      elem * elem2
    }
  }

  println(transformer(2)(6)) // curried function
}

object Solution4 {

//  trait MyPredicate[-T] {
//    def test(elem: T): Boolean
//  }
//
//  trait MyTransformer[-A, B] {
//    def transform(elem: A): B
//  }

  abstract class MyList[+A] {
    def head(): A

    def tail(): MyList[A]

    def isEmpty: Boolean

    def add[B >: A](item: B): MyList[B]

    override def toString: String = "[" + printElements + "]"

    def printElements: String

    // Note: These are higher-order functions - a concept critical to FP
    def map[B](transformer: A => B): MyList[B]
    def filter(predicate: A => Boolean): MyList[A]
    def flatMap[B](transformer: A => MyList[B]): MyList[B]

    // concatenation
    def ++[B >: A](other: MyList[B]): MyList[B]
  }

  case object Empty extends MyList[Nothing] {
    def head(): Nothing = throw new NoSuchElementException

    def tail(): MyList[Nothing] = throw new NoSuchElementException

    def isEmpty: Boolean = true

    def add[B >: Nothing](item: B): MyList[B] = Cons(item, Empty)

    def printElements: String = ""

    override def map[B](transformer: Nothing => B): MyList[B] =
      Empty

    override def filter(predicate: Nothing => Boolean): MyList[Nothing] =
      Empty

    override def flatMap[B](
        transformer: Nothing => MyList[B]
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

    override def map[B](transformer: A => B): MyList[B] = {
      // Note: now we just call transformer as a function
      Cons(transformer(h), t.map(transformer))
    }

    override def filter(predicate: A => Boolean): MyList[A] = {
      if (predicate(h)) Cons(h, t.filter(predicate))
      else t.filter(predicate)
    }

    override def flatMap[B](
        transformer: A => MyList[B]
    ): MyList[B] = {
      transformer(h) ++ t.flatMap(transformer)
    }

    override def ++[B >: A](other: MyList[B]): MyList[B] = {
      Cons(h, t ++ other)
    }
  }

  val listOfIntegers: MyList[Int] = Cons(1, Cons(2, Cons(3, Empty)))
  val anotherListOfIntegers: MyList[Int] = Cons(1, Cons(2, Cons(3, Empty)))

  // Now that MyList is a case class we gain all the built-in functions.
  // E.g. equals, hashCode and toString, auto-promoting params to fields, and cloning
  println(listOfIntegers == anotherListOfIntegers)
}
