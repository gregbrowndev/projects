package lectures.part3FP.exercises

import lectures.part2OOP.Generics.Mylist

object MyListExercise3 {
  abstract class MyList[+A] {
    def head(): A

    def tail(): MyList[A]

    def isEmpty: Boolean

    def add[B >: A](item: B): MyList[B]

    override def toString: String = "[" + printElements + "]"

    def printElements: String

    // HOFs
    def map[B](transformer: A => B): MyList[B]
    def filter(predicate: A => Boolean): MyList[A]
    def flatMap[B](transformer: A => MyList[B]): MyList[B]
    def ++[B >: A](other: MyList[B]): MyList[B]
    def forEach[B](func: A => Unit): Unit
    def sort(compare: (A, A) => Int): MyList[A]
    def zipWith[B, C](other: MyList[B], func: (A, B) => C): MyList[C]
    def fold[B](start: B)(func: (B, A) => B): B
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

    override def forEach[B](func: Nothing => Unit): Unit = ()

    override def sort(compare: (Nothing, Nothing) => Int): MyList[Nothing] = Empty

    override def zipWith[B, C](other: MyList[B], func: (Nothing, B) => C): MyList[C] = 
      if (!other.isEmpty) throw RuntimeException("Lists do not have the same length")
      else Empty

    override def fold[B](start: B)(func: (B, Nothing) => B): B = start
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

    override def forEach[B](func: A => Unit): Unit = {
      func(h)
      t.forEach(func)
    }

    override def sort(compare: (A, A) => Int): MyList[A] = {
        def insert(x: A, sortedList: MyList[A]): MyList[A] = 
            if (sortedList.isEmpty) new Cons(x, Empty)
            else if (compare(x, sortedList.head()) <= 0) new Cons(x, sortedList)
            else new Cons(sortedList.head(), insert(x, sortedList.tail()))

        val sortedTail = t.sort(compare)
        insert(h, sortedTail)
    }

    override def zipWith[B, C](other: MyList[B], func: (A, B) => C): MyList[C] = {
        if (other.isEmpty) throw RuntimeException("Lists do not have the same length")
        else new Cons(func(h, other.head()), t.zipWith(other.tail(), func)) 
    }

    override def fold[B](start: B)(func: (B, A) => B): B = 
      t.fold(func(start, h))(func)
  }
}

object Exercise3 extends App {

    /**
     *   1. Expand MyList
     *      - foreach A => Unit [1,2,3].foreach(x => println(x))
     *
     *   - sort ((A, A) => Int) => MyList [1,2,3].sort((x, y) => y - x) == [3,2,1]
     *
     *   - zipWith (list, (A, A) => B) => MyList[B]
     *        [1,2,3].zipWith([4,5,6], (x, y) => x * y) == [4,10,18]
     *
     *   - fold (start)(function) => a value ^ curried [1,2,3].fold(0)(x + y) == 6
     *
     * 2. Write two functions to transform a function to/from curried version
     *   - toCurry(f: (Int, Int) => Int) => (Int => Int => Int)
     *   - fromCurry(f: (Int => Int => Int)) => (Int, Int) => Int
     *
     * 3. Write two functions that can compose two functions and apply another
     *   - compose(f, g) => f(g(x))
     *   - andThen(f, g) => g(f(x))
     */

    // 1 - MyList
    import MyListExercise3._

    val listOfIntegers: MyList[Int] = Cons(1, Cons(2, Cons(3, Empty)))
    listOfIntegers.forEach(println)

    println(listOfIntegers.sort((x, y) => y - x))

    val anotherListOfIntegers: MyList[Int] = Cons(4, Cons(5, Cons(6, Empty)))
    println(listOfIntegers.zipWith(anotherListOfIntegers, (x, y) => x * y))

    println(listOfIntegers.fold(0)(_ + _))

    // 2 - toCurry/fromCurry
    def toCurry(f: (Int, Int) => Int): (Int => (Int => Int)) = {
      (x: Int) => (y: Int) => f(x, y)
    }

    def fromCurry(f: (Int => Int => Int)): (Int, Int) => Int = {
      (x: Int, y: Int) => f(x)(y)
    }

    val curried = toCurry((x: Int, y: Int) => x + y)
    println(curried(4)(5))

    val uncurried = fromCurry((x: Int) => (y: Int) => x + y)
    println(uncurried(2, 9))

    // 3 - compose/andThen
    def compose[A, B, C](f: B => C, g: A => B): A => C =
      x => f(g(x))
      
    def andThen[A, B, C](f: A => B, g: B => C): A => C =
      x => g(f(x))

    val add3: (Int => Int) = 
      x => x + 3

    val mul5: (Int => Int) =
      x => x * 5

    println(compose(add3, mul5)(2))
    println(andThen(add3, mul5)(2))
}
