package sections.section2_Intro

object P7_AdvancedPatternMatchingP2 extends App {
  // In this lesson, we'll learn about infix patterns for decomposing sequences
  // and writing custom return types for unapply

  // Infix patterns
  case class Or[A, B](a: A, b: B)
  val either = Or(2, "two")
  val humanDescription = either match {
    // case Or(number, string) => s"$number is written as $string"
    // we can also write this as:
    case number Or string => s"$number is written as $string"
  }

  println(humanDescription)

  // decomposing sequences
  val numbers = List(1)
  val vararg = numbers match {
    case List(1, _*) => "starting with 1"
  }

  // because lists have a variable number of args, the standard way to write
  // unapply doesn't work. If we have our own sequence collection that we want
  // to patten match, we need to use unapplySequence

  abstract class MyList[+A] {
    def head: A = ???
    def tail: MyList[A] = ???
  }

  case object Empty extends MyList[Nothing]
  case class Cons[+A](override val head: A, override val tail: MyList[A])
      extends MyList[A]

  object MyList {
    // A companion object with unapplySeq that turns MyList into a Seq
    def unapplySeq[A](list: MyList[A]): Option[Seq[A]] =
      if (list == Empty) Some(Seq.empty)
      else unapplySeq(list.tail).map(list.head +: _)
    // unapplySeq returns a optional Seq, so if we get some value, we prepend
    // the head element to the seq to build a sequence with the same elements
    // in the same order
  }

  val myList: MyList[Int] = Cons(1, Cons(2, Cons(3, Empty)))
  val decomposed = myList match {
    case MyList(1, 2, _*) => "starting with 1, 2"
    case _                => "something else"
  }

  println(decomposed)
  // This works because elements (1, 2, _*) are matched against the option Seq

  // Custom return types for unapply
  // unapply and unapplySeq don't need to return an Option, but the data
  // structure returned must hav methods: isEmpty: Boolean, get: something

  abstract class Wrapper[T] {
    def isEmpty: Boolean
    def get: T
  }

  class Person(val name: String, val age: Int)
  object PersonWrapper {
    def unapply(person: Person): Wrapper[String] = new Wrapper[String] {
      override def isEmpty: Boolean = false
      override def get: String = person.name
    }
  }

  val bob = Person("Bob", 35)
  println(bob match {
    case PersonWrapper(name) => s"This person's name is $name"
    case _                   => "An alien"
  })

  // note: using this pattern is very rare
}
