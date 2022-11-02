package sections.section2_Intro

import scala.util.Try

object P5_DarkSugars extends App {
  // syntax sugar #1: methods with single param
  def singleArgMethod(arg: Int): String = s"$arg little ducks..."

  val description = singleArgMethod {
    // write some complex code
    42
  }

  // practical examples of this
  val aTryInstance = Try {
    // some code...
    // This syntax resembles the try {} catch {} syntax!
  }

  List(1, 2, 3).map { x =>
    x + 1
  }

  // syntax sugar #2: single abstract method
  trait Action {
    def act(x: Int): Int
  }

  val anInstance: Action = new Action {
    override def act(x: Int): Int = x + 1
  }

  val aFunkyInstance: Action = (x: Int) => x + 1

  // example: Runnables
  val aThread = new Thread(new Runnable {
    override def run(): Unit = println("hello, Scala")
  })

  val aBetterThread = new Thread(() => println("Hello, Scala!"))

  // this also works for abstract classes that have a single unimplemented
  // property
  abstract class AnAbstractType {
    def implemented: Int = 23
    def f(a: Int): Unit
  }

  val anAbstractInstance: AnAbstractType = (a: Int) => println("sweet")

  // syntax sugar #3: the :: and #:: methods are special

  val prependedList = 2 :: List(3, 4)
  // 2.::(List(3, 4))  - however, there is no .:: method on int! Instead, the
  // compiler writes it as List(3, 4).::(2)
  // this works because scala spec says: last char decides associativity of the
  // method, i.e. if it ends in a colon, it is right associative. Otherwise,
  // it is left associative.

  // E.g.
  1 :: 2 :: 3 :: List(4, 5)
  List(4, 5).::(3).::(2).::(1) // equivalent

  class MyStream[T] {
    def -->:(value: T): MyStream[T] = this // with some impl
  }

  val myStream = 1 -->: 2 -->: 3 -->: new MyStream[Int]
  // the -->: operator is right associative because it ends in a colon!

  // syntax sugar #4: multi-word method naming
  class TeenGirl(name: String) {
    def `and then said`(gossip: String): Unit = println(s"$name said $gossip")
  }

  val lilly = new TeenGirl("Lilly")
  lilly `and then said` "Scala is so sweat"

  // syntax sugar #5: infix types
  class Composite[A, B]
  //  val composite: Composite[Int, String] = ???
  val composite: Int Composite String = ??? // we can write the types like this

  // because naming is so permissive, we can create a class like this:
  class -->[A, B]
  val towards: Int --> String = ???
  // we will make use of names like this in the type-level section at the end
  // of the course

  // syntax sugar #6: update() is very special, much like apply()
  val anArray = Array(1, 2, 3)
  anArray(2) = 7 // rewritten to anArray.update(2, 7)
  // very useful in mutable collections
  // remember to add an update method to our custom mutable collections

  // syntax sugar #7: setters for mutable containers
  class Mutable {
    private var internalMember: Int = 0 // private for OO encapsulation
    def member: Int = internalMember // "getter"
    def member_=(value: Int): Unit =
      internalMember = value // "setter"
  }

  val aMutableContainer = new Mutable
  aMutableContainer.member = 43 // we can use the setter like the getter!
}
