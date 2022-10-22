package lectures.part4PM

import scala.util.Random

object L1_PatternMatching extends App {
  // Pattern matching on the surface is like a switch on steroids...
  val random = new Random
  val x = random.nextInt(10)

  val description = x match {
    case 1 => "the ONE"
    case 2 => "double or nothing"
    case 3 => "third time is the charm"
    case _ => "something else"
  }
  println(x)
  println(description)

  // However, digging deeper, there are many other power use cases!
  // 1. Decompose values
  case class Person(name: String, age: Int)
  val bob = Person("Bob", 20)

  val greeting = bob match {
    case Person(n, a) => s"Hi, my name is $n and I am $a years old"
    case _            => "I don't know who I am"
  }
  println(greeting)

  // 2. Guards
  val greetingWithGuard = bob match {
    case Person(n, a) if a <= 20 =>
      s"Hi, my name is $n and I can't drink in the US"
    case Person(n, a) => s"Hi, my name is $n and I am $a years old"
    case _            => "I don't know who I am"
  }
  println(greetingWithGuard)
  /* Notes
    1. Cases are matched in order
    2. If no cases match, a MatchError is raised!
    3. The type of a pattern match expression is the union of the return types of all cases
    4. Pattern matching works really well with case classes
   */

  // 3. Pattern matching on sealed hierarchies
  sealed trait Animal
  case class Dog(breed: String) extends Animal
  case class Parrot(greeting: String) extends Animal

  val animal: Animal = Dog("Terra Nova")
  animal match {
    case Dog(someBreed) => println(s"Matched a dog of the $someBreed breed")
  }
  // we get a compiler WARNING in the code above as we have not covered all cases!

  // Some devs that learn pattern matching for the first time, will
  // try to use it everywhere! Don't be a hammer!
  val isEven = x match {
    case n if n % 2 == 0 => true
    case _               => false
  }
  // likewise, this is also bad ;)
  val isEvenCond = if (x % 2 == 0) true else false // ?!
  // just write...
  val isEvenNormal = x % 2 == 0

  /* Exercise
    Write a simple function using PM that
     takes an Expr => human readable form

    E.g. Sum(Number(2), Number(3)) => 2 + 3
   */
  trait Expr
  case class Number(n: Int) extends Expr
  case class Sum(e1: Expr, e2: Expr) extends Expr
  case class Prod(e1: Expr, e2: Expr) extends Expr

  def show(expr: Expr): String =
    expr match {
      case Number(n)   => s"$n"
      case Sum(e1, e2) => show(e1) + " + " + show(e2)
      case Prod(e1, e2) => {
        def showParentheses(expr: Expr) = expr match {
          case Sum(_, _) => "(" + show(expr) + ")"
          case _         => show(expr)
        }

        showParentheses(e1) + " * " + showParentheses(e2)
      }
    }

  println(show(Sum(Number(2), Number(3))))
  println(show(Sum(Sum(Number(2), Number(3)), Number(4))))
  println(show(Prod(Sum(Number(2), Number(1)), Sum(Number(3), Number(4)))))
  println(show(Sum(Prod(Number(2), Number(1)), Number(3))))
}
