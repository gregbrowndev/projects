package sections.section2_Intro

import scala.annotation.tailrec
import scala.jdk.Accumulator

object P4_Recap extends App {
  val aCondition: Boolean = false
  val aConditionedVal = if (aCondition) 42 else 65
  // instructions vs expressinos

  val aCodeBlock = {
    // we'll be writing lots of code blocks
    if (aCondition) 54
    56 // the last expression is returned
  }

  // Unit == void
  val theUnit = println("hello, Scala")

  // functions
  def aFunction(x: Int): Int = x + 1

  // recursion: stack and tail
  @tailrec def factorial(n: Int, accumulator: Int): Int =
    if (n <= 0) accumulator
    else factorial(n - 1, n * accumulator)

  // Object-Oriented Programming
  class Animal
  class Dog extends Animal
  val aDog: Animal = new Dog // subtyping polymorphism

  trait Carnivore {
    def eat(animal: Animal): Unit
  }

  class Crocodile extends Animal with Carnivore {
    override def eat(animal: Animal): Unit = println("Crunch!")
  }

  // method notations
  val aCroc = new Crocodile
  aCroc.eat(aDog)
  aCroc eat aDog  // natural language

  1.+(2)  // equiv to 1 + 2

  // anonymous classes
  val aCarnivore = new Carnivore:
    override def eat(animal: Animal): Unit = println("Roar!")

  // generics
  abstract class MyList[+A]  // variance and variance problems in THIS course

  // singletons and companions
  object MyList

  // case classes
  case class Person(name: String, age: Int)

  // exceptions and try/catch/finally
  val throwsException = throw new RuntimeException  // Nothing
  val aPotentialFailure = try {
    throw new RuntimeException
  } catch {
    case e: Exception => "I caught an exception"
  } finally {
    println("some logs")
  }

  // packaging and imports
  // we won't care too much are packaging and imports in this class

  // We concluded the OOP section by realising that Scala is a more
  // object-oriented than the canonical OOP languages, such as Java and C++,
  // because everything in Scala is either an object, class, or package-object.

  // Functional Programming

  // we started the FP section by questioning what a function actually is:
  // functions are instances of classes with an apply method!
  val incrementer = new Function1[Int, Int] {
    override def apply(v1: Int): Int = v1 + 1
  }

  incrementer(1)

  val anonymousIncrementer = (x: Int) => x + 1
  List(1,2,3).map(anonymousIncrementer) // HOF
  // map, flatMap, filter

  // for-comprehension - syntax sugar for a chain of flatMaps
  val pairs = for {
    num <-  List(1,2,3) if num % 2 == 0  // if condition
    char <- List('a', 'b', 'c')
  } yield num + "-" + char

  // Scala collectionos: Seqs, Arrays, Lists, Vectors, Maps, Tuples
  val aMap = Map(
    "Daniel" -> 789,
    "Jess" -> 555
  )

  // "collections": Options, Try  (monads)
  val anOption = Some(2)

  // pattern matching
  val x = 2
  val order = x match {
    case 1 => "first"
    case 2 => "second"
    case 3 => "third"
    case _ => x + "th"
  }

  val bob = Person("bob", 22)
  val greeting = bob match {
    case Person(n, _) => s"Hi, my name is $n"
  }
  
  // finally we looked at all the possible patterns
}
