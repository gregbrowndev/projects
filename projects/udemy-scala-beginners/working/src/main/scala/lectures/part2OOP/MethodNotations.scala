package lectures.part2OOP

import scala.annotation.targetName
import scala.language.postfixOps

object MethodNotations extends App {
  class Person(val name: String, favoriteMovie: String, val age: Int) {
    // Infix notation
    def likes(movie: String): Boolean = movie == favoriteMovie

    def hangoutWith(person: Person): String = s"${this.name} is hanging out with ${person.name}"

    def +(person: Person): String = s"${this.name} + ${person.name}"

    // Prefix notation
    // Note: in the unary method below, you need the space before the colon before the return type
    // Otherwise, the compiler will interpret the colon as part of the method name!
    def unary_! : String = s"$name, what the heck?"

    // Postfix notation
    def isAlive: Boolean = true

    // Apply method
    def apply(): String = s"Hi, my name is $name and I like $favoriteMovie"

    // Exercises
    def +(nickname: String): Person = new Person(s"${this.name} (${nickname})", this.favoriteMovie, this.age)

    def unary_+ : Person = new Person(this.name, this.favoriteMovie, this.age + 1)

    def learns(subject: String): String = s"${this.name} learns ${subject}"

    def apply(number: Int): String = s"${this.name} watched ${this.favoriteMovie} ${number} times"
  }

  val mary = new Person("Mary", "Inception", 20)
  println(mary.likes("Inception"))

  // Scala supports infix (or operator) notation for methods that have 1 parameter
  println(mary likes "Inception")

  val tom = new Person("Tom", "Fight Club", 18)
  println(mary hangoutWith tom)

  // The name operator notation relates to binary operators. We can even define methods,
  // such as '+' and '&' on the class if we wanted
  println(mary + tom)

  // Binary operators are actually methods that support the bracket syntax
  println(1 + 2)
  println(1.+(2)) // same thing

  // ALL OPERATORS ARE METHODS
  // Akka actors have operators like ! ?

  // Prefix Notation
  val x = -1 // equivalent with 1.unary-
  val y = 1.unary_-

  println(!mary)
  println(mary.unary_!)

  // Postfix Notation
  println(mary.isAlive)
  println(mary isAlive)

  // Postfix notation only works with methods with no parameters. In practice, we rarely use
  // this notation as the only difference is the dot. Also, it can make the code more difficult
  // to read due to spatial ambiguities

  // Apply
  // Defining a function named apply on a class has a special property. It makes the instance callable
  println(mary.apply())
  println(mary()) // equivalent


  /**
   * Exercises
   *
   * 1. Overload the + operator
   * mary + "the rockstar" => new person "Mary (the rockstar)"
   *
   * 2. Add an age to the Person class
   * Add a unary + operator => new Person with the age + 1
   * +mary => mary with the age incremented
   *
   * 3. Add a "learns" method in the Person class => "Mary learns Scala"
   * Add a learnsScala method, calls learns method with "Scala"
   * Use it in postfix notation
   *
   * 4. Overload the apply method
   * mary.apply(2) => "Mary watched Inception 2 times"
   */
  val rockstar = mary + "the rockstar"
  println(rockstar.name)

  val olderMary = +mary
  println(olderMary.age)

  println(mary learns "Scala")

  println(mary(2))
}
