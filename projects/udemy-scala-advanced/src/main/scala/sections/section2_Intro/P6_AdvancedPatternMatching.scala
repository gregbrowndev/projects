package sections.section2_Intro

object P6_AdvancedPatternMatching extends App {

  val numbers = List(1)
  val description = numbers match {
    // complex pattern!
    case head :: Nil => println(s"the only element is $head.")
    case _           =>
  }

  /* Currently, we know about the following pattern matchable structures:
    - constants
    - wildcards
    - case classes
    - tuples
    - some special magic like above

    In this section, we'll look at how to make our own structures pattern
    matchable, in the event we ever need them to be!
   */

  // Let's assume we can't use a case class. How do we make an ordinary class
  // pattern matchable?
  class Person(val name: String, val age: Int)

  // we need to create a companion object with an unapply method
  object Person {
    def unapply(person: Person): Option[(String, Int)] =
      if (person.age < 21) None
      else Some((person.name, person.age))
  }

  val bob = new Person("Bob", 25)
  val greeting = bob match {
    // now we can PM on Person!
    case Person(n, a) => s"Hi, my name is $n and I am $a years old."
  }

  println(greeting)

  val lilly = new Person("Lilly", 20)
//  val greeting2 = lilly match {
//    // will get MatchError!
//    case Person(n, a) => s"Hi, my name is $n and I am $a years old."
//  }

  // Note: We could name the companion object "object Person" to anything, e.g.
  // "object PersonPatternMatchers" (of course it wouldn't be a companion object
  // then). But the point is, as long as there is a function with the signature
  // unapply(person: Person): Option[(String, Int])

  object PersonPatternMatchers {
    def unapply(person: Person): Option[(String, Int)] = Some(
      (person.name, person.age)
    )

    def unapply(age: Int): Option[String] =
      Some(if (age < 21) "minor" else "major")
  }

  val mark = new Person("Mark", 25)
  val greeting3 = bob match {
    // now we can PM on Person!
    case PersonPatternMatchers(n, a) =>
      s"Hi, my name is $n and I am $a years old."
  }

  // we can even overload unapply
  val legalStatus = bob.age match {
    case PersonPatternMatchers(status) => s"My legal status is $status"
  }
  println(legalStatus)

  /* Exercise
  Let's imagine we want to test for dozens of conditions, e.g. where
  a number x is a single digit, is even, etc. We could write the code like
  below, but this would be very verbose.

  The exercise is to devise a custom pattern matching solution to this problem.
   */
  val n: Int = 45
  val mathProperty = n match {
    case x if x < 10     => "single digit"
    case x if x % 2 == 0 => "an even number"
    // and so on
    case _ => "no property"
  }

  // My solution: only the right lines but was quite exactly what the tutor
  // wanted
  object Condition {
    def unapply(x: Int): Option[String] =
      if (x < 10) Some("single digit")
      else if (x % 2 == 0) Some("an even number")
      else Some("no property")
  }

  val mathProperty2 = n match {
    case Condition(p) => s"Number $n identified as: $p"
  }

  println(mathProperty2)

  // Solution: create an object for each condition with an unapply. Note, the
  // convention is to name the object lower case
  object singleDigit {
    def unapply(x: Int): Option[Boolean] =
      if (-10 < x && x < 10) Some(true)
      else None
  }

  object even {
    def unapply(x: Int): Option[Boolean] =
      if (x % 2 == 0) Some(true)
      else None
  }

  val mathProperty3 = n match {
    case singleDigit(_) => "single digit"
    case even(_)        => "an even digit"
    case _              => "no property"
  }

  println(mathProperty3)

  // Note, we can clean up the above code even more. Clearly it doesn't matter
  // if we returned Some(true) or Some(false) as the value is not used (the PM
  // only cares if it received Some or None). Instead, we can reduce it to a
  // single boolean expression like below:
  object singleDigit2 {
    def unapply(x: Int): Boolean = -10 < x && x < 10
  }

  object even2 {
    def unapply(x: Int): Boolean = x % 2 == 0
  }

  val mathProperty4 = n match {
    case singleDigit2() => "single digit"
    case even2()        => "an even digit"
    case _              => "no property"
  }

  println(mathProperty4)

  /* The benefit of this approach is that you can reuse these conditions
  throughout our codebase. However, the code to define each condition is a
  little bit more verbose.
   */
}
