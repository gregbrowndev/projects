package lectures.part2OOP

object CaseClasses extends App {

  /**
   * equals, hashCode, toString
   */

  case class Person(name: String, age: Int)

  // 1. class parameters are fields
  val jim = new Person("Jim", 34)
  println(jim.name)

  // 2. sensible toString
  println(jim)

  // 3. equals and hashCode implemented out of the box
  val jim2 = new Person("Jim", 34)
  println(jim == jim2)

  // 4. case classes have handy copy methods
  val jim3 = jim.copy(age = 46)
  println(jim3)

  // 5. case classes have companion objects
  val thePerson = Person
  val mary = Person("Mary", 23)

  // 6. case classes are serialisable
  // extremely useful for Akka

  // 7. case classes have extractor patterns, e.g. can be used in PATTERN MATCHING

  // 8. also possible: case objects
  case object UnitedKingdom {
    def name: String = "The UK of GB and NI"
  }

  // Exercise: complete exercise 4!
}
