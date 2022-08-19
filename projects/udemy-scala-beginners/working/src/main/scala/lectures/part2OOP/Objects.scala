package lectures.part2OOP

object Objects extends App {
  // SCALA DOES NOT HAVE CLASS-LEVEL (STATIC) FUNCTIONALITY

  // In Java, we can define static properties on a class that can be accessed statically.
  // In Scala, we cannot do this. However, we have something better - objects! In most,
  // OOP languages, an object is an instance of a class. In Scala, an object is a much
  // deeper, dedicated concept.

  object Person { // Singleton - we've defined a Person type + its only instance
    // class-level (static) functionality
    val N_EYES = 2

    def canFly: Boolean = false

    // factory method using apply - we can make the singleton a callable factory
    def apply(mother: Person, father: Person): Person = new Person("Bobbie")
  }

  println(Person.N_EYES)
  println(Person.canFly)

  // Scala objects = SINGLETONS
  val mary = Person
  val john = Person
  println(mary == john)

  // We can also define a class with the same name in the same scope
  class Person(val name: String) {
    // instance-level functionality
  }

  // We call this functionality COMPANIONS

  // With the companion class, if we instantiate using new then these will be
  // different objects
  val ben = new Person("Ben")
  val laura = new Person("Laura")
  println(ben == laura)

  // but you can still get hold of the singleton object and this will always be a singleton!

  // Using the factory on the object. Note, we defined it using apply
  val bobbie = Person(laura, ben)
  println(bobbie.name)

  // Scala Applications = Scala object with
  // def main(args: Array[String]): Unit
}
