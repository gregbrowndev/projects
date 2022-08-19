package lectures.part2OOP

object AnonymousClasses extends App {
  abstract class Animal {
    def eat(): Unit
  }

  val funnyAnimal: Animal = new Animal {
    override def eat(): Unit = println("moooo")
  }

  println(funnyAnimal.getClass)

  // Animal is an abstract class so how is the funnyAnimal function
  // able to instantiate one? It turns out the syntax `new Animal {...}`
  // creates an instance of an anonymous class. It is equivalent to the code
  // below:

  /**
   * class AnomymousClasses$$anon$1 extends Animal {
   * override def eat(): Unit = println("moooo")
   * }
   *
   * val funnyAnimal: Animal = new AnomymousClasses$$anon$1
   */

  // Note: you need to make sure your anonymous class properly overrides all
  // abstract properties and passes any required constructor arguments

  // Note: you can also create anonymous classes from regular classes and traits
  // too, not just abstract class
}
