package lectures.part2OOP

object Inheritance extends App {

  class Animal {
    val creatureType: String = "wild"

    def eat(): Unit = println("nomnomnom")
    // Note: we can set access modifiers such as protected to make a method only
    // accessible within the class and its subclasses
  }

  class Cat extends Animal {
    def crunch(): Unit = {
      eat()
      println("crunch crunch")
    }
  }

  val cat = new Cat
  cat.crunch()

  // constructors
  class Person(name: String, age: Int)

  class Adult(name: String, age: Int, idCard: String) extends Person(name, age)

  // overriding
  class Dog(dogType: String) extends Animal {
    override val creatureType: String = dogType

    override def eat(): Unit = println("crunch, crunch")
  }

  val dog = new Dog("K9")
  dog.eat()
  println(dog.creatureType)

  // Note: we can also override directly in the constructor:
  //  class Dog(override val creatureType: String) extends Animal { }

  // type substitution
  val unknownAnimal: Animal = new Dog("K9")
  unknownAnimal.eat()

  // super
  class Cow extends Animal {
    override def eat(): Unit = {
      super.eat()
      println("moo")
    }
  }

  val cow = new Cow()
  cow.eat()

  // final
  // the final keyword prevents further overriding of a function, class member or class itself

  // sealed = a "soft final" that allows extension only within this file

  // access modifiers: private, protected, public (default)
}
