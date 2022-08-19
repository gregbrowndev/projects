package lectures.part2OOP

object AbstractDataTypes extends App {

  // abstract
  abstract class Animal {
    val creatureType: String

    def eat(): Unit
  }

  class Dog extends Animal {
    override val creatureType: String = "Canine"

    def eat(): Unit = println("crunch, crunch")
    // Note: we have to provide implementations for the members and functions
    // on the abstract base class. The override keyword is optional
  }

  // traits
  trait Carnivore {
    def eat(animal: Animal): Unit
  }

  class Crocodile extends Animal with Carnivore {
    override val creatureType: String = "croc"

    override def eat(): Unit = println("snap, snap")

    override def eat(animal: Animal): Unit = println(s"I'm a croc and I'm eating ${animal.creatureType}")
  }

  val dog = new Dog
  val croc = new Crocodile

  croc.eat(dog)

  // traits vs abstract classes
  // - both traits and abstract classes allow abstract and non-abstract members
  // - traits do not have constructor parameters (this is possible in Scala 3)
  // - classes can mixin multiple traits but only inherit a single abstract base
  // - traits = behaviour, abstract = a "thing"
}
