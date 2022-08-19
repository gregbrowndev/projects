package lectures.part2OOP

object Generics extends App {
  // generic class (and trait)
  class Mylist[A] {
    // use type A
  }

  val listOfIntegers = new Mylist[Int]
  val listOfStrings = new Mylist[String]

  // generic methods
  object MyList {
    def empty[A]: Mylist[A] = ???
  }

  val emptyListOfIntegers = MyList.empty[Int]

  // variance problem
  class Animal

  class Cat extends Animal

  class Dog extends Animal

  // 1. Covariance
  // Enables you to use a more derived type than originally specified.
  // You can assign an instance of IEnumerable<Derived> to a variable of type IEnumerable<Base>.

  // We define a type parameter as covariant using +:
  class CovariantList[+A]

  val animal: Animal = new Cat
  val animalList: CovariantList[Animal] = new CovariantList[Cat]

  // CovariantList[Cat] <: CovariantList[Animal]

  // can we add a Dog to a covariant list of Animals?
  // animalList.add(new Dog)  ?? HARD QUESTION


  // 2. Invariance
  // Means that you can use only the type originally specified. An invariant generic type parameter is neither covariant nor contravariant.
  // You cannot assign an instance of List<Base> to a variable of type List<Derived> or vice versa.

  // We define a type parameter as invariant without any other symbol, note this is the default:
  class InvariantList[A]

  val invariantList: InvariantList[Animal] = new InvariantList[Animal]


  // 3. Contravariance
  // Enables you to use a more generic (less derived) type than originally specified.
  // You can assign an instance of Action<Base> to a variable of type Action<Derived>.
  class Trainer[-A]

  val trainer: Trainer[Cat] = new Trainer[Animal]

  // trainer[Animal] <: trainer[Cat]

  // In the example above, we want a trainer for our cat. Because Trainer is contravariant
  // in the Cat type parameter, this means we can assign a more generic Animal trainer.
  // What we are saying is: we want someone to train our cat, but it can be any more generic
  // trainer, they don't specifically have to be a cat trainer!


  // Bounded types
  // The Cage has type parameter A that is bound to be a subtype of Animal
  class Cage[A <: Animal](animal: A)

  val cage = new Cage(new Dog)

  class Car
  //  val newCage = new Cage(new Car)  // error!

  // bounded types are useful for solving a covariant problem, if we define an 'add' function
  // like below:
  class CoList[+A] {
    // def add(element: A): CoList[A] = ???
    // Error: covariant type A occurs in contravariant position in type A of parameter element
    // This is the same problem we asked in point 1 (HARD QUESTION)

    // To implement this correctly:
    def add[B >: A](element: B): CoList[B] = ???
  }

  // What this means is you can start with a CoList[Cat], but if you add(Dog) it will
  // return a CoList[Animal]
  val coList: CoList[Cat] = new CoList[Cat]
  val coList2 = coList.add(new Dog)
}
