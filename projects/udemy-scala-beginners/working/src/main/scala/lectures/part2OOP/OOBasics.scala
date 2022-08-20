package lectures.part2OOP

object OOBasics extends App {

  val person = new Person("Bob", 25)

  println(person.age)
  // Note: If we try to print .age, we'll see a compilation error
  // class parameters are NOT FIELDS, i.e. a parameter of the constructor
  // does not become accessible on the class

  // In order for it to become a class field, we need to add `val` to the
  // class definition

  person.greet("Greg")
}

// constructor
class Person(val name: String, val age: Int) {
  // body of the class (not its constructor)
  // we can do anything inside this block that we can in a block
  // expression, including val/var, expressions like println, and
  // functions

  // val definitions are FIELDS that can be accessed outisde the class
  val x = 2

  println(1 + 3)

  // Note: the class parameter in the constructor can be accessed using this.name
  // even though its not a class field
  def greet(name: String): Unit = println(s"${this.name} says: Hi, $name")

  // Note: in the function below that "this." is optional. The variable will always
  // resolve to the class parameter unless the function parameter shadows the name
  def greet(): Unit = println(s"${this.name} says: Hi, $name")

  // Note: Scala supports function overloading

  // In Scala, classes can also have multiple constructors defined like below.
  // Secondary (or tertiary) constructors are restricted to call other constructors
  def this(name: String) = this(name, 0)

  def this() = this("John Doe")
  // This makes additional constructors impractical as they can only be used to write
  // default constructors. We could more easily solve this by using default parameters
  // in the primary constructor

  // exercises
  val author = new Writer("Charles", "Dickens", 1812)
  val imposter = new Writer("Charles", "Dickens", 1812)
  val novel = new Novel("Great Expectations", 1861, author)

  println(novel.authorAge())
  println(
    novel.isWrittenBy(imposter)
  ) // returns false as it compares by object reference

  val counter = new Counter()
  counter.inc().print()
}

/**
 * Exercise
 *
 * 1) Write a Novel and a Write class
 *
 * Writer: first name, surname, year
 *  - method fullname
 *
 * Novel: name, year of release, author
 *  - authorAge
 *  - isWrittenBy(author)
 *  - copy (new year of release) = new instance of Novel
 */
class Writer(firstName: String, surname: String, val yearOfBirth: Int) {
  def fullName(): String = firstName + " " + surname
}

class Novel(name: String, yearOfRelease: Int, var author: Writer) {
  def authorAge(): Int = yearOfRelease - author.yearOfBirth

  def isWrittenBy(author: Writer): Boolean = this.author == author

  def copy(newYearOfRelease: Int): Novel = Novel(name, newYearOfRelease, author)
}

// Note: isWrittenBy compares the two authors by reference. However, in this
// case, we'd expect to use compare by value. We'll look at that in a following lesson

/**
 * 2) Write a Counter class
 *   - receives an int value
 *   - method current count
 *   - method to increment/decrement => new Counter
 *   - overload inc/dec to receive an amount
 */
class Counter2(var count: Int) {
  def currentCount(): Int = count

  def increment(): Unit = increment(1)

  def increment(amount: Int = 1): Unit = count = count + amount

  def decrement(): Unit = decrement(1)

  def decrement(amount: Int = 1): Unit = count = count - amount
}

// Note: we should always use immutability in FP!
class Counter3(val count: Int) {
  def inc(): Counter3 = inc(1)

  def inc(amount: Int = 1): Counter3 = new Counter3(
    count + amount
  ) // immutability

  def dec(): Counter3 = dec(1)

  def dec(amount: Int = 1): Counter3 = new Counter3(count - amount)
}

// If we wanted to print a log everytime the counter is inc or dec:
class Counter(val count: Int = 0) {
  def inc(): Counter = {
    println("incrementing")
    new Counter(count + 1)
  }

  def dec(): Counter = {
    println("decrementing")
    new Counter(count - 1)
  }

  def inc(n: Int): Counter = {
    if (n <= 0) this
    else inc().inc(n - 1)
  }

  def dec(n: Int): Counter = {
    if (n <= 0) this
    else dec().dec(n - 1)
  }

  def print(): Unit = println(count)
}
