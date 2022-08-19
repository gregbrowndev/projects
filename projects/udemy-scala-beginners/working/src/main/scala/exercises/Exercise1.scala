package exercises
package exercise1

/**
 * Exercise 1: Implement the MyList abstract class with the following methods
 *   1. head = first element of the list
 *      2. tail = remainder of the list
 *      3. isEmpty = is this list empty
 *      4. add(int) => new list with this element added
 *      5. toString => a string representation of the list
 */

abstract class MyList {
  def head(): Option[Int]

  def tail(): MyList

  def isEmpty: Boolean

  def add(item: Int): MyList

  def toString: String
}

// This was my implementation of the MyList abstract class. It is naive because
// all it does is just wrap the built-in List type. This wasn't what the exercise
// was getting at.
class NaiveList(val list: List[Int]) extends MyList {

  override def head(): Option[Int] = list.headOption

  override def tail(): MyList = new NaiveList(list.tail)

  override def isEmpty: Boolean = list.isEmpty

  override def add(item: Int): MyList = new NaiveList(item :: list)

  override def toString: String = "This is a list"
}

// Here's how to implement MyList:
abstract class MyList2 {
  def head(): Int // dont need Optional here

  def tail(): MyList2

  def isEmpty: Boolean

  def add(item: Int): MyList2

  // To implement the toString function, we'll add an external function
  // printElements that will delegate to the subclass
  // Note: toString requires "override" as it is defined on the AnyRef class
  override def toString: String = "[" + printElements + "]"

  // polymorphic call
  def printElements: String
}

object Empty extends MyList2 {
  def head(): Int = throw new NoSuchElementException

  def tail(): MyList2 = throw new NoSuchElementException

  def isEmpty: Boolean = true

  def add(item: Int): MyList2 = new Cons(item, Empty)

  def printElements: String = ""
}

class Cons(h: Int, t: MyList2) extends MyList2 {
  def head(): Int = h

  def tail(): MyList2 = t

  def isEmpty: Boolean = false

  def add(item: Int): MyList2 = new Cons(item, this)

  def printElements: String = {
    if (t.isEmpty) "" + h
    else h + " " + t.printElements
  }
}


object Exercise1MyList extends App {
  val list = new NaiveList(List[Int]())

  val list2 = new Cons(1, Empty)
  println(list2.head())
  println(list2.toString)
}