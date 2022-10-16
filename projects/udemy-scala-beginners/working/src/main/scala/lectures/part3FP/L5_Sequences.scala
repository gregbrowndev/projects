package lectures.part3FP

import scala.util.Random

object L5_Sequences extends App {
  /* Sequences are a generic data structure that
       - have a well defined order
       - can be indexed

     Suppoorts various operations:
       - apply, iterator, length, reverse for indexing and iterating
       - concatenation, appending, prepending
       - a lot of others: grouping, sorting, zipping, searching, slicing
   */
  // Seq
  val aSequence = Seq(1, 4, 3, 2)
  println(aSequence)
  // prints out List(1, 4, 3, 2) because the apply method on Seq constructs
  // a List!
  println(aSequence.reverse)
  println(aSequence(2)) // apply method retrieves the item at the index
  println(aSequence ++ Seq(5, 6, 7))
  println(aSequence.sorted)

  // Ranges
  val aRange: Seq[Int] = 1 to 10
  aRange.foreach(println)
  (0 until 10).foreach(println)

  // List
  /* Official List signature

    sealed abstract class List[+A]
    case object Nil extends List[Nothing]
    case class ::[A](val hd: A, val tl: List[A]) extends List[A]

  A LinearSeq immutable linked list
    - head, tail, isEmpty methods are fast: O(1)
    - most operations are O(n), e.g. length, reverse

  Sealed - has two subtypes:
    - object Nil (empty)
    - class :: (remember Scala symbol names are very permissive)
   */
  // we've seen how to create a List using the apply method on the
  // companion object
  val aList = List(1, 2, 3)

  // prepending with :: operator
  val prepended = 42 :: aList
  println(prepended)

  // prepending with +: and appending with :+ operators
  println(42 +: aList :+ 89)

  val apples5 = List.fill(5)("apple")
  println(apples5)
  println(aList.mkString("-|-"))

  /* Arrays

    final class Array[T]
      extends java.io.Serializable
      with java.lang.Cloneable
  
  The equivalent of simple Java arrays
   - can be mutually constructed with predefined lengths
   - can be mutated
   - are interoperable with Java's T[] arrays
   - indexing is fast
  */
  val numbers = Array(1,2,3,4)
  val threeElements = Array.ofDim[Int](3)
  println(threeElements)  // prints out some ID/address
  threeElements.foreach(println)  // prints out three '0' values
  // values in arrays are initialised to some default value, such a 0 for int arrays
  // and null for string arrays (and other reference types, e.g. an array of Persons)
  Array.ofDim[String](3).foreach(println)

  // mutation
  numbers(2) = 0  // syntax sugar for numbers.update(2, 0) - note update is similar to the apply method as both have special behaviour
  println(numbers.mkString(" "))

  // arrays and seq
  val numbersSeq: Seq[Int] = numbers  // implicit conversion of array to seq
  println(numbersSeq)
  // this implicit conversion is why arrays have access to methods found on seq, e.g. mkstring
  // In IntelliJ, method calls that use implicit conversion are underlined

  /* Vector

    final class Vector[+A]

  The default implementation for immutable sequences
   - effectively constant indexed read and write: O(log32 n)
   - fast element addition: append/prepend
   - implemented as a fixed-branched trie (branch factor 32)
   - good performance for large sizes
  */
  val vector: Vector[Int] = Vector(1, 2, 3)
  println(vector)

  // vectors vs lists
  val maxRuns = 1000
  val maxCapacity = 1000000

  def getWriteTime(collection: Seq[Int]): Double = {
    val r = new Random
    val times = for {
      it <- 1 to maxRuns
    } yield {
      val currentTime = System.nanoTime()
      collection.updated(r.nextInt(maxCapacity), r.nextInt())
      System.nanoTime() - currentTime
    }

    times.sum * 1.0 / maxRuns
  }

  val numbersList = (1 to maxCapacity).toList
  val numbersVector = (1 to maxCapacity).toVector

  // List keeps a reference to the tail, so it's very efficient if you want to update the first element.
  // However, the list must be traversed if you want to update an element in the middle of the list
  println(getWriteTime(numbersList))  // 2662989.404 ns  (2.7 ms)

  // To update any part of a vector, the 32 branch tree must be traversed and the chunk must be replaced
  // Random indexing is efficient because it's only dependent on the depth of the tree which is small!
  // However, a downside of vector is the need to replace the 32-element chunk
  println(getWriteTime(numbersVector))  // 2309.442 ns (2.3 microseconds)

  // this demonstrates why vector is the default implementation of seq!
}
