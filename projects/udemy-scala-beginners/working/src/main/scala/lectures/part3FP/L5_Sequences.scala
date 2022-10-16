package lectures.part3FP

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
}
