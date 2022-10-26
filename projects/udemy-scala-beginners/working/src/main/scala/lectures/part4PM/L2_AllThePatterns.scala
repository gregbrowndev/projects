package lectures.part4PM

import lectures.part3FP.exercises.Solution4.{Cons, MyList, Empty}

object L2_AllThePatterns extends App {
  // In this lecture, we'll look at all the other ways
  // to do pattern matching
  // 1 - constants
  val x: Any = "Scala"
  val constants = x match {
    case 1                 => "a number"
    case "Scala"           => "THE Scala"
    case true              => "The Truth"
    case L2_AllThePatterns => "A singleton object"
  }

  // 2 - match anything
  // 2.1 - wildcard
  val matchAnything = x match {
    case _ =>
  }

  // 2.2 - variable
  val matchVariable = x match {
    case something => s"I've found $something"
  }

  // 3 - tuples
  val aTuple = (1, 2)
  val matchATuple = aTuple match {
    case (1, 1)         =>
    case (something, 2) => s"I've found $something"
  }

  val nestedTuple = (1, (2, 3))
  val matchANestedTuple = nestedTuple match {
    // PM can be nested!
    case (_, (2, v)) =>
  }

  // 4 - case classes
  val aList: MyList[Int] = Cons(1, Cons(2, Empty))
  val matchAList = aList match {
    case Empty                              =>
    case Cons(head, Cons(subhead, subtail)) =>
    case Cons(head, tail)                   =>
    // PM can be nested with case classes to!
  }

  // 5 - list patterns
  val aStandardList = List(1, 2, 3, 4)
  val standardListMatching = aStandardList match {
    case List(1, _, _, _)   => // extractor - advanced
    case List(1, _*)        => // a list of arbitrary length - advanced
    case 1 :: List(_)       => // infix pattern
    case List(1, 2, 3) :+ 4 => // infix pattern
  }

  // 6 - type specifiers
  val unknown: Any = 2
  val unknownMatch = unknown match {
    case list: List[Int] => // explicit type specifiers
    case _               =>
  }

  // 7 - name binding
  val nameBindingMatch = aList match {
    case noneEmpty @ Cons(_, _)     => // name binding - use the name later
    case Cons(1, rest @ Cons(2, _)) => // name binding inside nested patterns
  }

  // 8 - multi-patterns
  val multipattern = aList match {
    case Empty | Cons(0, _) => // compound pattern (multi-pattern)
  }

  // 9 - if guards
  val secondElementSpecial = aList match {
    case Cons(_, Cons(specialElement, _)) if specialElement % 2 == 0 =>
  }

  /*
    Question
   */
  val numbers = List(1, 2, 3)
  val numbersMatch = numbers match {
    case listOfStrings: List[String] => "a list of strings"
    case listOfNumbers: List[Int]    => "a list of numbers"
    case _                           => ""
  }

  println(numbersMatch) // "a list of strings"
  // JVM trick question!
  // The JVM was designed for the Java language, and the Java
  // language was designed with backwards compatibility in mind.
  // E.g. a JVM version 9 can run a Java 1 program written 20 years
  // ago. Unfortunately, generic parameters were not introduced until
  // Java 5. To maintain backwards compatibility, the compiler removes
  // all generic types after type checking. This means the JVM is
  // oblivious to generic types! Because of this Scala is also affected.
  // What the JVM actually sees:
  val numbersMatchv2 = numbers match {
    case listOfStrings => "a list of strings"
    case listOfNumbers => "a list of numbers"
    case _             => ""
  }

  // This is called type erasure!
}
