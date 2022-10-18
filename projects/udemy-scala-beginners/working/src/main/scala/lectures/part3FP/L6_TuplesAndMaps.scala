package lectures.part3FP

object L6_TuplesAndMaps extends App {
    // tuple - finite ordered "lists"
    val aTuple = new Tuple2(2, "Hello Scala")  // Tuple2[Int, String]  (Int, String)
    // We can also write tuples with a syntax similar to Python!
    val anotherTuple = (2, "Hello Scala")

    // Note: Tuples can group up to 22 elements of different types. This aligns with the 
    // the Function22 type.

    println(aTuple._1)  // access
    println(aTuple.copy(_2 = "Goodbye Java"))
    println(aTuple.swap)  // ("Hello Scala", 2)

    // Maps - key -> values
    val aMap: Map[String, Int] = Map()

    val phoneBook = Map(("Jim", 555), "Danial" -> 789)
    // Note: a -> b is sugar for (a, b)
    println(phoneBook)  // Map(Jim -> 555, Danial -> 789)

    // map operations
    println(phoneBook.contains("Jim"))
    println(phoneBook("Jim"))
    // println(phoneBook("Mary"))  // will throw a ExceptionInInitializerError!
    // we can create maps that return default values instead of throwing:
    val anotherMap = Map[String, Int]().withDefaultValue(0)

    // add a pairing
    val newPairing = "Mary" -> 678
    val newPhonebook = phoneBook + newPairing
    println(newPhonebook)

    // functionals on maps
    // map, flatMap, filter
    println(phoneBook.map(pair => pair._1.toLowerCase() -> pair._2))

    // filterKeys
    println(phoneBook.filterKeys(x => x.startsWith("J")).toMap)
    // Note: filterKeys returns a MapView, a lazy collection to optimise operations. 
    // This needs to be forced into a map to evaluate it for us to print it

    // mapValues
    println(phoneBook.mapValues(number => number * 10).toMap)

    // conversions to other collections
    println(phoneBook.toList)
    println(List(("Daniel", 555)).toMap)

    val names = List("Bob", "James", "Angela", "Mary", "Daniel", "Jim")
    println(names.groupBy(name => name.charAt(0)))
}
