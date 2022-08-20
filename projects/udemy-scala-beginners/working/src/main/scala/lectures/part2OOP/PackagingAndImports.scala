package lectures.part2OOP

import playground.{Cinderella => Princess, PrinceCharming}

object PackagingAndImports extends App {
  // Interestingly, the package stanza on line 1 is one of only a few pieces of
  // syntax that isn't an expression

  // package members are accessible by their simple name
  val writer = new Writer("Daniel", "RockTheJVM", 2018)

  // objects in other packages have to be imported
  val cinderella = new Princess()
  // we could also "fully qualify" the class name instead of using import
  // val cinderella = new playground.Cinderella()

  // package objects - these are niche but they allow us to write "global"
  // functions and properties that are not tied to any specific object/class
  // and can be accessed anywhere within the package
  // there can be only one package object per package and it follows the naming
  // convention, "package.scala", it contains a single "package object" with
  // the same name as the package it is in
  sayHello
  println(SPEED_OF_LIGHT)

  // the classes Cinderella and PrinceCharming are both in the same package,
  // 'playground', when we import both they will be grouped together
  val princeCharming = new PrinceCharming()

  // we can import everything from a package using:
  // import playground._

  // we can do name aliasing at import too
  // import playground.{Cinderella => Princess}

  // Note: if we have name collisions, aliasing and fully-qualifying the class
  // name are useful features

  // default imports
  // java.lang - String, Object, Exception
  // scala - Int, Nothing, Function
  // scala. Predef - println, ???
}
