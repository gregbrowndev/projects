package lectures.part1Basics

import scala.annotation.tailrec

object DefaultArgs extends App {

  @tailrec
  def tailRecursiveFactorial(n: Int, acc: Int): Int =
    if (n <= 1) acc
    else tailRecursiveFactorial(n - 1, n * acc)

  val fact10 = tailRecursiveFactorial(10, 1)

  // The problem with the above code is the initial value of
  // the accumulator is always required, spoiling the func
  // signature. We could wrap this with another function to
  // hide this. However, there is another way:

  @tailrec
  def tailRecursiveFactorial2(n: Int, acc: Int = 1): Int =
    if (n <= 1) acc
    else tailRecursiveFactorial2(n - 1, n * acc)

  val fact12 = tailRecursiveFactorial2(12)

  // this cleans up the code but the developer can still provide an
  // accumulator if they wanted.

  // lets look at another example.
  def savePicture(format: String = "jpg", width: Int, height: Int): Unit =
    println("saving")

  // The format has a default value "jpg". However, we need to be careful, as in the code below,
  // the argument 800 is interpreted to be the format
  // savePicture(800, 600)

  // instead, we can call the function with kwargs (or provide all arguments positionally)
  savePicture(width = 800, height = 600)

}
