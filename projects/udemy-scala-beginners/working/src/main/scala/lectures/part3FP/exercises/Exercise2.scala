package lectures.part3FP.exercises

object Exercise2 extends App {

  /**
   * 1. MyList: replace all FunctionX calls with Lambdas 
   * 2. Rewrite "special" curried adder as anon function
   */

    // 2
    val superAdd = (x: Int) => (y: Int) => x + y
    println(superAdd(3)(4))
}
