package lectures.part3FP

object HOFsCurries extends App {
  
    val superFunction: (Int, (String, (Int => Boolean)) => Int) => (Int => Int) = null
    // The superFunction takes 2 params, an Int and another function, and it returns a func
    
    // A function that takes another function as an argument or returns a function is called
    // a Higher-Order Function (HOF)

    // Lets define a function that applies a func n times over a value x
    val nTimes: (f: Int => Int, n: Int, x: Int) => Int = (f, n, x) => { 
        if (n <= 0) x
        else nTimes(f, n-1, f(x))
    }
    
    val plusOne = (x: Int) => x + 1
    println(nTimes(plusOne, 10, 1))

    // Let's look at a better way to implement this nTimes function
    // ntb(f, n) = x => f(f(f...(x)))
    // increment10 = ntb(plusOne, 10) = x => plusOne(plusOne...(x))
    // val y = increment10(1)
    val nTimesBetter: (f: Int => Int, n: Int) => (Int => Int) = (f, n) => {
        if (n <= 0) (x: Int) => x  // identity function
        else (x: Int) => nTimesBetter(f, n-1)(f(x))
    }

    val plus10 = nTimesBetter(plusOne, 10)
    println(plus10(1))

    // The plus10 function is a curried function
    // Lets look at the superAdder again:
    val superAdder: Int => (Int => Int) = (x: Int) => (y: Int) => x + y
    val add3 = superAdder(3)  // lambda y => 3 + y
    println(add3(10))
    // we can also call the function with all the param lists
    println(superAdder(3)(10))

    // Scala supports another almost different kind of curried function with
    // multiple parameter lists
    def curriedFormatter(c: String)(x: Double): String = c.format(x)
    
    val standardFormat: (Double => String) = curriedFormatter("%4.2f")
    val preciseFormat: (Double => String) = curriedFormatter("%10.8f")

    println(standardFormat(Math.PI))
    println(preciseFormat(Math.PI))

    // Again note for functions with multiple parameter lists, we must specify the
    // type of value after applying it otherwise we
    // get a compiler error because Scala doesn't know what its type will be.
    // We will look at the proper way to do this later in the course when we introduce
    // partial application
}
