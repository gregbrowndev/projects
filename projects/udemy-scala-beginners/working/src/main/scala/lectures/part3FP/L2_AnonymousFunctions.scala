package lectures.part3FP

object L2_AnonymousFunctions extends App {
    
    // The object oriented way to define a Function1 is:
    val doubler = new Function1[Int, Int] {
        override def apply(v1: Int): Int = v1 * 2
    }

    // the idiomatic FP way to do this is instead
    val doublerFP = (x: Int) => x * 2

    // this is called an anonymous or lambda function
    // The lambda is a value that is an instance of a Function1

    // Another way to write them:
    val doublerFP2: Int => Int = x => x * 2
    // because we declare the type of doubleFP2, we can drop
    // the arguement type in the expression

    // Multiple parameters in a Lambda
    val adder = (a: Int, b: Int) => a + b
    val adder2: (Int, Int) => Int = (a, b) => a + b

    // No params
    val justDoSomething: () => Int = () => 3

    // Important note: 
    println(justDoSomething)    // Out: lectures.part3FP.L2_AnonymousFunctions$$$Lambda$5/0x0000000800063040@34c4973
    println(justDoSomething())  // Out: 3

    // Unlike methods which can be invoked without supplying the parentheses,
    // doing this with a function will print the function object itself

    // Curly braces
    val stringToInt = { (str: String) =>
        str.toInt
    }

    // This style is quite common but not always loved!

    // More syntactic sugar
    val niceIncrementer: Int => Int = _ + 1  // equiv. to x => x + 1
    val niceAdder: (Int, Int) => Int = _ + _ // eqiv. to (a, b) = a + b

    // to use this syntax, the function type must be provided
}