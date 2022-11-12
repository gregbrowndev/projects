Haskell's type system is so powerful, it is best to approach it as a separate
language that works in conjunction with what we learned in unit 1.

In this unit:
  - we'll learn what it means to program in types
  - we'll combine types in ways that aren't possible in other languages
  - see how types can take arguments of their own
  - see how using the right types can eliminate an entire class of bugs

We can use a type signature as a description of a transformation.
`CoffeeBeans -> CoffeeGrounds` probably describes the `grind` function.
`CoffeeGrounds -> Water -> Coffee` most likely describes the `brew` function.
Types in Haskell allow you to view programs as a series of transformations.

For example, let's say you want to find all the numbers in a large text doc
and add them together. How might we go about solving this?

First, represent the document as a string:

```haskell
type Document = String
-- parse the doc into tokens:     Document -> [String]
-- check each token is a number:  [String] -> (String -> Bool) -> [String]
-- converter tokens to numbers:   [String] -> [Integer]
-- sum all numbers to find total: [Integer] -> Integer
```
