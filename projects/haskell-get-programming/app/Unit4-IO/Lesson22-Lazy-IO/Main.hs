{- Lesson 22: Interacting with the command line and lazy I/O

After this lesson, I'll be able to:

  - Access command-line arguments
  - Use the traditional approach to interacting through I/O
  - Write I/O code using lazy evaluation to make I/O easier

Some people may assume that IO is more difficult in Haskell since the language
is geared towards pure FP. However, Haskell has a unique approach to IO that is
less clunky than how we deal with IO in other languages.

In most other languages, we talk about IO Streams: a lazily evaluated list of
characters that is streamed from STDIN into the program. This stream eventually
reaches an end, but in theory the stream may never end. This is exactly how we
think about lists in Haskell when using lazy evaluation.

In this lesson, we'll look at a simple program and solve it in a few ways. The
program reads an arbitrary long list of numbers entered by a user, add them up,
and return the result to the user. We'll learn how to write traditional I/O as
well as how to use lazy evaluation to take an approach that makes the solution
much easier to reason about.

Let's write a program sum.hs...
-}


{- Summary

In this lesson, we saw how to write simple command line applications using the
familiar approach to I/O used in most programming languages. We used do-notation
to create a procedure of IO actions. The key takeaway is that our program wanted
to interact with the user multiple times to receive inputs to sum up. This
resulted in code that was closely coupled to the IO code.

A more interesting approach, that is only possible in few languages other than
Haskell, is to use lazy evaluation to treat the IO stream as a lazily evaluated
list of characters. This radically simplifies the code by writing pure functions
that work with just the [Char] type.
-}

{-
Questions
  Q22.1: Write a program, simple_calc.hs, that reads simple equations involving
  adding two numbers or multiplying two numbers. The program should solve the
  equation each user types into each line as each line is entered.

  Q22.2: Write a program that allows a user to select a number between 1 and 5
  and then prints a famous quote. After printing the quote, the program asks
  the user if they would like another.
-}
