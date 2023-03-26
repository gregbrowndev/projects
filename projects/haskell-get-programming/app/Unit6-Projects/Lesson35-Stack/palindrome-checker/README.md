# Lesson 35: Building projects with Stack

## 35.1: Starting a new stack project

In this lesson, we'll use stack to create a new project to check our palindrome code.

Make sure stack is up-to-date:

    stack update

Create the project (in the app/Unit6-Projects/Lesson35-Stack directory):

     stack new palindrome-checker


## 35.2: Understanding the project structure

The most important file is _package.yaml_. Here we can fill in details about our 
project and where the main entrypoint and source/test directories are located.


## 35.3: Writing our code

We can copy the Main.hs and Palindrome.hs from the previous lesson and replace the 
files in _app/_ and _src/_.

## 35.4: Building and running the project

First, we'd need to ensure the correct version of GHC is installed:

    $ stack setup

To build the project:

    $ stack build 

This installs any third-party packages specified in _package.yaml_, e.g.

```yaml
dependencies:
- base >= 4.7 && < 5
- containers
- random
- split
- text
- bytestring
- filepath
- text-conversions
```

and it will regenerate the file _palindrome-checker.cabal_.

Once our project is built, we can run the executable:

```
$ stack exec palindrome-checker-exe
"Enter a word"
A man, a plan, a canal: Panama!
"A man, a plan, a canal: Panama! is a palindrome"
```

A useful tip: we can avoid needing to write language pragmas in every file by
including a `default-extensions` block in _package.yaml_:

```yaml
default-extensions:
  - OverloadedStrings
  - DataKinds
  - TypeOperators
```

## Summary

In this lesson, we've used the Stack build tool to create a new project for
our palindrome checker. We've organised the code in _app/_ and _src/_, and
used _package.yaml_ to define the details and dependencies of our project.