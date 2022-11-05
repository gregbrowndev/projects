# Get Programming with Haskell

## Quick Start

### Stack

I've used [Stack](https://docs.haskellstack.org/en/stable/README/) to initialise a project space for
each unit in the book.

Install some packages (globally, i.e. not directly in the project) for formatting:

```shell
stack install hindent stylish-haskell
```

Create a project:

```shell
stack new unit1-foundations 
```

Useful commands:

* Setup - downloads an isolated compiler 

    ```
    stack setup
    ```

* Build - compiles the project

    ```
    stack build --test --haddock --no-haddock-hyperlink-source
    ```

* REPL - starts the GHCi REPL

    ```
    stack exec ghci
    ```
  
### IDE: IntelliJ

I installed the [IntelliJ-Haskell plugin](https://github.com/rikvdkleij/intellij-haskell/blob/master/README.md)

#### Formatting

The plugin provides a few formatting options, e.g.

* _Code_ > _Haskell_ > _Reformat Code by Ormolu_
* _Code_ > _Haskell_ > _Reformat Code by Stylish-Haskell_

However, these have separate shortcuts and IntelliJ's "Reformat Code" function doesn't seem
to do anything. This [GitHub issue](https://github.com/rikvdkleij/intellij-haskell/issues/284)
suggests the problem was solved but it still wasn't working for me.

To ensure my code has some basic auto-formatting, I installed Hindent and set up 
a File Watcher as described [here](https://github.com/mihaimaruseac/hindent#intellij--other-jetbrains-ides).