# Get Programming with Haskell

## Quick Start

### Stack

I've used [Stack](https://docs.haskellstack.org/en/stable/README/) to create this project, e.g.

```shell
stack new haskell-get-programming
```

Install some packages (globally, i.e. not directly in the project) for formatting:

```shell
stack install hindent stylish-haskell
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

I installed the [IntelliJ-Haskell plugin](https://github.com/rikvdkleij/intellij-haskell/blob/master/README.md).

Note: There is also the [HaskForce plugin](https://plugins.jetbrains.com/plugin/7602-haskforce).
However, you cannot have both install at the same time. Additionally, this plugin
crashed my IDE ([see issue](https://githubplus.com/carymrobbins/intellij-haskforce/issues/452))


#### Formatting

Run the format script:

```bash
./bin/format
```

The [IntelliJ-Haskell plugin](https://github.com/rikvdkleij/intellij-haskell/blob/master/README.md) provides a few formatting options, e.g.

* _Code_ > _Haskell_ > _Reformat Code by Ormolu_
* _Code_ > _Haskell_ > _Reformat Code by Stylish-Haskell_

However, these have separate shortcuts and IntelliJ's "Reformat Code" function doesn't seem
to do anything. This [GitHub issue](https://github.com/rikvdkleij/intellij-haskell/issues/284)
suggests the problem was solved but it still wasn't working for me.

To ensure my code has some basic auto-formatting, I installed Hindent and set up 
a File Watcher as described [here](https://github.com/mihaimaruseac/hindent#intellij--other-jetbrains-ides).