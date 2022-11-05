# unit1-foundations

## Lessons

### Hello World

The first Haskell program we created is a simple Hello World app.

We can run the progam in the REPL:

```shell
$ stack exec ghci

GHCi> :l app/HelloWorld.hs
GHCi> main
"Hello World!"
```

### First Prog

The next program we wrote was to demonstrate how to refactor a messy Haskell main function into smaller, individual
functions. This much improves our ability to debug the code, as we can execute each function the REPL.

```shell
$ stack exec ghci

GHCi> :l app/FirstProg.hs
GHCi> createEmail "Greg" "Learn Haskell" "Will Kurt"
"Dear Greg,\nThanks for buying Learn Haskell.\nThanks, \nWill Kurt"
```