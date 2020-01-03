# Search algorithms and data structures in Haskell

Build and run the benchmark:

```
cabal clean
cabal configure
cabal build
cabal run benchmark
```
This will build an executable that calls whatever is defined in Main.hs

Build and run the test suite, in case you wouldn't want to use cabal test:

```
cabal clean
cabal configure --enable-tests
cabal build all_tests
cabal run all_tests
```