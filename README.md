# Advent of Code 2019

My answers for [Advent of Code 2019](https://adventofcode.com/2019/).

## Running solutions

There are three different ways you can run a solution. They all involve changing
to the directory of the day. I prefer `runhaskell`, but you can use `ghci`, or
`ghc`.

```shell
$ cd Day01
$ runhaskell Day01.hs
Part 1 solution: 3268951
Part 2 solution: 4900568
$ ghc Day01.hs && ./Day01
[1 of 1] Compiling Main             ( Day01.hs, Day01.o )
Linking Day01 ...
Part 1 solution: 3268951
Part 2 solution: 4900568
$ ghci
GHCi, version 8.10.7: https://www.haskell.org/ghc/  :? for help
Prelude> :l Day01.hs
[1 of 1] Compiling Main             ( Day01.hs, interpreted )
Ok, one module loaded.
*Main> recursiveFuelRequired 1969
966
*Main> main
Part 1 solution: 3268951
Part 2 solution: 4900568
```