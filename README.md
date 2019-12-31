# Haskell ITL Package

This is Interval Temporal Logic (ITL) package for haskell.
Please read [this paper](http://antonio-cau.co.uk/ITL/publications/reports/tempura-book.pdf) for about ITL.

## Overview

This package contains the following ITL operator:

- `next`
- `always`
- `until`
- etc.

Here is example for writing a predicate `○□(H > 2)` in ITL and applying it to an interval [1 .. 10].

```
import ITL
import Prelude hiding (head)

type State = Integer

context :: Interval State
context = interval [1 .. 10]

p1 :: Predicate State
p1 = next $ always $ head .>= const 2

main :: IO ()
main = print $ context |= p1

-- True
```

See `example/Main.hs` for more detail.


## Build

Use `stack` for building and running.
For running example, please type these commands on terminal:

```
$ stack build
$ stack exec itl-example
```
