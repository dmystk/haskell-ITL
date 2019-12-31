module Main where

import Prelude hiding (not, head, tail, until, mod)
import ITL

main :: IO ()
main =
    let context = interval [1 .. 10]
    in  do
        putStr "\n\n"
        putStr "For the interval :\n"
        print context


        putStr "\n\n"
        putStr "These are true.\n\n"

        putStr "next (always (head >= 2))           : "
        print $ context |= next $ always $ head .>= const 2

        putStr "(head <= 3) `until` (head > 3)      : "
        print $ context |= (head .<= const 3) `until` (head .> const 3)

        putStr "always (more --> nexp head <= 10)   : "
        print $ context |= always $ more .--> nexp head .<= const 10

        putStr "always (empty --> stable head)      : "
        print $ context |= always $ empty .--> stable head

        putStr "eventually (head `gets` 10)         : "
        print $ context |= eventually $ head `gets` (const 10)

        putStr "halt (weakNext False)               : "
        print $ context |= halt $ weakNext $ const False

        putStr "head <= 10 `weakUntil` False        : "
        print $ context |= (head .<= const 10) `weakUntil` (const False)

        putStr "head == 5 `precede` head == 9       : "
        print $ context |= (head .== const 5) `precede` (head .== const 9)


        putStr "\n\n"
        putStr "These are false.\n\n"

        putStr "next (always (head >= 3))           : "
        print $ context |= next $ always $ head .>= const 3

        putStr "(head <= 100) `until` False         : "
        print $ context |= (head .<= const 100) `until` (const False)

        putStr "eventually (more && empty)          : "
        print $ context |= eventually $ more .&& empty

        putStr "stable head                         : "
        print $ context |= stable head

        putStr "head `gets` head + 2                : "
        print $ context |= head `gets` (head .+ const 2)

        putStr "halt (not (weakNext False))         : "
        print $ context |= halt $ not $ weakNext $ const False

        putStr "(head <= 3) `weakUntil` (head > 4)  : "
        print $ context |= (head .<= const 3) `weakUntil` (head .> const 4)

        putStr "head == 9 `precede` head == 5       : "
        print $ context |= (head .== const 9) `precede` (head .== const 5)


        putStr "\n\n"

