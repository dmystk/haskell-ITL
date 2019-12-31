module Main where

import Prelude hiding (not, head, tail, until, mod)
import ITL

main :: IO ()
main =
    let -- The interval is the finite sequence of the state, containing one at least.
        -- You can make any interval through the `interval` function.
        -- But please be careful that passing empty list is not allowed.
        context = interval [1 .. 10]

    in  do
        putStr "\n\n"
        putStr "For the interval :\n"
        print context

        -- The `head` means the current state, the type is `Integer` in this example.
        -- For example, `head (interval [1 .. 10])` returns 1.

        -- And you can use usual operators with predicates by adding (.) before the operator.
        -- So it allows to write like `head .+ const 1` and `head .== const 2`.
        -- Please notice that here uses `const` to define the constant value.

        -- To evaluate the predicate, (|=) operator is convenient.
        -- Writing like `interval [1 .. 10] |= <your-predicate>` makes the context clear.


        -- Here are some examples.
        -- Please refer for writing and enjoy the ITL world! :)

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

        -- Thank you for reading! :)

