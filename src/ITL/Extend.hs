
module ITL.Extend
    ( more
    , empty
    , gets
    , stable
    , halt
    , eventually
    , weakNext
    , weakUntil
    , precede
    ) where


import ITL.Core
import ITL.Interval

import Prelude hiding (not, until, head)


-- |
-- It is true iff the interval length is greater than zero.
more :: Predicate a
more = next (\_ -> True)


-- |
-- It means the negation of 'more' operator.
empty :: Predicate a
empty = not more


-- |
-- @e1 `gets` e2@ means that nexp e1 equals e2 in anywhere on the interval.
-- In other words, this shows the relationship of the state transfer between current to next.
-- 
-- >>> interval [1 .. 10] |= head `gets` (\i -> head i + 1)
-- True
-- >>> interval [0, 2 .. 20] |= head `gets` (\i -> head i + 2)
-- True
gets :: Eq b => Expression a b -> Expression a b -> Predicate a
gets e1 e2 = always $ more .--> (nexp e1 .== e2)


-- |
-- It means the value is constant on the interval.
-- 
-- >>> interval (replicate 10 1) |= stable head
-- True
stable :: Eq b => Expression a b -> Predicate a
stable e = e `gets` e


-- |
-- It means the predicate will be true in somewhere on the interval.
eventually :: Predicate a -> Predicate a
eventually p = not $ always (not p)


-- |
-- It means the predicate will be true only on the final state.
halt :: Predicate a -> Predicate a
halt p = always $ p .== empty


-- |
-- This applies the predicate to the next interval.
-- But if the length of the interval is zero, it returns TRUE, not false.
-- 
-- >>> interval [1] |= next (\_ -> False)
-- False
-- >>> interval [1] |= weakNext (\_ -> False)
-- True
weakNext :: Predicate a -> Predicate a
weakNext p = empty .|| next p


-- |
-- Almost same as 'until' function.
-- But @p1 `weakUntil` p2@ doesn't require that p2 is true on somewhere.
-- 
-- >>> interval [1 .. 10] |= (head .<= const 4) `weakUntil` (const 4 .< head)
-- True
-- >>> interval [1 .. 10] |= (head .<= const 11) `weakUntil` (const 11 .< head)
-- True
weakUntil :: Predicate a -> Predicate a -> Predicate a
weakUntil p1 p2 = p1 `until` p2 .|| always p1


-- |
-- @p1 `precede` p2@ means that p1 is true at least once before p2 is true.
precede :: Predicate a -> Predicate a -> Predicate a
precede p1 p2 = not $ (not p1) `until` p2

