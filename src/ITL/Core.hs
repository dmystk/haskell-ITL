
module ITL.Core
    ( Expression
    , Predicate
    , (|=)
    , next
    , always
    , until
    , nexp
    , not
    , (.&&), (.||), (.-->)
    , (.==), (./=), (.<), (.<=), (.>), (.>=)
    , (.~~), (./~), (.<<), (.<~), (.>>), (.>~)
    , (.+), (.-), (.*), (./), (.^), (.**), ITL.Core.mod
    ) where


import ITL.Interval as Interval

import Prelude hiding (head, tail, until, not)
import qualified Data.Bool as B


type Expression a b = Interval a -> b


type Predicate a = Expression a Bool


infixr 8 .^, .**
infixl 7 .*, ./, `mod`
infixr 6 .+, .-
infix  4 .==, ./=, .<, .<=, .>, .>=, .~~, ./~, .<<, .<~, .>>, .>~
infixr 3 .&&
infixr 2 .||
infixr 1 .-->
infixr 0 |=


-- |
-- Evaluate the predicate with the interval.
-- This operator is for convenient to make a predicate on an interval.
-- 
-- >>> interval [1 .. 10] |= head .== const 1
-- True
-- >>> interval [1 .. 10] |= next $ head .== const 1
-- False
(|=) :: Interval a -> Expression a b -> b
(|=) i e = e i


-- |
-- This applies the predicate to the next interval.
-- It returns false if the length of the interval is zero.
-- 
-- For the expression, please use @nexp@ function.
next :: Predicate a -> Predicate a
next p i = case safeTail i of
    Just i' -> p i'
    Nothing -> False


-- |
-- It means the predicate is satisfied in anywhere on the interval.
always :: Predicate a -> Predicate a
always p i = and . map p $ powerSet i


-- |
-- The strong until operator.
-- 
-- @p1 `until` p2@ means that p1 is true until p2 is true.
-- And more, p2 needs to be true at least once.
-- 
-- >>> interval [1 .. 10] |= (head .<= const 4) `until` (const 4 .< head)
-- True
-- >>> interval [1 .. 10] |= (head .<= const 11) `until` (const 11 .< head)
-- False
until :: Predicate a -> Predicate a -> Predicate a
until p1 p2 i
    | r1 && B.not r2 = (next $ until p1 p2) i
    | r2 = True
    | otherwise = False
    where
        r1 = p1 i
        r2 = p2 i


-- |
-- This applies the expression to the next interval.
-- It is undefined for the interval which length equals zero.
-- 
-- @nexp@ is shorthand for "the 'next' operator for the expression."
-- 
-- >>> interval [1 .. 10] |= head
-- 1
-- >>> interval [1 .. 10] |= nexp head
-- 2
-- >>> interval [1 .. 10] |= nexp $ nexp head
-- 3
nexp :: Expression a b -> Expression a b
nexp e i = e $ tail i


not :: Predicate a -> Predicate a
not p = B.not . p


combine :: (a -> b -> c)
    -> Expression d a -> Expression d b -> Expression d c
combine op e1 e2 i = op (e1 i) (e2 i)


(.&&) :: Predicate a -> Predicate a -> Predicate a
(.&&) = combine (&&)


(.||) :: Predicate a -> Predicate a -> Predicate a
(.||) = combine (||)


(.-->) :: Predicate a -> Predicate a -> Predicate a
(.-->) = combine (\x y -> (B.not x) || y)


(.==) :: Eq b => Expression a b -> Expression a b -> Predicate a
(.==) = combine (==)


(./=) :: Eq b => Expression a b -> Expression a b -> Predicate a
(./=) = combine (/=)


(.<) :: Ord b => Expression a b -> Expression a b -> Predicate a
(.<) = combine (<)


(.<=) :: Ord b => Expression a b -> Expression a b -> Predicate a
(.<=) = combine (<=)


(.>) :: Ord b => Expression a b -> Expression a b -> Predicate a
(.>) = combine (>)


(.>=) :: Ord b => Expression a b -> Expression a b -> Predicate a
(.>=) = combine (>=)


-- |
-- Temporal equality, means the values always equal on the interval.
-- 
-- >>> interval [(1, 1), (1, 1), (1, 1)] |= (fst . head) .~~ (snd . head)
-- True
-- >>> interval [(1, 1), (1, 2), (1, 1)] |= (fst . head) .~~ (snd . head)
-- False
(.~~) :: Eq b => Expression a b -> Expression a b -> Predicate a
(.~~) e1 e2 = always $ e1 .== e2


-- |
-- Negation of temporal equality.
(./~) :: Eq b => Expression a b -> Expression a b -> Predicate a
(./~) e1 e2 = not $ e1 .~~ e2


-- |
-- Always less than.
(.<<) :: Ord b => Expression a b -> Expression a b -> Predicate a
(.<<) e1 e2 = always $ e1 .< e2

-- |
-- Always less than or equal.
(.<~) :: Ord b => Expression a b -> Expression a b -> Predicate a
(.<~) e1 e2 = always $ e1 .<= e2


-- |
-- Always greater than.
(.>>) :: Ord b => Expression a b -> Expression a b -> Predicate a
(.>>) e1 e2 = always $ e1 .> e2


-- |
-- Always greater than or equal.
(.>~) :: Ord b => Expression a b -> Expression a b -> Predicate a
(.>~) e1 e2 = always $ e1 .>= e2


(.+) :: Num b => Expression a b -> Expression a b -> Expression a b
(.+) = combine (+)


(.-) :: Num b => Expression a b -> Expression a b -> Expression a b
(.-) = combine (-)


(.*) :: Num b => Expression a b -> Expression a b -> Expression a b
(.*) = combine (*)


(./) :: Fractional b => Expression a b -> Expression a b -> Expression a b
(./) = combine (/)


(.^) :: Integral b => Expression a b -> Expression a b -> Expression a b
(.^) = combine (^)


(.**) :: Floating b => Expression a b -> Expression a b -> Expression a b
(.**) = combine (**)


mod :: Integral b => Expression a b -> Expression a b -> Expression a b
mod = combine Prelude.mod

