
module ITL.Interval where


import Prelude hiding (head, tail)
import Data.List as L


data Interval a = Interval a [a]


instance Show a => Show (Interval a) where
    show (Interval x xs) = show (x : xs)


-- |
-- Constructor for interval type.
-- An interval needs one state at least, i.e. it is undefined for the empty list.
interval :: [a] -> Interval a
interval [] = undefined
interval (x:xs) = Interval x xs


-- |
-- Returns current state.
head :: Interval a -> a
head (Interval x _) = x


-- |
-- @tail@ is undefined when the length of the interval equals zero.
tail :: Interval a -> Interval a
tail (Interval x xs) = interval xs


safeTail :: Interval a -> Maybe (Interval a)
safeTail (Interval x []) = Nothing
safeTail (Interval x xs) = Just $ interval xs


-- |
-- This returns all intervals which are included by the passed interval.
-- If the argument is [s1, s2, s3], the result is [[s1, s2, s3], [s2, s3], [s3]].
powerSet :: Interval a -> [Interval a]
powerSet i@(Interval x []) = [i]
powerSet i@(Interval x xs) = (:) i $ powerSet $ interval xs


-- |
-- This returns the length of the interval, equals the number of states minus 1.
length :: Interval a -> Int
length (Interval _ xs) = L.length xs

