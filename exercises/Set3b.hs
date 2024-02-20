{-# LANGUAGE NoImplicitPrelude #-}

module Set3b where

import Mooc.LimitedPrelude
import Mooc.Todo

------------------------------------------------------------------------------
-- Ex 1: given numbers start, count and end, build a list that starts
-- with count copies of start and ends with end.
-- Use recursion and the : operator to build the list.
buildList :: Int -> Int -> Int -> [Int]
buildList start count end = if count > 0 then start : buildList start (count - 1) end else [end]

------------------------------------------------------------------------------
-- Ex 2: given i, build the list of sums [1, 1+2, 1+2+3, .., 1+2+..+i]
-- Use recursion and the : operator to build the list.
sums :: Int -> [Int]
sums i = go 1
  where
    go n
      | n > i = []
      | otherwise = customSum 1 n : go (n + 1)

    -- Custom sum function to replace the standard library `sum`
    customSum start end
      | start > end = 0
      | otherwise = start + customSum (start + 1) end

------------------------------------------------------------------------------
-- Ex 3: define a function mylast that returns the last value of the
-- given list. For an empty list, a provided default value is
-- returned.
-- Use only pattern matching and recursion (and the list constructors : and [])
mylast :: a -> [a] -> a
mylast def [] = def
mylast _ (x:[]) = x
mylast def (_:xs) = mylast def xs

------------------------------------------------------------------------------
-- Ex 4: safe list indexing. Define a function indexDefault so that
--   indexDefault xs i def
-- gets the element at index i in the list xs. If i is not a valid
-- index, def is returned.
-- Use only pattern matching and recursion (and the list constructors : and [])
indexDefault :: [a] -> Int -> a -> a
indexDefault [] _ def = def
indexDefault (x:xs) i def
  | i == 0 = x
  | otherwise = indexDefault xs (i - 1) def

------------------------------------------------------------------------------
-- Ex 5: define a function that checks if the given list is in
-- increasing order.
-- Use pattern matching and recursion to iterate through the list.
sorted :: [Int] -> Bool
sorted [] = True
sorted [_] = True
sorted (x:y:xs) = x <= y && sorted (y:xs)

------------------------------------------------------------------------------
-- Ex 6: compute the partial sums of the given list like this:
--   sumsOf [a,b,c]  ==>  [a,a+b,a+b+c]
--   sumsOf [a,b]    ==>  [a,a+b]
--   sumsOf []       ==>  []
-- Use pattern matching and recursion (and the list constructors : and [])
sumsOf :: [Int] -> [Int]
sumsOf = go 0
  where
    go _ [] = []
    go acc (x:xs) = let sum = acc + x in sum : go sum xs

------------------------------------------------------------------------------
-- Ex 7: implement the function merge that merges two sorted lists of
-- Ints into a sorted list
-- Use only pattern matching and recursion (and the list constructors : and [])
merge :: [Int] -> [Int] -> [Int]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
  | x <= y = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys

------------------------------------------------------------------------------
-- Ex 8: compute the biggest element, using a comparison function
-- passed as an argument.
-- That is, implement the function mymaximum that takes
mymaximum :: (a -> a -> Bool) -> a -> [a] -> a
mymaximum _ initial [] = initial
mymaximum bigger initial (x:xs)
  | bigger x initial = mymaximum bigger x xs
  | otherwise = mymaximum bigger initial xs

------------------------------------------------------------------------------
-- Ex 9: define a version of map that takes a two-argument function
-- and two lists. Example:
--   map2 f [x,y,z,w] [a,b,c]  ==> [f x a, f y b, f z c]
-- If the lists have differing lengths, ignore the trailing elements
-- of the longer list.
-- Use recursion and pattern matching. Do not use any library functions.
map2 :: (a -> b -> c) -> [a] -> [b] -> [c]
map2 _ [] _ = []
map2 _ _ [] = []
map2 f (a:as) (b:bs) = f a b : map2 f as bs

------------------------------------------------------------------------------
-- Ex 10: implement the function maybeMap, which works a bit like a
-- combined map & filter.
-- maybeMap is given a list ([a]) and a function of type a -> Maybe b.
maybeMap :: (a -> Maybe b) -> [a] -> [b]
maybeMap _ [] = []
maybeMap f (x:xs) =
  case f x of
    Just y -> y : maybeMap f xs
    Nothing -> maybeMap f xs
