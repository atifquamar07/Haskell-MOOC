module Set4b where

import Mooc.Todo

------------------------------------------------------------------------------
-- Ex 1: countNothings with a fold.
countNothings :: [Maybe a] -> Int
countNothings xs = foldr countHelper 0 xs

countHelper :: Maybe a -> Int -> Int
countHelper Nothing acc = acc + 1
countHelper _ acc = acc

------------------------------------------------------------------------------
-- Ex 2: myMaximum with a fold.
myMaximum :: [Int] -> Int
myMaximum [] = 0
myMaximum (x:xs) = foldr maxHelper x xs

maxHelper :: Int -> Int -> Int
maxHelper = max

------------------------------------------------------------------------------
-- Ex 3: compute the sum and length of a list with a fold.
sumAndLength :: [Double] -> (Double,Int)
sumAndLength xs = foldr slHelper slStart xs

slStart :: (Double, Int)
slStart = (0.0, 0)

slHelper :: Double -> (Double, Int) -> (Double, Int)
slHelper x (s, l) = (s + x, l + 1)

------------------------------------------------------------------------------
-- Ex 4: implement concat with a fold.
myConcat :: [[a]] -> [a]
myConcat xs = foldr concatHelper concatStart xs

concatStart :: [a]
concatStart = []

concatHelper :: [a] -> [a] -> [a]
concatHelper = (++)

------------------------------------------------------------------------------
-- Ex 5: get all occurrences of the largest number in a list with a fold.
largest :: [Int] -> [Int]
largest xs = foldr largestHelper [] xs

largestHelper :: Int -> [Int] -> [Int]
largestHelper x [] = [x]
largestHelper x acc@(a:_)
  | x == a = x : acc
  | x > a = [x]
  | otherwise = acc

------------------------------------------------------------------------------
-- Ex 6: get the first element of a list with a fold.
myHead :: [a] -> Maybe a
myHead xs = foldr headHelper Nothing xs

headHelper :: a -> Maybe a -> Maybe a
headHelper x _ = Just x

------------------------------------------------------------------------------
-- Ex 7: get the last element of a list with a fold.
myLast :: [a] -> Maybe a
myLast xs = foldr lastHelper Nothing xs

-- Adjusting lastHelper to fit foldr's requirements
lastHelper :: a -> Maybe a -> Maybe a
lastHelper x Nothing = Just x
lastHelper _ (Just y) = Just y

