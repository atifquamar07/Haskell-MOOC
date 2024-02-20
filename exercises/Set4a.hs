module Set4a where

import Mooc.Todo
import Data.List
import Data.Ord
import qualified Data.Map as Map
import Data.Array

------------------------------------------------------------------------------
-- Ex 1: implement the function allEqual which returns True if all
-- values in the list are equal.
allEqual :: Eq a => [a] -> Bool
allEqual [] = True
allEqual (x:xs) = all (== x) xs

------------------------------------------------------------------------------
-- Ex 2: implement the function distinct which returns True if all
-- values in a list are different.
distinct :: Eq a => [a] -> Bool
distinct xs = length xs == length (nub xs)

------------------------------------------------------------------------------
-- Ex 3: implement the function middle that returns the middle value
-- (not the smallest or the largest) out of its three arguments.
middle :: Ord a => a -> a -> a -> a
middle x y z = sort [x, y, z] !! 1

------------------------------------------------------------------------------
-- Ex 4: return the range of an input list, that is, the difference
-- between the smallest and the largest element.
rangeOf :: (Num a, Ord a) => [a] -> a
rangeOf xs = maximum xs - minimum xs

------------------------------------------------------------------------------
-- Ex 5: given a (non-empty) list of (non-empty) lists, return the longest
-- list. If there are multiple lists of the same length, return the list that
-- has the smallest _first element_.
longest :: Ord b => [[b]] -> [b]
longest = minimumBy (comparing (\x -> (negate $ length x, head x)))

------------------------------------------------------------------------------
-- Ex 6: Implement the function incrementKey, that takes a list of
-- (key,value) pairs, and adds 1 to all the values that have the given key.
incrementKey :: (Eq k, Num v) => k -> [(k, v)] -> [(k, v)]
incrementKey key = map (\(k,v) -> if k == key then (k, v+1) else (k, v))

------------------------------------------------------------------------------
-- Ex 7: compute the average of a list of values of the Fractional
-- class.
average :: Fractional a => [a] -> a
average xs = sum xs / fromIntegral (length xs)

------------------------------------------------------------------------------
-- Ex 8: given a map from player name to score and two players, return
-- the name of the player with more points. If the players are tied,
-- return the name of the first player.
winner :: Map.Map String Int -> String -> String -> String
winner scores player1 player2
  | Map.findWithDefault 0 player1 scores >= Map.findWithDefault 0 player2 scores = player1
  | otherwise = player2

------------------------------------------------------------------------------
-- Ex 9: compute how many times each value in the list occurs. Return
-- the frequencies as a Map from value to Int.
freqs :: (Eq a, Ord a) => [a] -> Map.Map a Int
freqs = foldr (\key -> Map.alter (Just . maybe 1 (+1)) key) Map.empty

------------------------------------------------------------------------------
-- Ex 10: recall the withdraw example from the course material. Write a
-- similar function, transfer, that transfers money from one account
-- to another.
transfer :: String -> String -> Int -> Map.Map String Int -> Map.Map String Int
transfer from to amount bank
  | amount < 0 || not (Map.member from bank && Map.member to bank) || fromBalance < amount = bank
  | otherwise = Map.adjust (+amount) to $ Map.adjust (subtract amount) from bank
  where fromBalance = Map.findWithDefault 0 from bank

------------------------------------------------------------------------------
-- Ex 11: given an Array and two indices, swap the elements in the indices.
swap :: Ix i => i -> i -> Array i a -> Array i a
swap i j arr = arr // [(i, arr ! j), (j, arr ! i)]

------------------------------------------------------------------------------
-- Ex 12: given an Array, find the index of the largest element. You
-- can assume the Array isn't empty.
maxIndex :: (Ix i, Ord a) => Array i a -> i
maxIndex arr = fst $ maximumBy (comparing snd) $ assocs arr
