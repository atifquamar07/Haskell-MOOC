module Set3a where

import Data.Char
import Data.Either
import Data.List

-- Ex 1: implement the function maxBy that takes as argument a measuring function (of type a -> Int) and two values (of type a).
-- maxBy should apply the measuring function to both arguments and return the argument for which the measuring function returns a higher value.
maxBy :: (a -> Int) -> a -> a -> a
maxBy measure a b = if measure a > measure b then a else b

-- Ex 2: implement the function mapMaybe that takes a function and a Maybe value.
-- If the value is Nothing, it returns Nothing. If it is a Just, it updates the contained value using the function.
mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe f Nothing = Nothing
mapMaybe f (Just x) = Just (f x)

-- Ex 3: implement the function mapMaybe2 that works like mapMaybe except it combines two Maybe values using a function of two arguments.
mapMaybe2 :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
mapMaybe2 f (Just x) (Just y) = Just (f x y)
mapMaybe2 _ _ _ = Nothing

-- Ex 4: define the functions firstHalf and palindrome so that palindromeHalfs returns the first halfs of all palindromes in its input.
-- The first half of a string should include the middle character of the string if the string has an odd length.
firstHalf :: String -> String
firstHalf xs = take ((length xs + 1) `div` 2) xs

palindrome :: String -> Bool
palindrome xs = xs == reverse xs

palindromeHalfs :: [String] -> [String]
palindromeHalfs xs = map firstHalf (filter palindrome xs)

-- Ex 5: Implement a function capitalize that takes in a string and capitalizes the first letter of each word in it.
capitalizeFirst :: String -> String
capitalizeFirst "" = ""
capitalizeFirst (x:xs) = toUpper x : xs

capitalize :: String -> String
capitalize xs = unwords $ map capitalizeFirst $ words xs

-- Ex 6: powers k max should return all the powers of k that are less than or equal to max.
powers :: Int -> Int -> [Int]
powers k max = takeWhile (<= max) $ map (k^) [0..]

-- Ex 7: implement a functional while loop.
while :: (a -> Bool) -> (a -> a) -> a -> a
while check update value = if check value then while check update (update value) else value

-- Ex 8: another version of a while loop. This time, the check function returns an Either value.
whileRight :: (a -> Either b a) -> a -> b
whileRight check x = case check x of
                        Left b -> b
                        Right a -> whileRight check a

-- Ex 9: given a list of strings and a length, return all strings that have the given length and are made by concatenating two input strings.
joinToLength :: Int -> [String] -> [String]
joinToLength n xs = [x ++ y | x <- xs, y <- xs, length (x ++ y) == n]

-- Ex 10: implement the operator +|+ that returns a list with the first elements of its input lists.
(+|+) :: [a] -> [a] -> [a]
(x:_) +|+ (y:_) = [x, y]
[] +|+ (y:_) = [y]
(x:_) +|+ [] = [x]
[] +|+ [] = []

-- Ex 11: remember the lectureParticipants example? Implement the function sumRights which sums all non-missing measurements.
sumRights :: [Either a Int] -> Int
sumRights = sum . map (either (const 0) id)

-- Ex 12: recall the binary function composition operation (f . g) x = f (g x). Your task is to define a function that takes any number of functions given as a list and composes them.
multiCompose :: [b -> b] -> b -> b
multiCompose = foldr (.) id

-- Ex 13: Given some function f, a list of functions gs, and some value x, define a composition operation that applies each function g in gs to x and then f to the resulting list.
multiApp :: ([b] -> c) -> [a -> b] -> a -> c
multiApp f gs x = f $ map ($ x) gs

-- Ex 14: implement an interpreter for a simple language to keep track of x and y coordinates, interpreting commands like up, down, left, right, printX, and printY.
interpreter :: [String] -> [String]
interpreter commands = go 0 0 commands
  where
    go _ _ [] = []
    go x y ("up":cs) = go x (y+1) cs
    go x y ("down":cs) = go x (y-1) cs
    go x y ("left":cs) = go (x-1) y cs
    go x y ("right":cs) = go (x+1) y cs
    go x y ("printX":cs) = show x : go x y cs
    go x y ("printY":cs) = show y : go x y cs
