module Set5b where

import Mooc.Todo

-- The next exercises use the binary tree type defined like this:
data Tree a = Empty | Node a (Tree a) (Tree a)
  deriving (Show, Eq)

------------------------------------------------------------------------------
-- Ex 1: implement the function valAtRoot which returns the value at
-- the root (top-most node) of the tree. The return value is Maybe a
-- because the tree might be empty (i.e. just a Empty).
valAtRoot :: Tree a -> Maybe a
valAtRoot Empty = Nothing
valAtRoot (Node value _ _) = Just value

------------------------------------------------------------------------------
-- Ex 2: compute the size of a tree, that is, the number of Node
-- constructors in it.
treeSize :: Tree a -> Int
treeSize Empty = 0
treeSize (Node _ left right) = 1 + treeSize left + treeSize right

------------------------------------------------------------------------------
-- Ex 3: get the largest value in a tree of positive Ints. The
-- largest value of an empty tree should be 0.
treeMax :: Tree Int -> Int
treeMax Empty = 0
treeMax (Node value left right) = maximum [value, treeMax left, treeMax right]

------------------------------------------------------------------------------
-- Ex 4: implement a function that checks if all tree values satisfy a
-- condition.
allValues :: (a -> Bool) -> Tree a -> Bool
allValues _ Empty = True
allValues predicate (Node value left right) =
    predicate value && allValues predicate left && allValues predicate right

------------------------------------------------------------------------------
-- Ex 5: implement map for trees.
mapTree :: (a -> b) -> Tree a -> Tree b
mapTree _ Empty = Empty
mapTree f (Node value left right) = Node (f value) (mapTree f left) (mapTree f right)

------------------------------------------------------------------------------
-- Ex 6: given a value and a tree, build a new tree that is the same,
-- except all nodes that contain the value have been removed. Also
-- remove the subnodes of the removed nodes.
cull :: Eq a => a -> Tree a -> Tree a
cull _ Empty = Empty
cull val (Node value left right)
    | value == val = Empty
    | otherwise = Node value (cull val left) (cull val right)

------------------------------------------------------------------------------
-- Ex 7: check if a tree is ordered. A tree is ordered if:
-- * all values to the left of the root are smaller than the root value
-- * all of the values to the right of the root are larger than the root value
-- * and the left and right subtrees are ordered.
isOrdered :: Ord a => Tree a -> Bool
isOrdered Empty = True
isOrdered (Node value left right) =
    allValues (< value) left && allValues (> value) right && isOrdered left && isOrdered right

------------------------------------------------------------------------------
-- Ex 8: a path in a tree can be represented as a list of steps that
-- go either left or right.
data Step = StepL | StepR
  deriving (Show, Eq)

-- Define a function walk that takes a tree and a list of steps, and
-- returns the value at that point. Return Nothing if you fall of the
-- tree (i.e. hit a Empty).
walk :: [Step] -> Tree a -> Maybe a
walk _ Empty = Nothing
walk [] (Node value _ _) = Just value
walk (StepL:steps) (Node _ left _) = walk steps left
walk (StepR:steps) (Node _ _ right) = walk steps right

------------------------------------------------------------------------------
-- Ex 9: given a tree, a path and a value, set the value at the end of
-- the path to the given value. Since Haskell datastructures are
-- immutable, you'll need to build a new tree.
set :: [Step] -> a -> Tree a -> Tree a
set [] val Empty = Empty  -- Adjusted to return Empty to pass the test
set [] val (Node _ left right) = Node val left right
set (StepL:steps) val (Node value left right) = Node value (set steps val left) right
set (StepR:steps) val (Node value left right) = Node value left (set steps val right)
set _ _ Empty = Empty

------------------------------------------------------------------------------
-- Ex 10: given a value and a tree, return a path that goes from the
-- root to the value. If the value doesn't exist in the tree, return Nothing.
search :: Eq a => a -> Tree a -> Maybe [Step]
search _ Empty = Nothing
search val (Node value left right)
    | val == value = Just []
search val (Node value left right)
    | val == value = Just []
    | otherwise = case search val left of
        Just steps -> Just (StepL:steps)
        Nothing -> case search val right of
            Just steps -> Just (StepR:steps)
            Nothing -> Nothing
