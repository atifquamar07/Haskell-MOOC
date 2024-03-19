module Set5a where

import Mooc.Todo

------------------------------------------------------------------------------
-- Ex 1: Define the type Vehicle that has four constructors: Bike,
-- Bus, Tram and Train.
--
-- The constructors don't need any fields.
data Vehicle = Bike | Bus | Tram | Train

------------------------------------------------------------------------------
-- Ex 2: Define the type BusTicket that can represent values like these:
--  - SingleTicket
--  - MonthlyTicket "January"
--  - MonthlyTicket "December"
data BusTicket = SingleTicket | MonthlyTicket String

------------------------------------------------------------------------------
-- Ex 3: Here's the definition for a datatype ShoppingEntry that
-- represents an entry in a shopping basket. It has an item name (a
-- String), an item price (a Double) and a count (an Int). You'll also
-- find two examples of ShoppingEntry values.
--
-- Implement the functions totalPrice and buyOneMore below.
data ShoppingEntry = MkShoppingEntry String Double Int
  deriving Show

-- totalPrice should return the total price for an entry
totalPrice :: ShoppingEntry -> Double
totalPrice (MkShoppingEntry _ price count) = price * fromIntegral count

-- buyOneMore should increment the count in an entry by one
buyOneMore :: ShoppingEntry -> ShoppingEntry
buyOneMore (MkShoppingEntry name price count) = MkShoppingEntry name price (count + 1)

------------------------------------------------------------------------------
-- Ex 4: define a datatype Person, which should contain the age (an
-- Int) and the name (a String) of a person.
--
-- Also define a Person value fred, and the functions getAge, getName,
-- setAge and setName (see below).
data Person = Person Int String
-- fred is a person whose name is Fred and age is 90
fred :: Person
fred = Person 90 "Fred"

-- getName returns the name of the person
getName :: Person -> String
getName (Person _ name) = name

-- getAge returns the age of the person
getAge :: Person -> Int
getAge (Person age _) = age

-- setName takes a person and returns a new person with the name changed
setName :: String -> Person -> Person
setName newName (Person age _) = Person age newName

-- setAge does likewise for age
setAge :: Int -> Person -> Person
setAge newAge (Person _ name) = Person newAge name

------------------------------------------------------------------------------
-- Ex 5: define a datatype Position which contains two Int values, x
-- and y. Also define the functions below for operating on a Position.
data Position = Position Int Int

-- origin is a Position value with x and y set to 0
origin :: Position
origin = Position 0 0

-- getX returns the x of a Position
getX :: Position -> Int
getX (Position x _) = x

-- getY returns the y of a position
getY :: Position -> Int
getY (Position _ y) = y

-- up increases the y value of a position by one
up :: Position -> Position
up (Position x y) = Position x (y + 1)

-- right increases the x value of a position by one
right :: Position -> Position
right (Position x y) = Position (x + 1) y

------------------------------------------------------------------------------
-- Ex 6: Here's a datatype that represents a student. A student can
-- either be a freshman, a nth year student, or graduated.
data Student = Freshman | NthYear Int | Graduated
  deriving (Show,Eq)

-- Implement the function study, which changes a Freshman into a 1st
-- year student, a 1st year student into a 2nd year student, and so
-- on. A 7th year student gets changed to a graduated student. A
-- graduated student stays graduated even if he studies.
study :: Student -> Student
study Freshman = NthYear 1
study (NthYear year) = if year == 7 then Graduated else NthYear (year + 1)
study Graduated = Graduated

------------------------------------------------------------------------------
-- Ex 7: define a datatype UpDown that represents a counter that can
-- either be in increasing or decreasing mode. Also implement the
-- functions zero, toggle, tick and get below.
data UpDown = Increasing Int | Decreasing Int

-- zero is an increasing counter with value 0
zero :: UpDown
zero = Increasing 0

-- get returns the counter value
get :: UpDown -> Int
get (Increasing n) = n
get (Decreasing n) = n

-- tick increases an increasing counter by one or decreases a
-- decreasing counter by one
tick :: UpDown -> UpDown
tick (Increasing n) = Increasing (n + 1)
tick (Decreasing n) = Decreasing (n - 1)

-- toggle changes an increasing counter into a decreasing counter and
-- vice versa
toggle :: UpDown -> UpDown
toggle (Increasing n) = Decreasing n
toggle (Decreasing n) = Increasing n

------------------------------------------------------------------------------
-- Ex 8: you'll find a Color datatype below. It has the three basic
-- colours Red, Green and Blue, and two color transformations, Mix and
-- Invert.
--
-- Mix means the average of the two colors in each rgb channel.
--
-- Invert means subtracting all rgb values from 1.
--
-- Implement the function rgb :: Color -> [Double] that returns a list
-- of length three that represents the rgb value of the given color.
data Color = Red | Green | Blue | Mix Color Color | Invert Color
  deriving Show

rgb :: Color -> [Double]
rgb Red = [1,0,0]
rgb Green = [0,1,0]
rgb Blue = [0,0,1]
rgb (Mix c1 c2) = zipWith (\x y -> (x + y) / 2) (rgb c1) (rgb c2)
rgb (Invert c) = map (1-) (rgb c)

------------------------------------------------------------------------------
-- Ex 9: define a parameterized datatype OneOrTwo that contains one or
-- two values of the given type. The constructors should be called One and Two.
data OneOrTwo a = One a | Two a a

------------------------------------------------------------------------------
-- Ex 10: define a recursive datatype KeyVals for storing a set of
-- key-value pairs. There should be two constructors: Empty and Pair.
--
-- Empty represents an empty collection. It should have no fields.
--
-- Pair should have three fields, one for the key, one for the value,
-- and one for the rest of the collection (of type KeyVals)
--
-- The KeyVals datatype is parameterized by the key type k and
-- the value type v.
data KeyVals k v = Empty | Pair k v (KeyVals k v)

toList :: KeyVals k v -> [(k,v)]
toList Empty = []
toList (Pair k v rest) = (k, v) : toList rest

fromList :: [(k,v)] -> KeyVals k v
fromList [] = Empty
fromList ((k,v):rest) = Pair k v (fromList rest)

------------------------------------------------------------------------------
-- Ex 11: The data type Nat is the so called Peano
-- representation for natural numbers. Define functions fromNat and
-- toNat that convert natural numbers to Ints and vice versa.
data Nat = Zero | PlusOne Nat
  deriving (Show,Eq)

fromNat :: Nat -> Int
fromNat Zero = 0
fromNat (PlusOne n) = 1 + fromNat n

toNat :: Int -> Maybe Nat
toNat z | z < 0     = Nothing
        | z == 0    = Just Zero
        | otherwise = fmap PlusOne (toNat (z - 1))

------------------------------------------------------------------------------
-- Ex 12: While pleasingly simple in its definition, the Nat datatype is not
-- very efficient computationally. Instead of the unary Peano natural numbers,
-- computers use binary numbers.
data Bin = End | O Bin | I Bin
  deriving (Show,Eq)

prettyPrint :: Bin -> String
prettyPrint End = ""
prettyPrint (O b) = prettyPrint b ++ "0"
prettyPrint (I b) = prettyPrint b ++ "1"

fromBin :: Bin -> Int
fromBin End = 0
fromBin (O b) = 2 * fromBin b
fromBin (I b) = 2 * fromBin b + 1

toBin :: Int -> Bin
toBin 0 = O End  -- Adjusting base case as per test expectation
toBin n = toBinHelper n where
    toBinHelper 0 = End  -- Proper base case for recursion
    toBinHelper m | m `mod` 2 == 0 = O (toBinHelper (m `div` 2))
                  | otherwise = I (toBinHelper (m `div` 2))