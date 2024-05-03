module Lecture09 where
import Prelude hiding (filter, map, length, sum, concat, foldr)


data ArithExpr = Plus ArithExpr ArithExpr
               | Times ArithExpr ArithExpr
               | Leaf Int -- Base Case
    deriving Show

eval :: ArithExpr -> Int
eval (Leaf n) = n
eval (Plus n m) = eval n + eval m
-- eval (Minus n m) = eval n - eval m
eval (Times n m) = eval n * eval m

-- Motivation for higher-order functions:
-- a lot of code we write is "repetitive"

-- Takes a list of 'Int's and filters out the odd-numbered ones
evensOnly :: [Int] -> [Int]
evensOnly [] = []
evensOnly (x:xs) = if x `mod` 2 == 0 then x : evensOnly xs
                                     else evensOnly xs

-- Take a list of strings and filter out all but the four-letter strings
fourLettersOnly :: [String] -> [String]
fourLettersOnly [] = []
fourLettersOnly (x:xs) = if length x == 4 then x : fourLettersOnly xs
                                          else fourLettersOnly xs

-- Both functions above (evensOnly and fourLettersOnly) are very similar
-- What if we wrote one function that expresses this "pattern"?
-- In that case, 'higher-order functions' are our friend
-- Definition of Higher-order functions:
    -- It is a function that takes a function as an argument, or returns a function (or BOTH!)

-- The 'filter' pattern:
-- filter takes a function f and a list, and applies 
-- f to each element in the list. If the result is TRUE, then the element is included in the resulting list.
-- If FALSE, then it is left out

-- filter (\n -> n 'mod' 2 == 0) [1, 2, 3, 4] OR filter even [1, 2, 3, 4]
-- [2,4]

-- filter (\s -> length s == 4) ["apple", "banana", "pear"]
-- ["pear"]

-- filter (\n -> false) [1, 2, 3, 4]
-- []
-- filter (\n -> true) [1, 2, 3, 4]
-- [1, 2, 3, 4]
filter :: (a -> Bool) -> [a] -> [a]
filter f [] = []
filter f (x:xs) | f x = x : filter f xs
                | otherwise = filter f xs

-- Another example: Take a list of Ints and square all of them
squares :: [Int] -> [Int]
squares [] = []
squares (x:xs) = x*x : squares xs

-- Take a list of Ints and produce a list of Bools that
-- tell us whether the corresponding Int is divisible by 3
divisibleByThree :: [Int] -> [Bool]
divisibleByThree [] = []
divisibleByThree (x:xs) = (x `mod` 3 == 0) : divisibleByThree xs

-- We can generalize this pattern with the 'map' function
-- 'map' takes a function and a list, and applies the function to every element
-- in the list, resulting in a new list

-- map (\f -> f "apple") [length]
-- [5]
-- map (\n -> n*n) [1,2,3]
-- [1,4,9]

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs

-- QUIZ: What does the following Haskell expression evaluate to?
-- map (\x y z -> x) ["apple", "kumquat", "pear"]
-- [(\y z -> "apple"), (\y z -> "kumquat"), (\y z -> "pear")]

-- So far, only talked about HOF that take and return lists 
-- But what about boiling a list down to one value
-- Take a list, and return its length
length :: [a] -> Int
length [] = 0
length (x:xs) = 1 + length xs

-- Take a list and return its sum
sum :: [Int] -> Int
sum [] = 0
sum (x:xs) = x + sum xs

-- Take strings and concatenate
concat :: [String] -> String
concat [] = ""
concat (x:xs) = x ++ concat xs

-- The pattern here is called "fold" AKA "reduce"
-- foldr (+) 0 [1,2,3]
-- 6
-- foldr (++) "" ["apple", "banana", "pear"]
foldr f b [] = b
foldr f b (x:xs) = f x (foldr f b xs)
