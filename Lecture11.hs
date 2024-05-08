-- Exam review session

-- First part of the exam:
    -- Evaluating LC expressions (lectures 2-3 and hw0)
    -- programming in LC (lectures 3-5 and hw0)
    -- recursion in LC (lecture 5)

-- Second part:
    -- Understanding types of Haskell expressions, including types that contain type variables (lectures 5-10, HW 1-3)
    -- Pattern matching (lectures 5-10; HW 1-3)
    -- Tuples (lecture 7, hw 2-3)
    -- User-defined types (product types, sum types, recrusive types) (lectures 7-10, hw 2-3)
    -- tail recursion (lecture 8; hw3)
    -- The 'Maybe' type (lecture 8, hw3)
    -- Higher-order functions like 'map', 'foldr', etc (lecture 9, hw3)
    -- type classes: defining and using them; how to define and use custom type classes; type class constraints

-- Third part:
    -- abstract syntax trees and working with them (lectures 7-10, hw 2-3)
    -- environments and working with them (hw 3)


-- Let's deifne foldr!

foldr' :: (a -> b -> b) -> b -> [a] -> b -- takes three arguments (a -> b -> b), takes a base value (accumulator), 
                                         --takes a list, and returns an updated base value
foldr' f b [] = b
foldr' f b (x:xs) = f x (foldr' f b xs)

-- In general, foldr is great for taking a list of things, and boiling them down to one thing using some specified function

len :: [a] -> Int -- without foldr
len [] = 0
len (x:xs) = 1 + len xs

len' :: [a] -> Int -- with folr
len' x = foldr' (\x y -> 1 + y) 0 x

andOfList :: [Bool] -> Bool -- without foldr
andOfList [] = True -- this is a base case
andOfList (x:xs) = x && andOfList xs

andOfList' :: [Bool] -> Bool -- with foldr
andOfList' x = foldr' (&&) True x

sumList :: [Int] -> Int  -- without foldr
sumList []     = 0
sumList (x:xs) = x + sumList xs

sumList' :: [Int] -> Int -- using foldr
sumList' x = foldr' (+) 0 x

-- how did it work with foldr?
-- foldr' (+) 0 [1, 2, 3, 4]
-- (+) 1 (foldr' (+) 0 [2, 3, 4])
-- (+) 1 ((+) 2 (foldr' (+) 0 [3, 4]))
-- (+) 1 ((+) 2 ((+) 3 (foldr' (+) 0 [4])))
-- (+) 1 ((+) 2 ((+) 3 ((+) 4 (foldr' (+) 0 []))
-- (+) 1 ((+) 2 ((+) 3 ((+) 4 0)))
-- (1 + (2 + (3 + (4 + 0)))) -- more readable
-- 10

-- What is the type of the following Haskell expression?
-- map (\x y -> x) (foldr (++) [] [[True, False], [True]])

-- map :: (a -> b) -> [a] -> [b]
-- foldr :: (a -> b -> b) -> b -> [a] -> b

-- In the context of map and foldr, the foldr function is taking the b, which is a [], and the list of Bool lists,
-- and concatenating them into a new thing of b, which is a list. In this case, it would be a list of Bool aka [Bool].
-- (\x y -> x) can be written like \x -> (\y -> x), and since this type signature is mapped over a list of Bool, the type 
-- would be of [y -> Bool] where y can be anything since it wasn't specified
