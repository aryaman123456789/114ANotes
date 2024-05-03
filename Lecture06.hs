message :: String
message = "Welcome to lecture 6!"

-- Plan: Give you all the tools you need to do HW1

-- agenda:

-- - Lists!
--   - constructing lists (the list constructors in Haskell)
--   - pattern matching on lists
--   - strings are also lists!
--   - appending lists with (++)
--   - polymorphic list types
--   - writing list literals
-- - Guards in function definitions
-- - Using `div` and `mod`
-- - Using infix functions

-- Every list is either
--  - an empty list, `[]`
--  - a head *element*, attached to a tail *list*.
--    (The tail of a list is just the list with the first element removed.)

-- This tells us something about how to write functions that operate
-- on lists!

-- We're always going to have to deal with:
-- - the empty list case
-- - the case where the list has a head and a tail

-- Every list is *constructed* out of either:
-- - the empty list constructor, `[]`
-- - the `:` constructor, pronounced "cons", which puts a head and a tail
--   together.

-- I recommend using pattern matching instead of
-- the `head` and `tail` functions.
-- It's safer *and* better style.
ourLength :: [a] -> Int -- `a` is a type variable
ourLength []     = 0
ourLength (_:xs) = 1 + ourLength xs

ourHead :: [a] -> a
ourHead []    = error "You can't do that!!"
ourHead (x:_) = x

appendHelloToAll :: [String] -> [String]
appendHelloToAll []      = []
appendHelloToAll (hd:tl) = (hd ++ "hello") : appendHelloToAll tl

-- Strings are actually just lists of Char.
-- Let's try treating Strings as lists!
removeFirstChar :: String -> String
removeFirstChar []     = error "That's an empty string, you fool!"
removeFirstChar (_:xs) = xs
-- (That was actually just the `tail` function, specialized to the
-- String type.)

f :: [String] -> [String]
f []     = []
f (_:xs) = ["pikachu"] ++ xs
-- That's actually bad style.  Do this instead:

-- Instead of appending a one-element list to another list,
-- just use `:` to put the one element together with the list.
f' :: [String] -> [String]
f' []     = []
f' (_:xs) = "pikachu" : xs

-- Let's try to write a function that
-- appends "hello" to only the *second* element
-- of a list of Strings.
-- If the list has fewer than 2 elements,
-- return it unchanged.
-- > appendHelloToSecondElement ["apple", "orange", "banana"]
-- ["apple", "orangehello", "banana"]
appendHelloToSecondElement :: [String] -> [String]
appendHelloToSecondElement []       = []
appendHelloToSecondElement [x]      = [x]
appendHelloToSecondElement (x:y:ys) = x : (y ++ "hello") : ys

-- Here's an even more concise way to do it.
appendHelloToSecondElement' :: [String] -> [String]
appendHelloToSecondElement' (x:y:ys) = x : (y ++ "hello") : ys
appendHelloToSecondElement' xs       = xs

-- Let's talk about *guards*.
lessThanFive :: Int -> String
lessThanFive n | n <  5 = "Hooray!"
lessThanFive n | n >= 5 = "Boo!"
-- The `n <  5` and `n >= 5` parts here are *guards*.
-- The pattern only matches if the guard evaluates to True.

compareNumbers :: Int -> Int -> String
compareNumbers n m | n == m  = "Hooray!"
compareNumbers n m | n /= m  = "Boo!"

-- "otherwise" is a handy way to write a catch-all case
-- for everything that hasn't been matched yet.
compareNumbers' :: Int -> Int -> String
compareNumbers' n m | n == m    = "Hooray!" 
                    | otherwise = "Boo!"
