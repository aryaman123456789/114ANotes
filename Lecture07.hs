message :: String
message = "Welcome to lecture 7!"

-- Agenda:

-- Haskell features
-- - Tuples
-- - Product types, sum types, and recursive types

-- PL concepts
-- - More practice with recursion (really a *programming* concept)
-- - Tail recursion
-- - Abstract syntax trees and doing stuff with them

-- With tuples, the length is part of the type (unlike with lists!)

database :: [(String, Integer, String)]
database = [("Lindsey", 42, "bulbasaur"), ("Alex", 41, "mew"), ("Sylvia", 6, "houndoom")]

lookupAge :: [(String, Integer, String)] -> String -> Integer 
lookupAge []             _    = error "That person isn't in the database!"
lookupAge ((n,a,_):tups) name = if n == name then a else lookupAge tups name

bogusDatabase :: [(String, Integer, String)]
bogusDatabase = [("30450934354", 893290842309432, "Lindsey")]

-- It seems that built-in types don't capture all the info we want.
-- Let's switch to defining our own types!

-- A product type puts different things together (similar to a tuple)
-- Here "DBRecord" is a *constructor* for things of type
-- NameAgePokemonDatabaseRecord.
data NameAgePokemonDatabaseRecord = DBRecord HouseholdPerson Integer String

database' :: [NameAgePokemonDatabaseRecord]
database' = [DBRecord Lindsey 42 "bulbasaur",
             DBRecord Alex 41 "mew",
             DBRecord Sylvia 6 "houndoom"]

lookupAge' :: [NameAgePokemonDatabaseRecord] -> HouseholdPerson -> Integer 
lookupAge' []                    _    = error "That person isn't in the database!"
lookupAge' (DBRecord n a _:recs) name = if n == name then a else lookupAge' recs name

-- Sum types are for giving alternatives.
data HouseholdPerson = Lindsey | Alex | Sylvia
  deriving Eq

-- Let's talk about a data structure for arithmetic expressions.
-- 3 + 3, 5 - 2, 3 * 5, etc.
-- But we also want stuff like
-- 3 + (4 - 2)
-- ((2 + 8) - (10 * 6)) - 1

data SillyArithExpr = SillyPlus Int Int | SillyMinus Int Int | SillyTimes Int Int

threePlusThree :: SillyArithExpr
threePlusThree = SillyPlus 3 3

fiveMinusTwo :: SillyArithExpr
fiveMinusTwo = SillyMinus 5 2

threeTimesFive :: SillyArithExpr
threeTimesFive = SillyTimes 3 5

-- This won't scale to arithmetic expressions of arbitrary size.
-- So, recursive types to the rescue!
data ArithExpr = Plus ArithExpr ArithExpr 
               | Minus ArithExpr ArithExpr
               | Times ArithExpr ArithExpr
               | Leaf Int -- The base case
-- 3 + (4 - 2)
bigExpression :: ArithExpr
bigExpression = Plus (Leaf 3) (Minus (Leaf 4) (Leaf 2))

-- ((2 + 8) - (10 * 6)) - 1
biggerExpression :: ArithExpr
biggerExpression = Minus (Minus (Plus (Leaf 2) (Leaf 8)) 
                                (Times (Leaf 10) (Leaf 6))) (Leaf 1)

-- I was just a human parser.
-- I took "((2 + 8) - (10 * 6)) - 1", a string,
-- and I parsed it into an ArithExpr.
-- We've turned it into an Abstract Syntax Tree (AST).
-- And the ArithExpr type combines these concepts we just learned,
-- sum types, product types, and recursive types.


-- A type for binary trees with ints stored only at the leaves
data Tree = JustLeaf Int | NodeTree Tree Tree


-- Tail recursion

-- Recall this function:
-- Takes a number n and returns the sum of n + n-1 + n-2 + ... + 1 + 0.
ourSum :: Int -> Int
ourSum 0 = 0
ourSum n = n + ourSum (n - 1)

-- How does it work?
-- This is (roughly) a picture of what happens when we execute `ourSum 5`
-- ourSum 5
-- 5 + ourSum 4
-- 5 + (4 + ourSum 3)
-- 5 + (4 + (3 + ourSum 2))
-- 5 + (4 + (3 + (2 + ourSum 1)))
-- 5 + (4 + (3 + (2 + (1 + ourSum 0))))
-- 5 + (4 + (3 + (2 + (1 + 0))))
-- 5 + (4 + (3 + (2 + 1)))
-- 5 + (4 + (3 + 3))
-- 5 + (4 + 6)
-- 5 + 10
-- 15

-- HUGE CAVEAT: Haskell is what's called a "lazy" language.
-- This means that things don't actually always evaluate in the order you expect.
-- We'll talk more about this later.

-- Let's try rewriting ourSum in a way that uses the stack more sparingly.
-- We're going to use another argument and treat it as an *accumulator*.
ourSum' :: Int -> Int -> Int
ourSum' 0 acc = acc
ourSum' n acc = ourSum' (n-1) (acc+n)

-- ourSum' 5 0
-- ourSum' 4 5 -- we've accumulated 5 by now
-- ourSum' 3 9 -- we've accumulated 5 + 4 by now
-- ourSum' 2 12 -- we've accumulated 5 + 4 + 3 by now
-- ourSum' 1 14
-- ourSum' 0 15
-- 15

-- This is called *tail-recursive* code.
-- There's nothing around the outside of the recursive call
-- that's *waiting* for it to finish.
 





















