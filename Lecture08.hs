message :: String
message = "Welcome to lecture 8!"

-- A product type puts different things together (similar to a tuple)
-- Here "DBRecord" is a *constructor* for things of type
-- NameAgePokemonDatabaseRecord.
data NameAgePokemonDatabaseRecord = DBRecord HouseholdPerson Integer (Maybe String)
  deriving Show

-- Sum types are for giving alternatives.
data HouseholdPerson = Lindsey | Alex | Sylvia | Rainbow | Sprinkles
  deriving (Show, Eq)

database' :: [NameAgePokemonDatabaseRecord]
database' = [DBRecord Lindsey 42 (Just "bulbasaur"),
             DBRecord Alex 41 (Just "mew"),
             DBRecord Sylvia 6 (Just "houndoom"),
             DBRecord Rainbow 2 Nothing,
             DBRecord Sprinkles 3 Nothing]

lookupAge' :: [NameAgePokemonDatabaseRecord] -> HouseholdPerson -> Integer 
lookupAge' []                    _    = error "That person isn't in the database!"
lookupAge' (DBRecord n a _:recs) name = if n == name then a else lookupAge' recs name

lookupPokemon :: [NameAgePokemonDatabaseRecord] -> HouseholdPerson -> Maybe String
lookupPokemon [] _ = Nothing
lookupPokemon (DBRecord n _ p:recs) name = if n == name then p else lookupPokemon recs name  

-- A type for binary trees with ints stored at the leaves and at the internal nodes
data Tree = JustLeaf Int
          | NodeTree Int Tree Tree
          | NoData
  deriving Show

exampleTree :: Tree
exampleTree = NodeTree 7 (NodeTree 5 (JustLeaf 1) (JustLeaf 2))
                         (NodeTree 6 (JustLeaf 3) (JustLeaf 4))

-- Practice with writing code that operates on recursively defined data.
-- Let's write some tree traversal code!
preorderTraversal :: Tree -> [Int]
preorderTraversal NoData                  = []
preorderTraversal (JustLeaf n)            = [n]
preorderTraversal (NodeTree n left right) = [n] ++ preorderTraversal left ++ preorderTraversal right

inorderTraversal :: Tree -> [Int]
inorderTraversal NoData                  = []
inorderTraversal (JustLeaf n)            = [n]
inorderTraversal (NodeTree n left right) = inorderTraversal left ++ [n] ++ inorderTraversal right

-- Let's introduce the very useful `Maybe` type.

-- This is built in!
-- data Maybe a = Just a | Nothing

maybeNumber :: Maybe Int
maybeNumber = Just 5

-- Let's talk about tail recursion some more!

-- Here's a classic recursive function:

-- `fact n` should give us the factorial of n,
-- e.g. `fact 5` should give us 120
-- factorial of 5 = 5 * 4 * 3 * 2 * 1
fact :: Int -> Int
fact n | n <= 1    = 1
fact n | otherwise = n * fact (n - 1)

-- How does this behave operationally?
-- fact 5
-- 5 * fact 4
-- 5 * 4 * fact 3
-- 5 * 4 * 3 * fact 2
-- 5 * 4 * 3 * 2 * fact 1
-- 5 * 4 * 3 * 2 * 1
-- 120

factTR :: Int -> Int -> Int
factTR n acc | n <= 1    = acc
factTR n acc | otherwise = factTR (n - 1) (acc * n)

-- How does this behave?
-- factTR 5 1
-- factTR 4 5   -- (accumulated 5)
-- factTR 3 20  -- (accumulated 5 * 4)
-- factTR 2 60  -- (accumulated 5 * 4 * 3)
-- factTR 1 120 -- (accumulated 5 * 4 * 3 * 2)
-- 120

-- What can we do about the fact that factTR requires two arguments?
-- Just write a helper function,
-- and keep the top-level interface the same!
fact' :: Int -> Int
fact' n = helper n 1
    where helper :: Int -> Int -> Int
          helper n acc | n <= 1    = acc
          helper n acc | otherwise = helper (n - 1) (acc * n)

-- Back to our toy language of arithmetic expressions
data ArithExpr = Plus ArithExpr ArithExpr 
               | Minus ArithExpr ArithExpr
               | Times ArithExpr ArithExpr
               | Leaf Int -- The base case
  deriving Show

-- 3 + (4 - 2)
bigExpression :: ArithExpr
bigExpression = Plus (Leaf 3) (Minus (Leaf 4) (Leaf 2))

-- ((2 + 8) - (10 * 6)) - 1
biggerExpression :: ArithExpr
biggerExpression = Minus (Minus (Plus (Leaf 2) (Leaf 8)) 
                                (Times (Leaf 10) (Leaf 6))) (Leaf 1)

eval :: ArithExpr -> Int
eval (Leaf n) = n
eval (Plus n m) = eval n + eval m
eval (Minus n m) = eval n - eval m
eval (Times n m) = eval n * eval m
