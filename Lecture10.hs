{-# LANGUAGE InstanceSigs #-} 
-- ^ Makes it possible to write type signatures inside the definition of type class instances

-- Type classes
    -- (+) :: Num a => a -> a -> a
    -- 3 + 4 (good)
    -- 3.4 + 5.7 (good)
    -- "pikachu" + "venusaur" (bad)
    -- True + True (bad)
    -- (\x -> x) + (\x -> x) (bad)

    -- Int, Double, etc all implement the Num type class

    -- One way to think of a type class : A SET OF OPERATIONS that you can do on
    -- values of any type that implements that type class

    -- For types that implement Num (e.g., Int, Double, etc.)
    -- you can do (+), (-), (*), (abs), etc
    -- For types that implement Eq, you can do (==) and not equal (/=)

    -- Another example:
    -- (==) ::
    -- 5 == 6 (yes)
    -- "pikachu" == "venusaur" (yes)
    -- True == True (yes)
    -- (\x -> x) == (\x -> y)

    -- Sum Types are for giving alternatives.
    data HouseholdPerson = Lindsey | Alex | Sylvia | Rainbow | Sprinkles
    -- (before deriving Eq: Lindsey == Lindsey -> ERROR)
        deriving (Eq, Show)
    -- after deriving Eq: Lindsey == Lindsey -> True

    data ArithExpr = Plus ArithExpr ArithExpr
                   | Minus ArithExpr ArithExpr
                   | Times ArithExpr ArithExpr
                   | Leaf Int 
        -- deriving Eq (since this technically doesn't work the way we want it, it is later deleted)
    -- Plus (Leaf 3) (Leaf 3) == Plus (Leaf 2) (Leaf 5) -> False
    -- Plus (Leaf 3) (Leaf 3) == Plus (Leaf 2) (Leaf 4) -> False
    -- Plus (Leaf 3) (Leaf 3) == Plus (Leaf 3) (Leaf 3) -> True
    -- Leaf 2 == Leaf 2 -> True

    eval :: ArithExpr -> Int
    eval (Leaf n) = n
    eval (Plus n m) = eval n + eval m
    eval (Minus n m) = eval n - eval m
    eval (Times n m) = eval n * eval m

    -- instance is a Haskell keyword:
    -- instance Eq ArithExpr means I'm defining an instance of the 'Eq' type class for type 'ArithExpr'
    instance Eq ArithExpr where
        (==) :: ArithExpr -> ArithExpr -> Bool
        (==) n m = eval n == eval m

    -- Plus (Leaf 3) (Leaf 3) == Plus (Leaf 2) (Leaf 4) -> True (Now this works!)
    -- Plus (Leaf 3) (Leaf 3) == Plus (Leaf 2) (Leaf 5) -> False

    instance Show ArithExpr where
        show :: ArithExpr -> String
        show (Leaf n) = show n
        show (Plus n1 n2) = "(" ++ show n1 ++ " + " ++ show n2 ++ ")"
        -- Plus (Leaf 3) (Leaf 4) -> (3 + 4)
        -- Plus (Plus (Leaf 2) (Leaf 3)) (Leaf 4) -> ((2 + 3) + 4)
        show (Minus n1 n2) = "(" ++ show n1 ++ " - " ++ show n2 ++ ")"
        show (Times n1 n2) = "(" ++ show n1 ++ " * " ++ show n2 ++ ")"

        -- show (Plus n1 n2) = "(" ++ show (eval n1) ++ " + " ++ show (eval n2) ++ ")"
        -- Plus (Plus (Leaf 2) (Leaf 3)) (Leaf 4) -> (5 + 4)

    instance Ord HouseholdPerson where
        (<=) :: HouseholdPerson -> HouseholdPerson -> Bool
        (<=) p1 p2 = show p1 <= show p2

-- QUIZ: According to GHCi, what is the type of the following Haskell expression?
--      \x y -> (x + y) == 3
-- Answer: (Eq a, Num a) => a -> a -> Bool
    -- Explanation:
    -- Since `\x y -> (x + y) == 3` is a function of two arguments, we know the type will be something of the
    -- shape `... -> ... -> ...`. Because we're using `(+)` to add the two arguments, we need the typeclass constraint 
    -- `Num` on them, since the type of `(+)` is ` Num a => a -> a -> a`, so we know that the 
    -- arguments `x` and `y` are things that can be added. So they'll be both of type `a`, with the constraint 
    -- that `a` must be an instance of `Num`. In the body of the function, `(+)` will return something of type `a`, and we're 
    -- then using `(==)` to compare that value to 3. So, since the type of `(==)` is `Eq a => a -> a -> Bool`, we know 
    -- that we need the typeclass constraint `Eq` on `a`, because the result of adding `x` and `y` has to be 
    -- something that can be compared with another value, in this case 3. Finally, the return 
    -- type of the whole expression must be `Bool`, since we know that `(==)` returns something of type `Bool`.

