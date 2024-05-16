-- What will Nano have?
    -- - Arithmetic expressions (we've seen this)
    -- - Variables (we've seen this)
    -- - let-expressions (today)
    -- - functions (also recursive functions) (new)

    -- Back to our little arithmetic interpreter (lecture 7)
    -- Our AST for our little language
    data Expr = Plus Expr Expr 
                   | Minus Expr Expr
                   | Times Expr Expr
                   | ELet String Expr Expr
                   | ENum Int -- The base case
                   | EVar String
                deriving Show

    -- let x = 5 in x + 4
    tinyLetExpression :: Expr
    tinyLetExpression = ELet "x" (ENum 5) (Plus (EVar "x") (ENum 4))

    -- let x = 5 in (let y = 4 in x + y)
    biggerLetExpression :: Expr
    biggerLetExpression = ELet "x" (ENum 5) 
                                    (ELet "y" (ENum 4) (Plus (EVar "x") (EVar "y")))

    -- 3 + (4 - 2)
    bigExpression :: Expr
    bigExpression = Plus (ENum 3) (Minus (ENum 4) (ENum 2))

    -- ((2 + 8) - (10 * 6)) - 1
    biggerExpression :: Expr
    biggerExpression = Minus (Minus (Plus (ENum 2) (ENum 8)) 
                                    (Times (ENum 10) (ENum 6))) (ENum 1)

    -- What is an "environment"?
        -- It is a way to associate variables with something (ex: types, values, etc)
        -- We will be associating variables with values
    type Env = [(String, Int)]

    exampleEnv :: [(String, Int)]
    exampleEnv = [("x", 3), ("y", 4)]

    -- And our interpreter from lecture 8: (but now with support for let expressions)
    eval :: Expr -> Env -> Int
    eval (ENum n) env = n
    eval (EVar s) env = lookupInEnv env s
    eval (Plus n m) env = eval n env + eval m env
    eval (Minus n m) env = eval n env - eval m env
    eval (Times n m) env = eval n env * eval m env
    -- We want the value of a let-expression to be 
    -- whatever the value of its body is,
    -- but taking into account the new binding that the let-expression created
    -- Note: Don't forget that the new expression bound in the binding part
    -- of the let-expression needs to be evaluated too! "(eval expr env)""
    eval (ELet s expr bodyExpr) env = eval bodyExpr (extendEnv s (eval expr env) env)

    lookupInEnv :: Env -> String -> Int
    lookupInEnv [] s = error ("Sorry, the variable " ++ s ++ "is unbound!")
    lookupInEnv ((id, val):xs) s = if s == id then val else lookupInEnv xs s

    extendEnv :: String -> Int -> Env -> Env
    extendEnv s n env =  (s,n):env

-- Something new
    -- What about programs that actually bind new variables within the program?
    -- This leads us to let-expressions

    exampleLetExpr :: Integer
    exampleLetExpr = let x = 5 -- "x = 5" is the *binding*
                        in x + 3 -- "x + 3" is the *body* of the let-expression
    -- The value of a let-expression is whatever the value of its body is when the
    -- binding has been included in the environment

    -- And the type of a let-expression is whatever the type of 
    -- its body is when the binding is TAKEN into account

    -- QUIZ: 
    -- What should the following let-expression evaluate to?
        --quiz = let x = 5 in
            -- (let y = x + z in -- Variable not in scope: z
            -- (let z = 10 in y))

            --NOTE: In a language with dynamic scope (ex: Emacs Lisp), this would evaluate to 15!

    quizExpr :: Expr
    quizExpr = ELet "x" (ENum 5) (ELet "y" (Plus (EVar "x") (EVar "z")) (ELet "z" (ENum 10) (EVar "y")))

    -- Answer: The correct answer is "Error: unbound variable z."
            -- If you evaluated `let x = 5 in (let y = x + z in (let z = 10 in y))` in Haskell, 
            -- you'd get an error message saying that `z` is not in scope. (Try it in GHCi and see.)
            -- How does this happen? We begin with an empty environment. 
            -- To evaluate the outermost let-expression, we extend the empty environment with 
            -- a binding `("x", 5)` and then evaluate the body `let y = x + z in (let z = 10 in y)` in 
            -- that extended environment. To do that, we need to evaluate `x + z`, so we try 
            -- to look up `x` and `z` in the environment, but `z` isn't there, so we get an error.
     -- Kuper explanation:
        -- Step by step:
            -- Evaluate the body ((let y = x + z) in (let z = 10 in y)) in the environment (let x = 5)
            -- Evaliate the body (let z = 10 in y) in the environment [("y", ??? value of x + z ???), ("x", 5)]
        -- Uh oh! We can't extend the environment with a value bound to y,
        -- because we don't know what the value of x + z is supposed to be.
        -- Why don't we know what the value of x + z is supposed to be?
        -- Because we don't know what the value of z is supposed to be at this point