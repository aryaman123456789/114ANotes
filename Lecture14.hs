    data Binop = Plus | Minus | Times
        deriving Show
    data Expr = ELet String Expr Expr
            | ENum Int
            | EVar String
            | EBin Binop Expr Expr
            | ELam String Expr
            | EApp Expr Expr
            | EIf Expr Expr Expr
        deriving Show

    data Value = VInt Int
               | VClos String Expr Env -- A closure is really 3 things:
                                        -- 1. A binder, 2. A function body, 3. An environment
        deriving Show

    -- What a closure does is glue together a function with an environment
    -- What is an "environment"?
        -- It is a way to associate variables with something (ex: types, values, etc)
        -- We will be associating variables with values
    type Env = [(String, Value)]

    exampleEnv :: [(String, Value)]
    exampleEnv = [("x", VNum 3), ("y", VNum 4)]

    -- And our interpreter from lecture 8: (but now with support for let expressions)
    eval :: Expr -> Env -> Value
    eval (ENum n) env = VNum n
    eval (EVar s) env = lookupInEnv env s
    eval (EBin op n m) env = evalOp op (eval n env) (eval m env)
    -- What is the value of a function?
    eval (ELam s expr) env = VClos s expr env
    eval (EIf e1 e2 e3) env = if ifHelper (eval e1 env) then eval e2 env else eval e3 env
        where ifHelper :: Value -> Bool
                ifHelper (VInt n) | n == 0 = True
                        | otherwise = False
    eval (ELet s expr bodyExpr) env = eval bodyExpr extendedEnv
        where extendedEnv = extendEnv s (eval expr extendedEnv) env
    eval (EApp funcExpr argExpr) env = appHelper (eval argExpr env) -- step 2 from below
        where appHelper :: Value -> Value
              appHelper (VClos id closureBody closureEnv) = 
                -- make sure to extend the closure's environment (closureEnv), not the dynamic environment (env)
                eval closureBody (extendEnv id (eval argExpr env) closureEnv)
              appHelper v = error ("Sorry, " ++ show v ++ " is not a function!")

    evalOp :: Binop -> Value -> Value -> Value
    evalOp Plus (VInt n) (VInt m) = VNum (n + m)
    evalOp Minus (VInt n) (VInt m) = VNum (n - m)
    evalOp Times (VInt n) (VInt m) = VNum (n * m)
    evalOp _ _ _ = error "Wrong!"

    -- How to evaluate a function call?
    -- 1. Evaluate the function argument, getting a closure
    -- 2. Evaluate the argument expression, getting some kind of value
    -- 3. Pull out the body of the closure from step 1 and evaluate it in an extended argument
    --    where the formal parameter to the function is bound to the value of the argument from step 2

    -- Why extend the closure's environment (and not the dynamic environment?)
    -- The reason is: the closure's environment already contains all the variable bindings
    -- we need to evaluate the body of the function, EXCEPT for the one being passed in as an argument
    -- (which is what we are extending it with)


    -- the beta-reduction rule, AKA substitution
    -- (\x -> e1) e2 -> e1 [x := e2]

    -- let x = 5 in let f = \y -> y + x in let x = 6 in f x
    -- env = [("x", 5)]
    -- env = [("f", <closure with body "y + x" and env [("x", 5)]>), ("x", 5)]
    -- env = [("x", 6), ("f", <closure with body "y + x" and env [("x", 5)]>), ("x", 5)]
    -- When we evaluate the call 'f x',
    -- we look up f and x in env

    -- What about recursive functions?
    -- Like this:
    -- let f = \n -> if n == 0 then 1 else n * f (n - 1) in f 5

    -- If we just implemented 

    -- QUIZ: What should the following expression evaluate to in Haskell (and in Nano)?
    --let x = 3 in
        --let f = \y -> x + y in
        --let x = 5 in
            --f x
    -- Answer: 8
    -- Explanation: `f` uses the value of `x` that was present in the code in the place `f` was *defined*. 
    --          The value of 3 for `x` gets saved in `f`'s closure. Later, we create a new binding 
    --          for `x` and then call `f` with `x` as the argument. That argument's value, which 
    --          is `5`, gets substituted in for `y` in the body of `f` when the call to `f` is evaluated. 
    --          So, the call to `f`, which is `x + y`, evaluates to 3 + 5, or 8. 
    --          Your Nano interpreter should have the same behavior.

