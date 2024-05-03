message :: String
message = "Welcome to Haskell!"

-- In Haskell, functions can be *partially* applied.
-- You can pass only some of a function's arguments to it,
-- and that's still a valid Haskell program!

-- The type of this function says that it returns something
-- that's the same type as its argument.
partiallyAppliedFunction :: p -> p
partiallyAppliedFunction = (\x y -> y) "apple" -- Just the identity function
-- This will evaluate to `\y -> y`

anotherPartiallyAppliedFunction :: p -> String
anotherPartiallyAppliedFunction = (\x y -> x) "apple" -- What's this?
-- This will evaluate to `\y -> "apple"`

-- Let's talk about function types!

-- What do we mean by type of a function? The types of its arguments,
-- and the type of whatever it returns, 
-- put together in an "arrow type" that looks like "a -> b".

-- For example, let's write a function with type Bool -> String.

favoritePokemon :: Bool -> String
favoritePokemon = \b -> if b then "mew" else "gyarados"

-- This won't compile because the branches of the if expression have different types.
-- ifExpressionWithDifferentlyTypedBranches :: Bool -> String
-- ifExpressionWithDifferentlyTypedBranches = \b -> if b then 5 else "orange"

-- Let's do a function that takes more than one argument.
-- In Haskell, functions always take arguments *one at a time*.

favoritePokemonMessage :: Bool -> String -> String
favoritePokemonMessage = \b str -> if b then "mew" ++ str else "gyarados" ++ str

-- Let's apply the above function to only one of its arguments!
partiallyAppliedFavoritePokemonMessage :: String -> String
partiallyAppliedFavoritePokemonMessage = favoritePokemonMessage True

-- How does one usually define a function in Haskell?
-- You can define a function with multiple equations,
-- and then use *pattern matching* to match arguments.
idiomaticFavoritePokemonMessage :: Bool -> String -> String
idiomaticFavoritePokemonMessage True  str            = "mew" ++ str 
idiomaticFavoritePokemonMessage False " is the best" = "No it isn't"
idiomaticFavoritePokemonMessage False _ignored       = "I agree" -- _ignored matches anything

-- Here, the order of equations matters, because `n` matches anything!
ourSum :: Int -> Int
ourSum 0 = 0
ourSum n = n + ourSum (n - 1)






