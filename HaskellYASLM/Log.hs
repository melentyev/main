module Main where

import Control.Monad.Trans.State

type Log = [String]

newtype Logger a = Logger { execLogger :: (a, Log) }

instance Monad Logger where
    return a = Logger (a, [])
    -- (>>=) :: Logger a -> (a -> Logger b) -> Logger b
    m >>= k = 
        let (a, w) = execLogger m
            n      = k a
            (b, x) = execLogger n
        in Logger (b, w ++ x)

record :: String -> Logger ()
record s = Logger ((), [s])
{-
someTest =
    return 1                    >>= 
    \x -> record "Hi, mom"      >>
    return 2                    >>=
    \y -> record "Hi, dad"      >>
    return (x + y)
-}
stateTest = 
    let acts = do
                x <- return 3
                put "a"
                y <- return (x + 5)
                let z = y * y
                s <- get
                put (s ++ "b")
                return $ x + z

    in runState acts ""



main :: IO()
main = print ()