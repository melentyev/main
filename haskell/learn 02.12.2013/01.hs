main = do
    s <- getLine
    putStr s
processIt s = show (length s)