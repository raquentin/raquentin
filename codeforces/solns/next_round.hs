main :: IO ()
main = do
    line1 <- getLine
    line2 <- getLine
    let [n, k] = map read (words line1)
        a = map read (words line2)
 
    let threshold = a !! (k - 1)
 
    print $ length . filter (\x -> x >= threshold && x > 0) $ a
