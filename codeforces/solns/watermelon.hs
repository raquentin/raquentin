main :: IO ()
 
main = do
    input <- getLine
    let number = read input :: Int
    if number >= 4 && even number
        then putStrLn "YES"
        else putStrLn "NO"
