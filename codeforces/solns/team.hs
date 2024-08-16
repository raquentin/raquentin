import Control.Monad (replicateM)
 
processLine :: String -> Bool
processLine line = sum (map read $ words line) >= 2
 
main :: IO ()
main = do
    n <- readLn :: IO Int
    lines <- replicateM n getLine
    print $ length $ filter processLine lines
